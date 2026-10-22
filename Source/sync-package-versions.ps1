<#
.SYNOPSIS
    Brings every DPM.IDE.XXX.dproj to the package versions of a reference project (DPM.IDE.D130.dproj).

.DESCRIPTION
    Package references in the dproj files are pinned by manifestHash, and the hash differs per
    compiler for the same package/version - so you cannot just copy the <PackageReference> XML
    between projects. Instead this script compares each project's top-level packages (id + version)
    with the reference project and has dpm install only the ones that differ, so dpm downloads and
    compiles the package for that project's compiler and writes the correct manifestHash and
    DPMSearch.

      - Packages at a different version are installed with ONE `dpm install a@x,b@y --upgrade` per
        project, so packages pinned to each other (Spring4D.Core / Extensions / Base) move together,
        up or down.
      - Packages the project does not have yet are installed with a plain `dpm install` (--upgrade
        only touches packages a project already references).
      - Packages a project has that the reference does not are reported, not removed.
      - dpm install rewrites <DPMExe> to the dpm.exe that ran it - the original value is put back.
      - Finally every project's whole package graph (transitives included) and DPMSearch are compared
        with the reference project's.

    Exits 1 if an install failed or a project still differs from the reference.

.PARAMETER SourceProject
    The reference project whose package versions are propagated. Default DPM.IDE.D130.dproj.

.PARAMETER DpmExe
    Path to the built dpm.exe. Default ..\Output\dpm.exe (relative to this script). It must support
    package lists (id@version,id@version) and --upgrade.

.PARAMETER Only
    Optional list of target project file names to restrict to (for testing a single project).

.PARAMETER WhatIf
    Report the differences and the dpm commands that would run, without running them.

.EXAMPLE
    pwsh Source\sync-package-versions.ps1 -WhatIf

.EXAMPLE
    pwsh Source\sync-package-versions.ps1 -Only DPM.IDE.XE2.dproj
#>
param(
    [string]   $SourceProject = 'DPM.IDE.D130.dproj',
    [string]   $DpmExe        = '..\Output\dpm.exe',
    [string[]] $Only          = @(),
    [switch]   $WhatIf
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$scriptDir = $PSScriptRoot

# Direct children of ProjectExtensions/DPM/Packages are the installed (top-level) packages; nested
# PackageReference nodes are resolved dependencies. local-name() keeps it namespace-agnostic.
$packagesXPath = "//*[local-name()='ProjectExtensions']/*[local-name()='DPM']/*[local-name()='Packages']"
$topLevelXPath = $packagesXPath + "/*[local-name()='PackageReference']"
$allRefsXPath  = $packagesXPath + "//*[local-name()='PackageReference']"

# --- Helpers --------------------------------------------------------------------

function Read-ProjectXml([string] $path) {
    # XmlDocument.Load honours the file's encoding (Get-Content would not on Windows PowerShell).
    $xml = New-Object System.Xml.XmlDocument
    $xml.Load($path)
    return $xml
}

# lowercase id -> { Id; Version } for the project's top-level packages.
function Get-TopLevelPackages([string] $path) {
    $xml = Read-ProjectXml $path
    $result = [ordered]@{}
    foreach ($node in $xml.SelectNodes($topLevelXPath)) {
        $id  = $node.GetAttribute('id')
        $ver = $node.GetAttribute('version')
        if ([string]::IsNullOrWhiteSpace($id) -or [string]::IsNullOrWhiteSpace($ver)) {
            continue
        }
        $result[$id.ToLowerInvariant()] = [pscustomobject]@{ Id = $id; Version = $ver }
    }
    return $result
}

# Every edge of the package graph ("parent > id version range") plus the packages on the Win32
# DPMSearch path ("search id version") - what has to match the reference project once synced.
function Get-PackageGraph([string] $path) {
    $xml = Read-ProjectXml $path
    $entries = New-Object System.Collections.Generic.List[string]
    foreach ($node in $xml.SelectNodes($allRefsXPath)) {
        $parent = ''
        if ($node.ParentNode.LocalName -eq 'PackageReference') {
            $parent = $node.ParentNode.GetAttribute('id')
        }
        $entries.Add(('{0} > {1} {2} {3}' -f $parent, $node.GetAttribute('id'), $node.GetAttribute('version'), $node.GetAttribute('range')).Trim())
    }
    foreach ($node in $xml.SelectNodes("//*[local-name()='DPMSearch']")) {
        if ($node.GetAttribute('Condition') -notmatch 'Win32') {
            continue
        }
        foreach ($m in [regex]::Matches($node.InnerText, '\$\(DPM\)\\([^\\]+)\\([^\\]+)\\')) {
            $entries.Add(('search {0} {1}' -f $m.Groups[1].Value, $m.Groups[2].Value))
        }
    }
    return ,@($entries | Sort-Object -Unique)
}

function Get-DpmExeValue([string] $path) {
    $m = [regex]::Match([System.IO.File]::ReadAllText($path), '<DPMExe>([^<]*)</DPMExe>')
    if ($m.Success) {
        return $m.Groups[1].Value
    }
    return $null
}

# Puts <DPMExe> back without touching anything else in the file (BOM, line endings, formatting).
function Set-DpmExeValue([string] $path, [string] $value) {
    $bytes  = [System.IO.File]::ReadAllBytes($path)
    $hasBom = ($bytes.Length -ge 3) -and ($bytes[0] -eq 0xEF) -and ($bytes[1] -eq 0xBB) -and ($bytes[2] -eq 0xBF)
    $offset = if ($hasBom) { 3 } else { 0 }
    $utf8   = New-Object System.Text.UTF8Encoding($false)
    $text   = $utf8.GetString($bytes, $offset, $bytes.Length - $offset)
    $text   = [regex]::Replace($text, '<DPMExe>[^<]*</DPMExe>', { param($m) '<DPMExe>' + $value + '</DPMExe>' })
    $out    = $utf8.GetBytes($text)
    if ($hasBom) {
        $out = [byte[]]((0xEF, 0xBB, 0xBF) + $out)
    }
    [System.IO.File]::WriteAllBytes($path, $out)
}

function Format-PackageList($packages) {
    return (($packages | ForEach-Object { '{0}@{1}' -f $_.Id, $_.Version }) -join ',')
}

# Runs dpm (or just prints the command for -WhatIf). Returns true when it succeeded.
function Invoke-Dpm([string[]] $dpmArgs) {
    Write-Host ("  dpm {0}" -f ($dpmArgs -join ' ')) -ForegroundColor DarkGray
    if ($WhatIf) {
        return $true
    }
    & $dpmPath @dpmArgs | Out-Host
    if ($LASTEXITCODE -ne 0) {
        Write-Host ("    FAILED (exit {0})" -f $LASTEXITCODE) -ForegroundColor Red
        return $false
    }
    return $true
}

# --- Resolve and validate inputs ------------------------------------------------

$sourcePath = if ([System.IO.Path]::IsPathRooted($SourceProject)) { $SourceProject }
              else { Join-Path $scriptDir $SourceProject }
$sourcePath = [System.IO.Path]::GetFullPath($sourcePath)
if (-not (Test-Path -LiteralPath $sourcePath)) {
    throw "Source project not found: $sourcePath"
}
$sourceName = Split-Path -Leaf $sourcePath

$dpmPath = if ([System.IO.Path]::IsPathRooted($DpmExe)) { $DpmExe }
           else { Join-Path $scriptDir $DpmExe }
$dpmPath = [System.IO.Path]::GetFullPath($dpmPath)
if (-not (Test-Path -LiteralPath $dpmPath)) {
    throw "dpm.exe not found: $dpmPath (build it first, or pass -DpmExe)"
}

Write-Host "Source project : $sourceName" -ForegroundColor Cyan
Write-Host "dpm            : $dpmPath"     -ForegroundColor Cyan

$reference = Get-TopLevelPackages $sourcePath
if ($reference.Count -eq 0) {
    throw "No top-level packages found in $sourceName (ProjectExtensions/DPM/Packages was empty or missing)."
}

Write-Host ""
Write-Host "Found $($reference.Count) top-level package(s) in $sourceName :" -ForegroundColor Green
foreach ($p in $reference.Values) {
    Write-Host ("  {0}  {1}" -f $p.Id, $p.Version)
}

# --- Determine target projects --------------------------------------------------

$targets = @(Get-ChildItem -LiteralPath $scriptDir -Filter 'DPM.IDE.*.dproj' |
    Where-Object { $_.Name -ne $sourceName -and $_.Name -ne 'DPM.IDE.Template.dproj' } |
    Select-Object -ExpandProperty Name)

if ($Only.Count -gt 0) {
    # pwsh -File passes "a,b" as one string rather than an array.
    $Only = @($Only | ForEach-Object { $_ -split ',' } | ForEach-Object { $_.Trim() } | Where-Object { $_ -ne '' })
    $targets = @($targets | Where-Object { $Only -contains $_ })
    if ($targets.Count -eq 0) {
        throw "None of the -Only projects matched: $($Only -join ', ')"
    }
}

# --- Install what differs -------------------------------------------------------

$failed = @()

foreach ($target in $targets) {
    $targetPath = Join-Path $scriptDir $target
    $current = Get-TopLevelPackages $targetPath

    $changed = @()
    $missing = @()
    foreach ($key in $reference.Keys) {
        if (-not $current.Contains($key)) {
            $missing += $reference[$key]
        }
        elseif ($current[$key].Version -ne $reference[$key].Version) {
            $changed += $reference[$key]
        }
    }
    $extra = @($current.Keys | Where-Object { -not $reference.Contains($_) } | ForEach-Object { $current[$_] })

    Write-Host ""
    if (($changed.Count -eq 0) -and ($missing.Count -eq 0)) {
        Write-Host ("=== {0} : up to date ===" -f $target) -ForegroundColor Green
    }
    else {
        Write-Host ("=== {0} ===" -f $target) -ForegroundColor Yellow
    }
    foreach ($p in $changed) {
        Write-Host ("  {0} {1} -> {2}" -f $p.Id, $current[$p.Id.ToLowerInvariant()].Version, $p.Version)
    }
    foreach ($p in $missing) {
        Write-Host ("  {0} {1} (not installed)" -f $p.Id, $p.Version)
    }
    foreach ($p in $extra) {
        Write-Warning ("{0} has {1} {2}, which {3} does not - left as is." -f $target, $p.Id, $p.Version, $sourceName)
    }
    if (($changed.Count -eq 0) -and ($missing.Count -eq 0)) {
        continue
    }

    $dpmExeValue = Get-DpmExeValue $targetPath
    $ok = $true
    if ($changed.Count -gt 0) {
        $ok = Invoke-Dpm @('install', (Format-PackageList $changed), $targetPath, '--upgrade', '--preRelease')
    }
    if ($ok -and ($missing.Count -gt 0)) {
        $ok = Invoke-Dpm @('install', (Format-PackageList $missing), $targetPath, '--preRelease')
    }
    if (-not $ok) {
        $failed += $target
    }

    if ((-not $WhatIf) -and ($null -ne $dpmExeValue) -and ((Get-DpmExeValue $targetPath) -ne $dpmExeValue)) {
        Set-DpmExeValue $targetPath $dpmExeValue
    }
}

# --- Verify ---------------------------------------------------------------------

Write-Host ""
Write-Host "===================== Summary =====================" -ForegroundColor Cyan
$referenceGraph = Get-PackageGraph $sourcePath
$differing = @()
foreach ($target in $targets) {
    if ($failed -contains $target) {
        Write-Host ("{0,-22} install failed" -f $target) -ForegroundColor Red
        continue
    }
    $graph = Get-PackageGraph (Join-Path $scriptDir $target)
    $diff = @(Compare-Object -ReferenceObject $referenceGraph -DifferenceObject $graph)
    if ($diff.Count -eq 0) {
        Write-Host ("{0,-22} matches {1}" -f $target, $sourceName) -ForegroundColor Green
        continue
    }
    $differing += $target
    Write-Host ("{0,-22} differs from {1}" -f $target, $sourceName) -ForegroundColor Red
    foreach ($d in $diff) {
        $label = if ($d.SideIndicator -eq '<=') { 'missing' } else { 'extra  ' }
        Write-Host ("    {0} : {1}" -f $label, $d.InputObject) -ForegroundColor Red
    }
}
Write-Host "===================================================" -ForegroundColor Cyan

if ($WhatIf) {
    Write-Host "(whatif run - nothing was installed)" -ForegroundColor DarkGray
    exit 0
}

if (($failed.Count -gt 0) -or ($differing.Count -gt 0)) {
    Write-Host "Not every project matches $sourceName - review the summary above." -ForegroundColor Red
    exit 1
}

Write-Host "All projects match $sourceName." -ForegroundColor Green
exit 0
