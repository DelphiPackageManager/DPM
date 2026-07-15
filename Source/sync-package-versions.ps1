<#
.SYNOPSIS
    Syncs DPM package versions from a source DPM.IDE.XXX.dproj to all the others.

.DESCRIPTION
    Package references in the dproj files are pinned by manifestHash, and the hash
    differs per compiler for the same package/version - so you cannot just copy the
    <PackageReference> XML between projects. This script reads the top-level packages
    (id + version) from a source project and runs `dpm install` for each of them in
    every other DPM.IDE.XXX.dproj, letting dpm download the package for that compiler
    and write the correct per-compiler manifestHash automatically.

    Notes:
      - Only top-level packages are installed; their dependencies are resolved by dpm.
      - Packages present in a target but not in the source are left untouched (this
        script only pushes the source's set; it does not remove extras).
      - Companion to the existing installpkg.bat / restore.bat.

.PARAMETER SourceProject
    The reference project whose package versions are propagated. Default DPM.IDE.D130.dproj.

.PARAMETER DpmExe
    Path to the built dpm.exe. Default ..\Output\dpm.exe (relative to this script).

.PARAMETER Only
    Optional list of target project file names to restrict to (for testing a single project).

.PARAMETER WhatIf
    Print the dpm install commands without executing them.

.EXAMPLE
    pwsh Source\sync-package-versions.ps1 -WhatIf

.EXAMPLE
    pwsh Source\sync-package-versions.ps1 -SourceProject DPM.IDE.D130.dproj
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

# Map each DPM.IDE.XXX.dproj to the compiler token dpm expects (mirrors installpkg.bat / restore.bat).
$compilerMap = [ordered]@{
    'DPM.IDE.XE2.dproj'  = 'XE2'
    'DPM.IDE.XE3.dproj'  = 'XE3'
    'DPM.IDE.XE4.dproj'  = 'XE4'
    'DPM.IDE.XE5.dproj'  = 'XE5'
    'DPM.IDE.XE6.dproj'  = 'XE6'
    'DPM.IDE.XE7.dproj'  = 'XE7'
    'DPM.IDE.XE8.dproj'  = 'XE8'
    'DPM.IDE.D100.dproj' = '10.0'
    'DPM.IDE.D101.dproj' = '10.1'
    'DPM.IDE.D102.dproj' = '10.2'
    'DPM.IDE.D103.dproj' = '10.3'
    'DPM.IDE.D104.dproj' = '10.4'
    'DPM.IDE.D110.dproj' = '11'
    'DPM.IDE.D120.dproj' = '12'
    'DPM.IDE.D130.dproj' = '13'
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

# --- Read the top-level packages from the source project ------------------------

Write-Host "Source project : $sourceName" -ForegroundColor Cyan
Write-Host "dpm            : $dpmPath"     -ForegroundColor Cyan

[xml] $sourceXml = Get-Content -LiteralPath $sourcePath -Raw

# Direct children of ProjectExtensions/DPM/Packages are the installed (top-level) packages;
# nested PackageReference nodes are resolved dependencies (parent is a PackageReference, not
# Packages) and are excluded by this XPath. local-name() keeps it namespace-agnostic.
$xpath = "//*[local-name()='ProjectExtensions']/*[local-name()='DPM']/*[local-name()='Packages']/*[local-name()='PackageReference']"
$nodes = $sourceXml.SelectNodes($xpath)

$packages = @()
foreach ($node in $nodes) {
    $id  = $node.GetAttribute('id')
    $ver = $node.GetAttribute('version')
    if ([string]::IsNullOrWhiteSpace($id) -or [string]::IsNullOrWhiteSpace($ver)) {
        continue
    }
    $packages += [pscustomobject]@{ Id = $id; Version = $ver }
}

if ($packages.Count -eq 0) {
    throw "No top-level packages found in $sourceName (ProjectExtensions/DPM/Packages was empty or missing)."
}

Write-Host ""
Write-Host "Found $($packages.Count) top-level package(s) in $sourceName :" -ForegroundColor Green
foreach ($p in $packages) {
    Write-Host ("  {0}  {1}" -f $p.Id, $p.Version)
}

# --- Determine target projects --------------------------------------------------

$targets = Get-ChildItem -LiteralPath $scriptDir -Filter 'DPM.IDE.*.dproj' |
    Where-Object { $_.Name -ne $sourceName -and $_.Name -ne 'DPM.IDE.Template.dproj' } |
    Select-Object -ExpandProperty Name

if ($Only.Count -gt 0) {
    $targets = $targets | Where-Object { $Only -contains $_ }
    if (-not $targets -or @($targets).Count -eq 0) {
        throw "None of the -Only projects matched: $($Only -join ', ')"
    }
}

# --- Install loop ---------------------------------------------------------------

$results = @()   # one row per target: Project / Compiler / Ok / Failed[]

foreach ($target in $targets) {
    if (-not $compilerMap.Contains($target)) {
        Write-Warning "No compiler token mapped for $target - skipping."
        continue
    }
    $compiler   = $compilerMap[$target]
    $targetPath = Join-Path $scriptDir $target

    Write-Host ""
    Write-Host ("=== {0}  (compiler {1}) ===" -f $target, $compiler) -ForegroundColor Yellow

    $okCount = 0
    $failed  = @()

    foreach ($p in $packages) {
        $dpmArgs = @('install', $p.Id, $targetPath, "-version=$($p.Version)", "-compiler=$compiler", '-preRelease')

        if ($WhatIf) {
            Write-Host ("  [whatif] `"{0}`" {1}" -f $dpmPath, ($dpmArgs -join ' '))
            $okCount++
            continue
        }

        Write-Host ("  install {0} {1} ..." -f $p.Id, $p.Version)
        & $dpmPath @dpmArgs
        if ($LASTEXITCODE -ne 0) {
            Write-Host ("    FAILED (exit {0})" -f $LASTEXITCODE) -ForegroundColor Red
            $failed += ("{0}@{1}" -f $p.Id, $p.Version)
        }
        else {
            $okCount++
        }
    }

    $results += [pscustomobject]@{
        Project  = $target
        Compiler = $compiler
        Ok       = $okCount
        Failed   = $failed
    }
}

# --- Summary --------------------------------------------------------------------

Write-Host ""
Write-Host "===================== Summary =====================" -ForegroundColor Cyan
$anyFailed = $false
foreach ($r in $results) {
    $failCount = @($r.Failed).Count
    if ($failCount -gt 0) { $anyFailed = $true }
    $colour = if ($failCount -gt 0) { 'Red' } else { 'Green' }
    Write-Host ("{0,-22} compiler {1,-5}  ok:{2,-3} failed:{3}" -f $r.Project, $r.Compiler, $r.Ok, $failCount) -ForegroundColor $colour
    foreach ($f in $r.Failed) {
        Write-Host ("    - {0}" -f $f) -ForegroundColor Red
    }
}
Write-Host "===================================================" -ForegroundColor Cyan

if ($WhatIf) {
    Write-Host "(whatif run - nothing was installed)" -ForegroundColor DarkGray
    exit 0
}

if ($anyFailed) {
    Write-Host "One or more installs failed - review the summary above." -ForegroundColor Red
    exit 1
}

Write-Host "All packages installed successfully." -ForegroundColor Green
exit 0
