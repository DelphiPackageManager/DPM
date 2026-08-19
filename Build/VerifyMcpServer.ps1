<#
.SYNOPSIS
  Verifies that 'dpm mcp' speaks valid MCP over stdio and never contaminates stdout.

.DESCRIPTION
  The MCP stdio binding is unforgiving: the server MUST NOT write anything to stdout that is
  not a valid MCP message, messages are newline delimited and MUST NOT contain embedded
  newlines, and everything MUST be UTF-8. A single stray banner line, log line, BOM or CRLF
  breaks the session in a way that is very hard to diagnose from the client end.

  Three defects this guards against, all of which were real risks in this codebase:
    1. The banner and every ILogger call used to go to stdout. 'dpm mcp' swaps the console
       writer for a stderr one (Source\Cmdline\Writer\DPM.Console.Writer.StdErr.pas); if that
       wiring regresses, check 4 fails.
    2. The RTL Write converts to the console ANSI code page rather than UTF-8, so a package
       description containing non ASCII would go out as the wrong bytes. Check 7 covers it.
    3. A DEBUG build used to call ReadLn when a debugger was attached, which would consume the
       client's request stream and hang. Check 1 covers it.

  Also checks that a malformed frame does not end the session, since a server that dies on one
  bad line is useless in practice.

.PARAMETER DpmExe
  Path to dpm.exe. Defaults to ..\Output\dpm.exe relative to this script.

.EXAMPLE
  powershell -ExecutionPolicy Bypass -File Build\VerifyMcpServer.ps1
#>
[CmdletBinding()]
param(
  [string] $DpmExe = (Join-Path $PSScriptRoot '..\Output\dpm.exe')
)

$ErrorActionPreference = 'Stop'

if (-not (Test-Path $DpmExe)) {
  Write-Host "FAIL: dpm.exe not found at '$DpmExe'"
  exit 1
}
$DpmExe = (Resolve-Path $DpmExe).Path

$work = Join-Path $env:TEMP 'dpm-mcp-verify'
if (Test-Path $work) { Remove-Item $work -Recurse -Force }
New-Item -ItemType Directory -Path $work | Out-Null

$script:failures = 0
function Check($name, $ok, $detail) {
  if ($ok) {
    Write-Host ("  ok    {0}" -f $name)
  } else {
    Write-Host ("  FAIL  {0}" -f $name)
    if ($detail) { Write-Host ("        {0}" -f $detail) }
    $script:failures++
  }
}

# LF terminated, UTF-8, no BOM - exactly what a real client sends.
function Write-Frames($path, $lines) {
  $utf8NoBom = New-Object System.Text.UTF8Encoding($false)
  [System.IO.File]::WriteAllText($path, (($lines -join "`n") + "`n"), $utf8NoBom)
}

# Run through cmd, not PowerShell redirection: PS 5.1's '>' writes UTF-16 with a BOM, which
# would invalidate every byte level assertion below.
function Invoke-Mcp($framesPath, $outPath, $errPath) {
  cmd /c "type `"$framesPath`" | `"$DpmExe`" mcp > `"$outPath`" 2> `"$errPath`""
  return $LASTEXITCODE
}

Write-Host "Verifying MCP stdio behaviour of '$DpmExe'"
Write-Host ''

$meta = '"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}'

# ---------------------------------------------------------------- session 1: legacy era
$frames = Join-Path $work 'legacy.txt'
$out    = Join-Path $work 'legacy.out'
$err    = Join-Path $work 'legacy.err'
Write-Frames $frames @(
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"verify","version":"1.0"}}}'
  '{"jsonrpc":"2.0","method":"notifications/initialized"}'
  '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
)
$code = Invoke-Mcp $frames $out $err
$bytes = [System.IO.File]::ReadAllBytes($out)
$lines = @(Get-Content $out)

Check 'legacy: exits 0 on stdin EOF' ($code -eq 0) "exit code was $code"
Check 'legacy: stdout has no BOM' (-not ($bytes.Length -ge 3 -and $bytes[0] -eq 0xEF -and $bytes[1] -eq 0xBB -and $bytes[2] -eq 0xBF)) ''
Check 'legacy: stdout contains no CR byte' (-not ($bytes -contains 13)) 'framing must be LF only'
# 3 messages in, but one is a notification, which must never be answered.
Check 'legacy: one reply per request, none for the notification' ($lines.Count -eq 2) "got $($lines.Count) lines, expected 2"

$allJson = $true; $parsed = @()
foreach ($l in $lines) { try { $parsed += ($l | ConvertFrom-Json) } catch { $allJson = $false } }
Check 'legacy: every stdout line is valid JSON' $allJson ''
if ($allJson -and $parsed.Count -ge 1) {
  Check 'legacy: initialize echoes a supported protocol version' ($parsed[0].result.protocolVersion -eq '2025-06-18') "got '$($parsed[0].result.protocolVersion)'"
  Check 'legacy: serverInfo identifies dpm' ($parsed[0].result.serverInfo.name -eq 'dpm') ''
}
$outText = Get-Content $out -Raw
Check 'legacy: no banner on stdout' (-not ($outText -match 'Delphi Package Manager')) ''
Check 'legacy: no I/O error on stdout' (-not ($outText -match 'I/O error')) ''

# ---------------------------------------------------------------- session 2: modern era
$frames = Join-Path $work 'modern.txt'
$out    = Join-Path $work 'modern.out'
$err    = Join-Path $work 'modern.err'
Write-Frames $frames @(
  "{`"jsonrpc`":`"2.0`",`"id`":1,`"method`":`"server/discover`",`"params`":{$meta}}"
  "{`"jsonrpc`":`"2.0`",`"id`":2,`"method`":`"tools/list`",`"params`":{$meta}}"
  "{`"jsonrpc`":`"2.0`",`"id`":3,`"method`":`"server/discover`",`"params`":{`"_meta`":{`"io.modelcontextprotocol/protocolVersion`":`"1900-01-01`"}}}"
)
$code = Invoke-Mcp $frames $out $err
$lines = @(Get-Content $out)
$parsed = @(); foreach ($l in $lines) { $parsed += ($l | ConvertFrom-Json) }

Check 'modern: exits 0' ($code -eq 0) "exit code was $code"
Check 'modern: three replies' ($lines.Count -eq 3) "got $($lines.Count)"
if ($parsed.Count -ge 3) {
  Check 'modern: server/discover advertises supported versions' ($parsed[0].result.supportedVersions.Count -ge 1) ''
  Check 'modern: every result carries resultType' (($parsed[0].result.resultType -eq 'complete') -and ($parsed[1].result.resultType -eq 'complete')) ''
  # Required from 2026-07-28 on every cacheable result. Missing them makes a schema
  # validating client reject the whole tools/list response - the server looks connected
  # but no tools ever load.
  $d = $parsed[0].result; $t = $parsed[1].result
  Check 'modern: server/discover carries ttlMs' ($d.ttlMs -is [int64] -or $d.ttlMs -is [int]) "got '$($d.ttlMs)'"
  Check 'modern: server/discover carries a valid cacheScope' ($d.cacheScope -in @('public','private')) "got '$($d.cacheScope)'"
  Check 'modern: tools/list carries ttlMs' ($t.ttlMs -is [int64] -or $t.ttlMs -is [int]) "got '$($t.ttlMs)'"
  Check 'modern: tools/list carries a valid cacheScope' ($t.cacheScope -in @('public','private')) "got '$($t.cacheScope)'"
  Check 'modern: ttlMs is not negative' (($d.ttlMs -ge 0) -and ($t.ttlMs -ge 0)) ''
  Check 'modern: tools/list returns the five read-only tools' ($parsed[1].result.tools.Count -eq 5) "got $($parsed[1].result.tools.Count)"
  $readOnly = ($parsed[1].result.tools | Where-Object { $_.annotations.readOnlyHint -ne $true }).Count -eq 0
  Check 'modern: every tool declares readOnlyHint' $readOnly ''
  # An unsupported version must be refused with the list the client can retry from.
  Check 'modern: unknown version rejected with -32022' ($parsed[2].error.code -eq -32022) "got $($parsed[2].error.code)"
  Check 'modern: -32022 names the versions we do support' ($parsed[2].error.data.supported.Count -ge 1) ''
}

# ---------------------------------------------------------------- session 3: robustness
$frames = Join-Path $work 'bad.txt'
$out    = Join-Path $work 'bad.out'
$err    = Join-Path $work 'bad.err'
Write-Frames $frames @(
  "{`"jsonrpc`":`"2.0`",`"id`":1,`"method`":`"ping`",`"params`":{$meta}}"
  'this line is not json at all'
  '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"no_such_tool","arguments":{}}}'
  "{`"jsonrpc`":`"2.0`",`"id`":4,`"method`":`"ping`",`"params`":{$meta}}"
)
$code = Invoke-Mcp $frames $out $err
$lines = @(Get-Content $out)
$parsed = @(); foreach ($l in $lines) { $parsed += ($l | ConvertFrom-Json) }

Check 'robust: exits 0 despite a malformed frame' ($code -eq 0) "exit code was $code"
Check 'robust: four replies' ($lines.Count -eq 4) "got $($lines.Count)"
if ($parsed.Count -ge 4) {
  Check 'robust: malformed frame answered with -32700' ($parsed[1].error.code -eq -32700) "got $($parsed[1].error.code)"
  Check 'robust: unknown tool answered with -32602' ($parsed[2].error.code -eq -32602) "got $($parsed[2].error.code)"
  # The point of the whole session: the server kept going.
  Check 'robust: session survives and answers the next request' ($parsed[3].id -eq 4) "got id $($parsed[3].id)"
}

# ---------------------------------------------------------------- session 4: UTF-8 fidelity
# A tool error echoes the argument back, which gives a deterministic way to push non ASCII
# through the whole encode path without depending on what a live feed happens to return.
$frames = Join-Path $work 'utf8.txt'
$out    = Join-Path $work 'utf8.out'
$err    = Join-Path $work 'utf8.err'
Write-Frames $frames @(
  "{`"jsonrpc`":`"2.0`",`"id`":1,`"method`":`"tools/call`",`"params`":{`"name`":`"dpm_package_versions`",`"arguments`":{`"packageId`":`"x`",`"compiler`":`"café-€`"},$meta}}"
)
$code = Invoke-Mcp $frames $out $err
$bytes = [System.IO.File]::ReadAllBytes($out)
$text  = [System.Text.Encoding]::UTF8.GetString($bytes)

Check 'utf8: reply is a tool error, not a protocol error' ($text -match '"isError":true') ''
# e2 82 ac is the euro sign in UTF-8. If stdout were written through the RTL text file it
# would come out in the console ANSI code page instead, and this would fail.
$euro = $false
for ($i = 0; $i -lt $bytes.Length - 2; $i++) {
  if ($bytes[$i] -eq 0xE2 -and $bytes[$i+1] -eq 0x82 -and $bytes[$i+2] -eq 0xAC) { $euro = $true; break }
}
Check 'utf8: non-ASCII survives as UTF-8 bytes' $euro 'euro sign was not found as e2 82 ac'
Check 'utf8: still no CR in the frame' (-not ($bytes -contains 13)) ''

# ---------------------------------------------------------------- session 5: compiler precedence
# A --compiler baked into the client registration must NOT win over the project being asked
# about. Otherwise, the moment you open a project targeting a different Delphi, every package
# query silently answers for the wrong compiler - which is the whole reason the flag is a
# fallback rather than an override.
$proj = Join-Path $PSScriptRoot '..\Source\dpm.dproj'
if (Test-Path $proj) {
  $proj = (Resolve-Path $proj).Path
  $projJson = $proj.Replace('\', '\\')
  $frames = Join-Path $work 'precedence.txt'
  $out    = Join-Path $work 'precedence.out'
  $err    = Join-Path $work 'precedence.err'
  Write-Frames $frames @(
    "{`"jsonrpc`":`"2.0`",`"id`":1,`"method`":`"tools/call`",`"params`":{`"name`":`"dpm_project_info`",`"arguments`":{`"projectPath`":`"$projJson`"},$meta}}"
    "{`"jsonrpc`":`"2.0`",`"id`":2,`"method`":`"tools/call`",`"params`":{`"name`":`"dpm_package_versions`",`"arguments`":{`"packageId`":`"Spring4D.Core`",`"projectPath`":`"$projJson`"},$meta}}"
    "{`"jsonrpc`":`"2.0`",`"id`":3,`"method`":`"tools/call`",`"params`":{`"name`":`"dpm_package_versions`",`"arguments`":{`"packageId`":`"Spring4D.Core`",`"projectPath`":`"$projJson`",`"compiler`":`"11.0`"},$meta}}"
  )
  # Deliberately start with a fallback compiler that does NOT match the project.
  cmd /c "type `"$frames`" | `"$DpmExe`" mcp --compiler=10.4 > `"$out`" 2> `"$err`""
  $lines = @(Get-Content $out)
  $parsed = @(); foreach ($l in $lines) { $parsed += ($l | ConvertFrom-Json) }

  if ($parsed.Count -ge 3) {
    $projectCompiler = ($parsed[0].result.content[0].text | ConvertFrom-Json).compiler
    $inferred        = ($parsed[1].result.content[0].text | ConvertFrom-Json).compiler
    $overridden      = ($parsed[2].result.content[0].text | ConvertFrom-Json).compiler
    Check 'precedence: projectPath beats the --compiler fallback' ($inferred -eq $projectCompiler) "project is $projectCompiler but the tool answered for $inferred"
    Check 'precedence: the stale fallback did not leak in' ($inferred -ne 'delphi10.4') "got $inferred"
    Check 'precedence: an explicit compiler argument still wins' ($overridden -eq 'delphi11.0') "got $overridden"
  } else {
    Check 'precedence: three replies' $false "got $($lines.Count)"
  }
} else {
  Write-Host "  skip  precedence: '$proj' not found"
}

Write-Host ''
if ($script:failures -eq 0) {
  Write-Host 'RESULT: all checks passed'
  exit 0
} else {
  Write-Host ("RESULT: {0} check(s) failed" -f $script:failures)
  exit 1
}
