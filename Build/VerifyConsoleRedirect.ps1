<#
.SYNOPSIS
  Verifies dpm.exe behaves the same whether or not stdout is a real console.

.DESCRIPTION
  Regression guard for the "I/O error 6" crash. Every command that printed via
  TConsoleBase.WriteLine - help, exitcodes, why, and the invalid-argument paths -
  used to die with exit code 9999 whenever stdout was a pipe or a file. That is
  CI, scripts, an editor running dpm, and FinalBuilder's Execute Program action.

  Two defects combined:
    1. TConsoleBase.WriteLine called the System.WriteLn intrinsic instead of its
       own redirect-safe output path (Source\Cmdline\Writer\DPM.Console.Writer.pas).
    2. System.Console's class constructor left the RTL global InOutRes set to 6 at
       startup when stdout was not a console, so the next RTL text I/O statement
       anywhere in the process raised EInOutError (Source\dpm.dpr clears it).

  Checks both directions: redirected output must succeed AND a real console must
  keep working, so a fix that only helps one case cannot pass.

.PARAMETER DpmExe
  Path to dpm.exe. Defaults to ..\Output\dpm.exe relative to this script.

.EXAMPLE
  powershell -ExecutionPolicy Bypass -File Build\VerifyConsoleRedirect.ps1
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

$work = Join-Path $env:TEMP 'dpm-console-verify'
New-Item -ItemType Directory -Force -Path $work | Out-Null
$outFile = Join-Path $work 'out.txt'

$failures = 0

# Arguments, the exit code expected, and a string that must appear in the output.
# ExpectText $null skips the content check.
$cases = @(
  @{ Args = 'help';         Code = 0;   Text = 'Usage :' }
  @{ Args = 'help install'; Code = 0;   Text = 'dpm install' }
  @{ Args = '';             Code = 0;   Text = 'Usage :' }
  @{ Args = 'exitcodes';    Code = 0;   Text = 'Exit Codes' }
  # unknown option / command: still an error, but a reported one rather than a crash
  @{ Args = '--version';    Code = 101; Text = 'Unknown command line option' }
  @{ Args = 'boguscommand'; Code = 101; Text = 'Unknown option' }
  # control - this one always worked redirected, it must keep working
  @{ Args = 'sources list'; Code = 0;   Text = $null }
)

Write-Host "Verifying console redirection behaviour of '$DpmExe'"
Write-Host ''

foreach ($case in $cases) {
  $label = if ($case.Args -eq '') { '(no args)' } else { $case.Args }

  # stdin from nul so a stray prompt can never hang a build
  cmd /c "`"$DpmExe`" $($case.Args) > `"$outFile`" 2>&1 < nul"
  $code = $LASTEXITCODE
  $text = if (Test-Path $outFile) { Get-Content $outFile -Raw } else { '' }
  if ($null -eq $text) { $text = '' }

  $problems = @()
  if ($code -ne $case.Code) { $problems += "exit $code, expected $($case.Code)" }
  if ($text -match 'I/O error') { $problems += 'I/O error in output' }
  if ($case.Text -and ($text -notlike "*$($case.Text)*")) { $problems += "missing text '$($case.Text)'" }

  if ($problems.Count -gt 0) {
    Write-Host ("  FAIL  redirected: dpm {0}  --  {1}" -f $label, ($problems -join '; '))
    Write-Host '        ---- output ----'
    Write-Host $text
    Write-Host '        ----------------'
    $failures++
  }
  else {
    Write-Host ("  ok    redirected: dpm {0}" -f $label)
  }
}

# 'dpm verify -json-output' promises a single JSON object on stdout, so redirected
# stdout must parse as JSON with nothing else in it - no banner, no chat. Needs a
# .dpkg to verify; skipped (not failed) when the package cache has none.
$dpkg = Get-ChildItem -Path (Join-Path $env:APPDATA '.dpm\package_cache') -Filter *.dpkg -Recurse -ErrorAction SilentlyContinue |
        Select-Object -First 1
if ($null -eq $dpkg) {
  Write-Host '  skip  json-output: no .dpkg in the package cache to verify'
}
else {
  $jsonFile = Join-Path $work 'verify.json'
  cmd /c "`"$DpmExe`" verify `"$($dpkg.FullName)`" -json-output -offline > `"$jsonFile`" 2>nul < nul" | Out-Null
  $raw = Get-Content $jsonFile -Raw
  if ($null -eq $raw) { $raw = '' }
  try {
    $null = $raw | ConvertFrom-Json
    Write-Host '  ok    json-output: stdout is a single JSON object'
  }
  catch {
    Write-Host '  FAIL  json-output: stdout did not parse as JSON (banner or other output leaked in)'
    Write-Host '        ---- first 200 chars ----'
    Write-Host $raw.Substring(0, [Math]::Min(200, $raw.Length))
    Write-Host '        -------------------------'
    $failures++
  }
}

# A genuinely new console window, so output is NOT redirected and the
# WriteConsoleW branch runs. Only the exit code is observable.
foreach ($args in @('help', 'exitcodes')) {
  $p = Start-Process -FilePath $DpmExe -ArgumentList $args -Wait -PassThru -WindowStyle Minimized
  if ($p.ExitCode -ne 0) {
    Write-Host ("  FAIL  real console: dpm {0}  --  exit {1}, expected 0" -f $args, $p.ExitCode)
    $failures++
  }
  else {
    Write-Host ("  ok    real console: dpm {0}" -f $args)
  }
}

Write-Host ''
if ($failures -gt 0) {
  Write-Host "RESULT: $failures check(s) FAILED"
  exit 1
}
Write-Host 'RESULT: all checks passed'
exit 0
