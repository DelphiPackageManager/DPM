{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Console.Trust.Prompt;

// CLI implementation of ITrustPromptStrategy. When stdin is a TTY, asks
// the user for [o]verride/[c]ancel/[a]lways-block. When stdin is redirected
// (CI), degrades to a hard block — the user can override at config level
// by setting authorDowngradePolicy=allow.

interface

uses
  DPM.Core.Logging,
  DPM.Core.Trust.Prompt;

type
  TConsoleTrustPromptStrategy = class(TInterfacedObject, ITrustPromptStrategy)
  private
    FLogger : ILogger;
    function StdinIsTty : boolean;
    function AskUser(out decision : TTrustPromptDecision) : boolean;
  protected
    function PromptAuthorDowngrade(const context : TTrustPromptContext;
                                   out decision : TTrustPromptDecision) : boolean;
    function PromptRepositoryRatchet(const context : TRepoTrustPromptContext;
                                     out decision : TTrustPromptDecision) : boolean;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils;

constructor TConsoleTrustPromptStrategy.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

function TConsoleTrustPromptStrategy.StdinIsTty : boolean;
var
  handle : THandle;
  mode : DWORD;
begin
  handle := GetStdHandle(STD_INPUT_HANDLE);
  if (handle = INVALID_HANDLE_VALUE) or (handle = 0) then
  begin
    result := false;
    exit;
  end;
  // GetConsoleMode succeeds only when the handle is a real console — pipes
  // and redirected files return false.
  result := GetConsoleMode(handle, mode);
end;

function TConsoleTrustPromptStrategy.AskUser(out decision : TTrustPromptDecision) : boolean;
var
  line : string;
  ch : Char;
begin
  if not StdinIsTty then
  begin
    //the install is blocked, and the cache says so.
    FLogger.Information('  This is not an interactive session, so the install is blocked. Set ' +
      'signing.authorDowngradePolicy=allow in dpm.config.yaml to allow author signature changes.', true);
    decision := tpdBlockOnce;
    result := false;
    exit;
  end;

  Write('  [o]verride and install, [c]ancel install, [a]lways block? (c) ');
  ReadLn(line);
  line := Trim(line);
  if line = '' then
    ch := 'c'
  else
    ch := line[1];
  case ch of
    'o', 'O' : begin decision := tpdOverride;    result := true;  end;
    'a', 'A' : begin decision := tpdBlockAlways; result := false; end;
  else
    decision := tpdBlockOnce;
    result := false;
  end;
end;

function TConsoleTrustPromptStrategy.PromptAuthorDowngrade(
  const context : TTrustPromptContext;
  out decision : TTrustPromptDecision) : boolean;
begin
  FLogger.Information('--', true);
  FLogger.Information(Format('DPM detected an author-signing change for %s %s',
    [context.PackageId, context.Version]), true);
  if context.PreviousSpkiHex <> '' then
    FLogger.Information('  Previously signed by: sha256:' + context.PreviousSpkiHex, true);
  if context.NewSigned then
    FLogger.Information('  New build signed by: sha256:' + context.NewSpkiHex, true)
  else
    FLogger.Information('  New build is UNSIGNED.', true);

  result := AskUser(decision);
end;

function TConsoleTrustPromptStrategy.PromptRepositoryRatchet(
  const context : TRepoTrustPromptContext;
  out decision : TTrustPromptDecision) : boolean;
begin
  FLogger.Information('--', true);
  FLogger.Information(Format('DPM detected a repository trust change for %s %s (V-24)',
    [context.PackageId, context.Version]), true);
  if context.PreviousRepoSpkiHex <> '' then
  begin
    if context.PreviousNamespace <> '' then
      FLogger.Information(Format('  Previously from trusted repo: sha256:%s / namespace "%s"',
        [context.PreviousRepoSpkiHex, context.PreviousNamespace]), true)
    else
      FLogger.Information('  Previously from trusted repo: sha256:' + context.PreviousRepoSpkiHex, true);
  end;
  if context.NewHasTrustedRepo then
    FLogger.Information(Format('  New build: trusted repo sha256:%s / namespace "%s"',
      [context.NewRepoSpkiHex, context.NewNamespace]), true)
  else
    FLogger.Information('  New build: no trusted-repository signature.', true);

  result := AskUser(decision);
end;

end.
