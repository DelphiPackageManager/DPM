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
// the user for [t]rust/[b]lock/[a]lways-block. When stdin is redirected
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
  protected
    function PromptAuthorDowngrade(const context : TTrustPromptContext;
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

function TConsoleTrustPromptStrategy.PromptAuthorDowngrade(
  const context : TTrustPromptContext;
  out decision : TTrustPromptDecision) : boolean;
var
  line : string;
  ch : Char;
begin
  decision := tpdBlockOnce;
  result := false;

  FLogger.Warning('--');
  FLogger.Warning(Format('DPM detected an author-signing change for %s %s',
    [context.PackageId, context.Version]));
  if context.PreviousSpkiHex <> '' then
    FLogger.Warning('  Previously signed by: sha256:' + context.PreviousSpkiHex);
  if context.NewSigned then
    FLogger.Warning('  New build signed by: sha256:' + context.NewSpkiHex)
  else
    FLogger.Warning('  New build is UNSIGNED.');

  if not StdinIsTty then
  begin
    FLogger.Error('  Non-interactive session — blocking. Set ' +
      'signing.authorDowngradePolicy=allow in dpm.config.yaml to override.');
    exit;
  end;

  Write('  [t]rust this build, [b]lock this build, [a]lways block? (b) ');
  ReadLn(line);
  line := Trim(line);
  if line = '' then
    ch := 'b'
  else
    ch := line[1];
  case ch of
    't', 'T' : begin decision := tpdTrustOnce;   result := true;  end;
    'a', 'A' : begin decision := tpdBlockAlways; result := false; end;
  else
    decision := tpdBlockOnce;
    result := false;
  end;
end;

end.
