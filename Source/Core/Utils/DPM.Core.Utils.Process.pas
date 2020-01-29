{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Utils.Process;

interface

type
  // Simple wrapper around ShellExecute for now
  //TODO : Rework to use CreateProcess and capture stdout using pipes.
  TProcess = class
    //throws if cannot create process.
    class function Execute(const exe : string; const commandLine : string) : Cardinal;
  end;


implementation

uses
  WinApi.Windows,
  WinApi.ShellAPI,
  System.SysUtils;

{ TProcess }

class function TProcess.Execute(const exe, commandLine: string): Cardinal;
var
  shellInfo : TShellExecuteInfo;
begin
  result := MaxInt;
  shellInfo.cbSize := sizeOf(TShellExecuteInfo);
  shellInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  shellInfo.Wnd := 0;
  shellInfo.lpVerb := nil;
  shellInfo.lpFile := 'cmd.exe';
  shellInfo.lpParameters := PChar(commandLine);
  shellInfo.lpDirectory := nil;
  shellInfo.nShow := SW_HIDE;
  shellInfo.hInstApp := 0;

  if ShellExecuteEx(@shellInfo) then
  begin
    WaitForSingleObject(shellInfo.hProcess,INFINITE);
    GetExitCodeProcess(shellInfo.hProcess, result);
    CloseHandle(shellInfo.hProcess);
  end
  else
    raise Exception.Create('Unable to execute process : ' + SysErrorMessage(GetLastError));
end;

end.
