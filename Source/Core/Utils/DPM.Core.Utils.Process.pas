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

uses
  VSoft.CancellationToken;

type
  // Simple wrapper around ShellExecute for now
  //TODO : Rework to use CreateProcess and capture stdout using pipes.
  TProcess = class
    //throws if cannot create process.
    class function Execute(const cancellationToken : ICancellationToken; const executable : string; const commandLine : string) : Cardinal;
  end;


implementation

uses
  WinApi.Windows,
  WinApi.ShellAPI,
  System.SysUtils;

{ TProcess }

class function TProcess.Execute(const cancellationToken : ICancellationToken; const executable, commandLine : string) : Cardinal;
var
  shellInfo : TShellExecuteInfo;
  waitHandles : array[0..1] of THandle;
  waitRes : DWORD;
begin
  result := MaxInt;
  shellInfo.cbSize := sizeOf(TShellExecuteInfo);
  shellInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  shellInfo.Wnd := 0;
  shellInfo.lpVerb := nil;
  shellInfo.lpFile := PChar(executable);
  shellInfo.lpParameters := PChar(commandLine);
  shellInfo.lpDirectory := PChar(ExtractFilePath(executable));
  shellInfo.nShow := SW_HIDE;
  shellInfo.hInstApp := 0;

  if ShellExecuteEx(@shellInfo) then
  begin
    waitHandles[0] := shellInfo.hProcess;
    waitHandles[1] := cancellationToken.Handle;
    waitRes := WaitForMultipleObjects(2, @waithandles[0], false, 60000);
    try
      case waitRes of
        WAIT_OBJECT_0 : // Process has exited
          begin
            //all good
          end;
        WAIT_OBJECT_0 + 1 : // Event signalled to terminate process
          begin
            TerminateProcess(shellInfo.hProcess, 999);
            result := 999;
          end;
        WAIT_TIMEOUT : // Timed out
          begin
            TerminateProcess(shellInfo.hProcess, 888);

          end;
      else // Something else happened (like WAIT_FAILED)
        raise Exception.Create('Unexpected event wait result ' + IntToStr(waitRes));
      end;
    finally
      GetExitCodeProcess(shellInfo.hProcess, result);
      CloseHandle(shellInfo.hProcess);
    end;
  end
  else
    raise Exception.Create('Unable to execute process : ' + SysErrorMessage(GetLastError));
end;

end.

