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
  System.Classes,
  VSoft.CancellationToken;

type
  IEnvironmentBlock = interface
  ['{5A5AA8E1-1F8F-424D-827C-75B0C43FF972}']
    procedure Clear;
    procedure LoadCurrentEnvironmentBlock;
    procedure AddOrSet(const name : string; const value : string);
    function GetValue(const name : string) : string;
    procedure ApplyValues(const list : TStrings);
    procedure RemoveVariable(const name : string);
    function GetEnvironmentBlock : Pointer;
  end;



  // Simple wrapper around ShellExecute for now
  //TODO : Rework to use CreateProcess and capture stdout using pipes.
  TProcess = class
    //throws if cannot create process.
    class function Execute(const cancellationToken : ICancellationToken; const executable : string; const commandLine : string) : Cardinal;

    //new version using createprocess - not capturing output.
    class function Execute2(const cancellationToken : ICancellationToken; const executable : string; const commandLine : string; const startingDir : string; const enviroment : IEnvironmentBlock = nil; const timeout : integer = -1) : Cardinal;
  end;


  TEnvironmentBlockFactory = class
    class function Create(const AList : TStrings; const AReadCurrent : boolean = true) : IEnvironmentBlock;
  end;


implementation

uses
  WinApi.Windows,
  WinApi.ShellAPI,
  Winapi.TlHelp32,
  System.SysUtils;


function StripQuotes(const value : string) : string;
var
  len: integer;
begin
  result := value;
  len := Length(Value);
  if len > 2 then
  begin
    if (Value[1] = Value[len]) then
    begin
      if (Value[1] = '"') or (Value[1] = '''') then
        Result := Copy(value, 2, len - 2);
    end;
  end;
end;

function DoubleQuoteString(const value: string; const checkString: boolean = True): string;
begin
  result := StripQuotes(value);
  if checkString then
  begin
    if (pos(' ', result) > 0) then
      result := '"' + result + '"';
  end
  else
    result := '"' + result + '"';
end;


function TerminateProcessTree(const ParentProcessID : Cardinal) : Cardinal;
var
  hSnapshot : THandle;

  function _TerminateProcess(const ProcessID : Cardinal) : Cardinal; // A simple wrapper around TerminateProcess
  var
    hProcess : THandle;
  begin
    Result := 1001;

    hProcess := OpenProcess(PROCESS_TERMINATE,False, ProcessID);

    if (hProcess <> 0) and (hProcess <> INVALID_HANDLE_VALUE) then
    begin
      TerminateProcess(hProcess,Result);
      CloseHandle(hProcess);
    end;
  end;

  function _TerminateProcessAndChildren(const snapshot : THandle; const processID : DWORD) : Cardinal; // Nested recursive function
  var
    processInfo : PROCESSENTRY32;
    res : Boolean;
  begin
    processInfo.dwSize := sizeof(PROCESSENTRY32);

    res := Process32First(snapshot, processInfo);

    while res do
    begin
      if processInfo.th32ParentProcessID = ProcessID then // Child process of ProcessID
        _TerminateProcess(processInfo.th32ProcessID);

      res := Process32Next(hSnapshot, processInfo);
    end;

    result := _TerminateProcess(processID);
  end;
begin
  // First set up a global system snapshot to provide data to the recursive function
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if hSnapshot = INVALID_HANDLE_VALUE then
  begin
    // If we can't get a snapshot (for some reason) just kill the parent process
    Result := _TerminateProcess(ParentProcessID);
    exit;
  end;

  try
    Result := _TerminateProcessAndChildren(hSnapshot, ParentProcessID);
  finally
    CloseHandle(hSnapshot);
  end;
end;


{ TProcess }

class function TProcess.Execute2(const cancellationToken : ICancellationToken; const executable : string; const commandLine : string; const startingDir : string; const enviroment : IEnvironmentBlock = nil; const timeout : integer = -1) : Cardinal;
var
  sParams: string;
  pDir: PChar;

  pEnvironment : Pointer;
  startupInfo : TStartupInfo;
  processInfo: TProcessInformation;
  timeOutDuration: Cardinal;

  waitRes: Cardinal;
  objHandles: array[0..1] of THandle;

  function DoGetExitCodeProcess: Cardinal;
  begin
    if not GetExitCodeProcess(processInfo.hProcess, result) then
      result := GetLastError;
  end;

begin
  FillChar(startupInfo, SizeOf(TStartupInfo), 0);
  FillChar(processInfo, SizeOf(TProcessInformation), 0);

  startupInfo.cb := SizeOf(startupInfo);
  startupInfo.dwFlags := STARTF_USESHOWWINDOW;
  startupInfo.wShowWindow := SW_HIDE;

  if enviroment <> nil then
    pEnvironment := enviroment.GetEnvironmentBlock
  else
    pEnvironment := nil;

  if startingDir <> '' then
    pDir := PChar(startingDir)
  else
    pDir := nil;

  sParams := DoubleQuoteString(executable) + ' ' + commandLine;

  if not CreateProcess(nil, PChar(sParams), nil, nil, false, CREATE_UNICODE_ENVIRONMENT + CREATE_NEW_CONSOLE, pEnvironment, pDir, startupInfo, processInfo) then
  begin
    result := GetLastError;
    exit;
  end;

  //we don't need this handle.
  CloseHandle(processInfo.hThread);


  { Set timeout length }
  if timeout < 0 then
    timeoutDuration := INFINITE
  else
    timeoutDuration := Cardinal(timeout);


  objHandles[0] := processInfo.hProcess;
  objHandles[1] := cancellationToken.Handle;

  { Wait for Something interesting to happen }
  waitRes := WaitForMultipleObjects(2, @objHandles, False, timeoutDuration);

 case waitRes of
    WAIT_OBJECT_0: // Process.hProcess has exited
      begin
        Result := DoGetExitCodeProcess;
      end;
    WAIT_OBJECT_0 + 1: // Event signalled to terminate process
      begin
        result := TerminateProcessTree(processInfo.dwProcessId);
      end;
    WAIT_TIMEOUT: // Timed out
      begin
        Result := DoGetExitCodeProcess; // Check is still running (just in case)
        if (result = STILL_ACTIVE)  then
        begin
          result := TerminateProcessTree(processInfo.dwProcessId);
        end;
      end;
  else // Something else happened (like WAIT_FAILED)
    begin
        CloseHandle(processInfo.hProcess);
        raise Exception.Create('IProcess : Unexpected event wait result ' + IntToStr(waitRes));
    end;
  end;

  CloseHandle(processInfo.hProcess);


end;



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


type
  TEnvBlock = class(TInterfacedObject,IEnvironmentBlock)
  private
    FChanged : boolean;
    FData : TBytes;
    FVariables : TStringList;
  public
    constructor Create(const list : TStrings; const AReadCurrent : boolean = true);
    destructor Destroy;override;
    procedure Clear;
    procedure LoadCurrentEnvironmentBlock;
    procedure AddOrSet(const name : string; const value : string);
    function GetValue(const name : string) : string;
    procedure ApplyValues(const list : TStrings);
    procedure RemoveVariable(const name : string);
    function GetEnvironmentBlock : Pointer;
    function GetEnvironmentBlockText : string;
  end;


{ TEnvBlock }

constructor TEnvBlock.Create(const list : TStrings; const AReadCurrent: boolean);
begin
  inherited Create;
  SetLength(FData,0);
  FVariables := TStringList.Create;
  if AReadCurrent then
    LoadCurrentEnvironmentBlock;
  if list <> nil then
    ApplyValues(list);
end;

destructor TEnvBlock.Destroy;
begin
  SetLength(FData,0);
  FVariables.Free;
  inherited;
end;



procedure TEnvBlock.AddOrSet(const name, value: string);
var
  idx : integer;
begin
  idx := FVariables.IndexOfName(name);
  if idx <> -1 then
    FVariables.ValueFromIndex[idx] := value
  else
    FVariables.Add(name +'=' + value);

  FChanged := True;
end;

procedure TEnvBlock.ApplyValues(const list: TStrings);
var
  I: Integer;
begin
  if list = nil then
    exit;
  for I := 0 to list.Count -1 do
  begin
    if Pos('=',list.Strings[i]) <> 0 then
      AddOrSet(list.Names[i],list.ValueFromIndex[i]);
  end;
end;

procedure TEnvBlock.Clear;
begin
  FVariables.Clear;
  FChanged := True;
  SetLength(FData,0);
  //Should we zero the data here?
end;



function TEnvBlock.GetEnvironmentBlock: Pointer;
var
  len : integer;
  bytes : TBytes;
  byteCount : integer;
  count : integer;
  i: Integer;
begin
  if not FChanged  then
  begin
    if Length(FData) > 0 then
      result := @FData[0]
    else
      result := nil;
    exit;
  end;
  SetLength(FData,0);

  len := 2048;
  SetLength(FData,2048);

  count := 0;
  for i := 0 to FVariables.Count - 1 do
  begin
    bytes := TEncoding.Unicode.GetBytes(FVariables.Strings[i]);
    byteCount := Length(bytes);
    while (count + byteCount + 2) > len do
    begin
      Inc(len,2048);
      SetLength(FData,len);
    end;

    Move(bytes[0],FData[count],byteCount);
    Inc(count,byteCount);
    FData[count] := 0;
    Inc(count);
    FData[count] := 0;
    Inc(count);
  end;
  if count + 2 > len then
  begin
    Inc(len,2);
    SetLength(FData,len);
  end;
  FData[count] := 0;
  FData[count+1] := 0;
  result := @FData[0];

end;

function TEnvBlock.GetEnvironmentBlockText: string;
begin
  result := FVariables.Text;
end;

function TEnvBlock.GetValue(const name: string): string;
var
  idx : integer;
begin
  result := '';
  idx := FVariables.IndexOfName(name);
  if idx <> -1 then
    result := FVariables.Strings[idx];
end;

procedure TEnvBlock.LoadCurrentEnvironmentBlock;
var
  PEnvBlock : PChar;
  pTmp : PChar;
  sTmp : string;
begin
  Self.Clear;
  PEnvBlock := GetEnvironmentStrings;
  try
    pTmp := PEnvBlock;
    while pTmp^ <> #0 do
    begin
      stmp := string(pTmp);
      pTmp := pTmp + Length(sTmp) + 1;
      if (Trim(sTmp) <> '') and (Trim(stmp) <> '=::=::\') then
        FVariables.Add(stmp);
    end;    // while
  finally
    FreeEnvironmentStrings(PEnvBlock);
  end;
  FChanged := True;
end;

procedure TEnvBlock.RemoveVariable(const name: string);
var
  idx : integer;
begin
  idx := FVariables.IndexOfName(name);
  if idx <> -1 then
  begin
    FVariables.Delete(idx);
    FChanged := true;
  end;
end;

{ TEnvironmentBlockFactory }

class function TEnvironmentBlockFactory.Create(const AList: TStrings; const AReadCurrent: boolean): IEnvironmentBlock;
begin
  result := TEnvBlock.Create(AList, AReadCurrent);
end;

end.

