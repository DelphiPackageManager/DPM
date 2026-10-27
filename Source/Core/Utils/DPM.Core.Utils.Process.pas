{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

    //new version using createprocess. The executable is started directly - give its full path when it must
    //not be looked up. With an outputFile, the process's stdout and stderr are both written to it (as
    //'> file 2>&1' would, without needing cmd.exe to do the redirecting) and its stdin is NUL.
    class function Execute2(const cancellationToken : ICancellationToken; const executable : string; const commandLine : string; const startingDir : string; const enviroment : IEnvironmentBlock = nil; const timeout : integer = -1; const outputFile : string = '') : Cardinal;

    //Starts the process and returns immediately - does not wait, does not
    //capture the exit code. Needed for launching something that will outlive us
    //(the dpm installer, which cannot replace dpm.exe until we have exited).
    //Both Execute and Execute2 wait for the child, so neither can be used here.
    //Shows the process window - this is for user facing apps, not tools.
    //Throws if the process cannot be started.
    class procedure ExecuteNoWait(const executable : string; const commandLine : string);
  end;


  TEnvironmentBlockFactory = class
    class function Create(const AList : TStrings; const AReadCurrent : boolean = true) : IEnvironmentBlock;
  end;

  TProcessMessagePumpProc = procedure;

var
  //Optional hook installed by a UI host (the IDE) so TProcess can keep the host's message
  //loop alive while it is blocked waiting for a child process to exit (e.g. a multi-second
  //package compile run on the main thread). When this is nil - as it is in the CLI / console,
  //where there is no message loop to pump - TProcess does a plain blocking wait and never
  //pumps. The host installs a proc that calls Application.ProcessMessages.
  ProcessMessagePump : TProcessMessagePumpProc = nil;

///<summary> Waits for any of handles, as WaitForMultipleObjects does (not all) - the same WAIT_* results. On the main
///  thread of a UI host that has installed ProcessMessagePump the host's message loop is pumped while it waits, so
///  the host stays responsive : it repaints, and its Cancel button (which signals a cancellation token being waited
///  on) keeps working. Anywhere else it is a plain blocking wait. </summary>
function WaitForAnyHandle(const handles : array of THandle; const timeoutMs : Cardinal) : Cardinal;


implementation

uses
  WinApi.Windows,
  WinApi.ShellAPI,
  Winapi.TlHelp32,
  System.SysUtils;

//The job object API - declared here because XE2's Winapi.Windows doesn't have it.
const
  cJobObjectBasicProcessIdList = 3;
  //more than a build ever runs at once - any beyond it are still stopped, just not waited for.
  cMaxJobProcesses = 256;

type
  //JOBOBJECT_BASIC_PROCESS_ID_LIST
  TJobProcessIdList = record
    NumberOfAssignedProcesses : DWORD;
    NumberOfProcessIdsInList : DWORD;
    ProcessIdList : array[0..cMaxJobProcesses - 1] of NativeUInt;
  end;

const
  //what a process we stop is given as its exit code.
  cStoppedExitCode = 1001;
  //how long to wait for stopped processes to go before giving up on them.
  cStopWaitMs = 10000;

function DPMCreateJobObject(lpJobAttributes : Pointer; lpName : PWideChar) : THandle; stdcall; external kernel32 name 'CreateJobObjectW';
function DPMAssignProcessToJobObject(hJob : THandle; hProcess : THandle) : BOOL; stdcall; external kernel32 name 'AssignProcessToJobObject';
function DPMTerminateJobObject(hJob : THandle; uExitCode : UINT) : BOOL; stdcall; external kernel32 name 'TerminateJobObject';
function DPMQueryInformationJobObject(hJob : THandle; infoClass : integer; lpJobObjectInfo : Pointer;
                                      cbJobObjectInfoLength : DWORD; lpReturnLength : PDWORD) : BOOL; stdcall; external kernel32 name 'QueryInformationJobObject';


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


//Terminates the process and everything below it - the fallback for when the process could not be put in
//a job. A parent is stopped before its children, so it can't start another while they are being stopped.
function TerminateProcessTree(const ParentProcessID : Cardinal) : Cardinal;
var
  hSnapshot : THandle;
  entry : PROCESSENTRY32;
  processIds : array of Cardinal;
  parentIds : array of Cardinal;
  count : integer;

  //TerminateProcess only starts the process stopping - wait until it has, and so closed what it had open.
  function _TerminateProcess(const ProcessID : Cardinal) : Cardinal;
  var
    hProcess : THandle;
  begin
    Result := cStoppedExitCode;

    hProcess := OpenProcess(PROCESS_TERMINATE or SYNCHRONIZE, False, ProcessID);

    if (hProcess <> 0) and (hProcess <> INVALID_HANDLE_VALUE) then
    begin
      TerminateProcess(hProcess,Result);
      WaitForSingleObject(hProcess, cStopWaitMs);
      CloseHandle(hProcess);
    end;
  end;

  //every level - one pass over the snapshot only finds the children, not their children.
  procedure TerminateDescendants(const processID : Cardinal);
  var
    i : integer;
    childId : Cardinal;
  begin
    for i := 0 to count - 1 do
    begin
      if (parentIds[i] <> processID) or (processIds[i] = processID) then
        continue;
      childId := processIds[i];
      //visited - a recycled process id can't send us round in circles.
      parentIds[i] := 0;
      _TerminateProcess(childId);
      TerminateDescendants(childId);
    end;
  end;

begin
  Result := _TerminateProcess(ParentProcessID);

  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if hSnapshot = INVALID_HANDLE_VALUE then
    exit;
  try
    count := 0;
    entry.dwSize := SizeOf(PROCESSENTRY32);
    if Process32First(hSnapshot, entry) then
    begin
      repeat
        SetLength(processIds, count + 1);
        SetLength(parentIds, count + 1);
        processIds[count] := entry.th32ProcessID;
        parentIds[count] := entry.th32ParentProcessID;
        Inc(count);
      until not Process32Next(hSnapshot, entry);
    end;
  finally
    CloseHandle(hSnapshot);
  end;
  TerminateDescendants(ParentProcessID);
end;

//Stops the process and everything it started, then waits for them to go - until they have, they still
//hold what they had open (a compile holds its log file, the files it writes in the package cache).
function StopProcess(const processId : Cardinal; const job : THandle) : Cardinal;
var
  ids : TJobProcessIdList;
  handles : array of THandle;
  count : integer;
  i : integer;
begin
  result := cStoppedExitCode;
  if job = 0 then
  begin
    result := TerminateProcessTree(processId);
    exit;
  end;

  //The processes, to wait on. Not the job's own count - that drops to 0 the moment the job is told to
  //stop, before its processes have actually gone and let go of their files.
  FillChar(ids, SizeOf(ids), 0);
  //a list too long for the buffer fails with ERROR_MORE_DATA, but the ids that fit are still there.
  DPMQueryInformationJobObject(job, cJobObjectBasicProcessIdList, @ids, SizeOf(ids), nil);
  SetLength(handles, ids.NumberOfProcessIdsInList);
  count := 0;
  for i := 0 to Integer(ids.NumberOfProcessIdsInList) - 1 do
  begin
    handles[count] := OpenProcess(SYNCHRONIZE, False, Cardinal(ids.ProcessIdList[i]));
    if handles[count] <> 0 then
      Inc(count);
  end;

  DPMTerminateJobObject(job, cStoppedExitCode);
  for i := 0 to count - 1 do
  begin
    WaitForSingleObject(handles[i], cStopWaitMs);
    CloseHandle(handles[i]);
  end;
end;


function WaitForAnyHandle(const handles : array of THandle; const timeoutMs : Cardinal) : Cardinal;
var
  waitHandles : array of THandle;
  count : DWORD;
  i : integer;
  startTick : Cardinal;
  elapsed : Cardinal;
  remaining : Cardinal;
  res : Cardinal;
begin
  count := Length(handles);
  SetLength(waitHandles, count);
  for i := 0 to High(handles) do
    waitHandles[i] := handles[i];
  //only the main thread pumps the host's message loop.
  if (not Assigned(ProcessMessagePump)) or (GetCurrentThreadId <> MainThreadID) then
  begin
    result := WaitForMultipleObjects(count, @waitHandles[0], False, timeoutMs);
    exit;
  end;

  startTick := GetTickCount;
  while True do
  begin
    if timeoutMs = INFINITE then
      remaining := INFINITE
    else
    begin
      elapsed := GetTickCount - startTick; //Cardinal subtraction is wraparound-safe
      if elapsed >= timeoutMs then
      begin
        result := WAIT_TIMEOUT;
        exit;
      end;
      remaining := timeoutMs - elapsed;
    end;

    //the 'a message is waiting' wake is WAIT_OBJECT_0 + count. With bWaitAll = False a signalled handle is returned
    //in preference to the message wake, so the loop always ends promptly once one is signalled.
    res := MsgWaitForMultipleObjects(count, waitHandles[0], False, remaining, QS_ALLINPUT);
    if res = WAIT_OBJECT_0 + count then
      ProcessMessagePump //let the host drain its queue, then wait again
    else
    begin
      result := res; //signalled, abandoned, timed out, or the wait failed
      exit;
    end;
  end;
end;

//A program that can't be started has no exit code to return.
procedure RaiseLaunchError(const executable : string; const what : string; const errorCode : Cardinal);
var
  e : EOSError;
  detail : string;
begin
  detail := SysErrorMessage(errorCode);
  if what <> '' then
    detail := what + ' : ' + detail;
  e := EOSError.CreateFmt('Unable to start [%s] - %s', [executable, detail]);
  e.ErrorCode := errorCode;
  raise e;
end;

{ TProcess }

class function TProcess.Execute2(const cancellationToken : ICancellationToken; const executable : string; const commandLine : string; const startingDir : string; const enviroment : IEnvironmentBlock = nil; const timeout : integer = -1; const outputFile : string = '') : Cardinal;
var
  sParams: string;
  pDir: PChar;
  pApplication: PChar;
  launchError: Cardinal;

  pEnvironment : Pointer;
  startupInfo : TStartupInfo;
  processInfo: TProcessInformation;
  timeOutDuration: Cardinal;

  waitRes: Cardinal;
  objHandles: array[0..1] of THandle;
  job : THandle;

  securityAttributes : TSecurityAttributes;
  outputHandle : THandle;
  inputHandle : THandle;

  function DoGetExitCodeProcess: Cardinal;
  begin
    if not GetExitCodeProcess(processInfo.hProcess, result) then
      result := GetLastError;
  end;

  //Wait for the process to exit (index 0) or the cancellation token to be signalled (index 1) - keeping a UI host
  //responsive during a long compile (see WaitForAnyHandle).
  function DoWaitForProcess: Cardinal;
  begin
    result := WaitForAnyHandle(objHandles, timeoutDuration);
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
  //A path is started as that file and nothing else. A bare name is looked for in DPM's own folder and the current
  //folder before the system folders - callers that start anything that matters pass the full path.
  if ExtractFileDrive(executable) <> '' then
    pApplication := PChar(executable)
  else
    pApplication := nil;

  outputHandle := 0;
  inputHandle := 0;
  if outputFile <> '' then
  begin
    //inheritable, so the process gets them as its standard handles.
    FillChar(securityAttributes, SizeOf(securityAttributes), 0);
    securityAttributes.nLength := SizeOf(securityAttributes);
    securityAttributes.bInheritHandle := true;
    outputHandle := CreateFile(PChar(outputFile), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, @securityAttributes,
                               CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if outputHandle = INVALID_HANDLE_VALUE then
      RaiseLaunchError(executable, 'unable to create the output file [' + outputFile + ']', GetLastError);
    inputHandle := CreateFile('NUL', GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, @securityAttributes, OPEN_EXISTING, 0, 0);
    if inputHandle = INVALID_HANDLE_VALUE then
    begin
      launchError := GetLastError;
      CloseHandle(outputHandle);
      RaiseLaunchError(executable, 'unable to open NUL for its input', launchError);
    end;
    startupInfo.dwFlags := startupInfo.dwFlags or STARTF_USESTDHANDLES;
    startupInfo.hStdInput := inputHandle;
    //one handle for both, so what the process writes to each stays in order.
    startupInfo.hStdOutput := outputHandle;
    startupInfo.hStdError := outputHandle;
  end;

  //Suspended, so it is in the job before it can start anything - the job is how a stop reaches the whole
  //tree (cmd.exe starts cmd, which starts msbuild, which starts the compiler).
  if CreateProcess(pApplication, PChar(sParams), nil, nil, outputHandle <> 0, CREATE_UNICODE_ENVIRONMENT + CREATE_NEW_CONSOLE + CREATE_SUSPENDED, pEnvironment, pDir, startupInfo, processInfo) then
    launchError := 0
  else
    launchError := GetLastError;
  //the process has its own copies now - ours would only hold the file open.
  if outputHandle <> 0 then
  begin
    CloseHandle(outputHandle);
    CloseHandle(inputHandle);
  end;
  //not an exit code - the program never ran.
  if launchError <> 0 then
    RaiseLaunchError(executable, '', launchError);

  job := DPMCreateJobObject(nil, nil);
  if (job <> 0) and (not DPMAssignProcessToJobObject(job, processInfo.hProcess)) then
  begin
    //eg Windows 7, when DPM itself already runs in a job - StopProcess walks the process tree instead.
    CloseHandle(job);
    job := 0;
  end;

  ResumeThread(processInfo.hThread);
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
  waitRes := DoWaitForProcess;

 case waitRes of
    WAIT_OBJECT_0: // Process.hProcess has exited
      begin
        Result := DoGetExitCodeProcess;
      end;
    WAIT_OBJECT_0 + 1: // Event signalled to terminate process
      begin
        result := StopProcess(processInfo.dwProcessId, job);
      end;
    WAIT_TIMEOUT: // Timed out
      begin
        Result := DoGetExitCodeProcess; // Check is still running (just in case)
        if (result = STILL_ACTIVE)  then
        begin
          result := StopProcess(processInfo.dwProcessId, job);
        end;
      end;
  else // Something else happened (like WAIT_FAILED)
    begin
        if job <> 0 then
          CloseHandle(job);
        CloseHandle(processInfo.hProcess);
        raise Exception.Create('IProcess : Unexpected event wait result ' + IntToStr(waitRes));
    end;
  end;

  if job <> 0 then
    CloseHandle(job);
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

class procedure TProcess.ExecuteNoWait(const executable, commandLine : string);
var
  shellInfo : TShellExecuteInfo;
begin
  FillChar(shellInfo, SizeOf(shellInfo), 0);
  shellInfo.cbSize := SizeOf(TShellExecuteInfo);
  //No SEE_MASK_NOCLOSEPROCESS - we never wait on or close the handle, so we do
  //not want one returned to us.
  shellInfo.fMask := 0;
  shellInfo.Wnd := 0;
  //nil verb means 'open' - which lets the shell handle the UAC elevation
  //prompt if the target's manifest asks for it.
  shellInfo.lpVerb := nil;
  shellInfo.lpFile := PChar(executable);
  shellInfo.lpParameters := PChar(commandLine);
  shellInfo.lpDirectory := PChar(ExtractFilePath(executable));
  shellInfo.nShow := SW_SHOWNORMAL;
  shellInfo.hInstApp := 0;

  if not ShellExecuteEx(@shellInfo) then
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
  //the value - the list holds NAME=value.
  if idx <> -1 then
    result := FVariables.ValueFromIndex[idx];
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

