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
  System.SysUtils,
  VSoft.CancellationToken,
  VSoft.Process;

type
  TProcessMessagePumpProc = procedure;

  //A line of a process's output - from stdout or stderr - without its line end.
  TProcessOutputLineProc = reference to procedure(const line : string);

  TProcessRunner = class
  public
    ///<summary> Runs a configured process and waits on the calling thread until the run has completed - its output
    ///  delivered and its handles closed. The caller sets the process up (executable, arguments, working folder,
    ///  environment, timeout) and attaches no stdout / stderr targets of its own.
    ///  With onLine assigned, each line the process writes to stdout or stderr is decoded with encoding (the caller's,
    ///  kept alive until Run returns; nil leaves it to VSoft.Process) and handed to onLine in order, on the calling
    ///  thread and never another. Each stream has its own line splitter, so an unfinished line on one is never joined
    ///  to a line from the other. With onLine nil the output is not read.
    ///  The wait goes through WaitForAnyHandle, so on the main thread of a UI host that installed ProcessMessagePump
    ///  the host keeps running. cancellationToken and StartInfo.TimeoutInMSec stop the run - the whole process tree.
    ///  Returns false when the process could not be started (an exception raised while starting it included), with
    ///  errorMessage saying why. A token that is already cancelled also returns false (errorMessage says so) - check
    ///  cancellationToken.IsCancelled before reporting a failure to start. Run attaches its own pipe targets to
    ///  process, which stay attached - give it a process that has not been through Run before. Otherwise true, and
    ///  process.Status, ExitCode and TimedOut say how the run ended. If onLine raises, the run is stopped and waited
    ///  for, then the exception is raised again. </summary>
    class function Run(const process : IProcess; const cancellationToken : ICancellationToken;
                       const onLine : TProcessOutputLineProc; const encoding : TEncoding;
                       out errorMessage : string) : boolean;
  end;

var
  //Optional hook installed by a UI host (the IDE) so TProcessRunner can keep the host's message loop alive while it is
  //blocked waiting for a child process (e.g. a multi-second package compile run on the main thread). When this is nil -
  //as it is in the CLI / console, where there is no message loop to pump - the wait is a plain blocking one. The host
  //installs a proc that calls Application.ProcessMessages.
  ProcessMessagePump : TProcessMessagePumpProc = nil;

///<summary> Waits for any of handles, as WaitForMultipleObjects does (not all) - the same WAIT_* results. On the main
///  thread of a UI host that has installed ProcessMessagePump the host's message loop is pumped while it waits, so
///  the host stays responsive : it repaints, and its Cancel button (which signals a cancellation token being waited
///  on) keeps working. Anywhere else it is a plain blocking wait. </summary>
function WaitForAnyHandle(const handles : array of THandle; const timeoutMs : Cardinal) : Cardinal;


implementation

uses
  WinApi.Windows,
  System.Classes,
  System.SyncObjs;

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

const
  //how long to wait for a stopped run to complete. Its process tree is killed, so this is only reached if something is
  //badly wrong.
  cStopWaitMs = 5 * 60 * 1000;

type
  //The lines a run's pipe targets have read, waiting for Run to hand them to onLine on its own thread. Reference
  //counted, so a target still held by the process can never reach a freed queue.
  ILineQueue = interface
    ['{DED72CBF-4632-4E95-8E63-531A0C10AEAC}']
    //called on the threads VSoft.Process calls the pipe targets on.
    procedure Add(const line : string);
    //moves every queued line, in order, into lines.
    procedure TakeAll(const lines : TStrings);
    //signalled when a line has been added - auto reset.
    function Handle : THandle;
  end;

  TLineQueue = class(TInterfacedObject, ILineQueue)
  private
    FLock : TCriticalSection;
    FLines : TStringList;
    FLinesQueued : TEvent;
  protected
    procedure Add(const line : string);
    procedure TakeAll(const lines : TStrings);
    function Handle : THandle;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TLineQueue.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FLines := TStringList.Create;
  //auto reset - each wake takes everything queued so far.
  FLinesQueued := TEvent.Create(nil, false, false, '');
end;

destructor TLineQueue.Destroy;
begin
  FLinesQueued.Free;
  FLines.Free;
  FLock.Free;
  inherited;
end;

procedure TLineQueue.Add(const line : string);
begin
  FLock.Enter;
  try
    FLines.Add(line);
  finally
    FLock.Leave;
  end;
  FLinesQueued.SetEvent;
end;

procedure TLineQueue.TakeAll(const lines : TStrings);
begin
  lines.Clear;
  FLock.Enter;
  try
    lines.AddStrings(FLines);
    FLines.Clear;
  finally
    FLock.Leave;
  end;
end;

function TLineQueue.Handle : THandle;
begin
  result := FLinesQueued.Handle;
end;

//A line target for one stream - each stream gets its own.
function LineTarget(const queue : ILineQueue; const encoding : TEncoding) : IPipeTarget;
var
  target : ILineQueue;
begin
  target := queue;
  result := TPipeTarget.ToLineDelegate(
    procedure(const line : string)
    begin
      target.Add(line);
    end, encoding);
end;

//Hands every queued line to onLine, in order.
procedure Deliver(const queue : ILineQueue; const lines : TStrings; const onLine : TProcessOutputLineProc);
var
  i : integer;
begin
  queue.TakeAll(lines);
  for i := 0 to lines.Count - 1 do
    onLine(lines[i]);
  lines.Clear;
end;

//Stops the run and waits for it to complete - so nothing is left running, or holding files open, when Run raises.
procedure StopAndWait(const process : IProcess);
begin
  process.Terminate;
  WaitForAnyHandle([process.CompletionHandle], cStopWaitMs);
end;

{ TProcessRunner }

class function TProcessRunner.Run(const process : IProcess; const cancellationToken : ICancellationToken;
                                  const onLine : TProcessOutputLineProc; const encoding : TEncoding;
                                  out errorMessage : string) : boolean;
var
  queue : ILineQueue;
  lines : TStringList;
  waitResult : Cardinal;
  done : boolean;
  started : boolean;
  waitError : Cardinal;
  waitException : EOSError;
begin
  result := false;
  errorMessage := '';
  queue := nil;
  if Assigned(onLine) then
  begin
    queue := TLineQueue.Create;
    process.WithStdOutPipe(LineTarget(queue, encoding));
    process.WithStdErrPipe(LineTarget(queue, encoding));
  end;

  //validation failed, the OS refused, or the token was already cancelled.
  //ExecuteAsync can also raise (pipe or threadpool creation failed). Nothing is running then, so nothing to stop.
  try
    started := process.ExecuteAsync(cancellationToken);
  except
    on e : Exception do
    begin
      errorMessage := e.Message;
      exit;
    end;
  end;
  if not started then
  begin
    errorMessage := Trim(process.Errors.Text);
    if errorMessage = '' then
      errorMessage := 'The process could not be started.';
    exit;
  end;

  lines := TStringList.Create;
  try
    try
      done := false;
      while not done do
      begin
        if queue <> nil then
          waitResult := WaitForAnyHandle([process.CompletionHandle, queue.Handle], INFINITE)
        else
          waitResult := WaitForAnyHandle([process.CompletionHandle], INFINITE);
        waitError := GetLastError;
        case waitResult of
          WAIT_OBJECT_0 :
            begin
              //completed - every target's Done has been called, so every line is queued by now.
              if queue <> nil then
                Deliver(queue, lines, onLine);
              done := true;
            end;
          WAIT_OBJECT_0 + 1 :
            Deliver(queue, lines, onLine);
        else
          begin
            waitException := EOSError.CreateFmt('Unexpected result %d waiting for [%s] : %s',
                                   [waitResult, process.StartInfo.Executable, SysErrorMessage(waitError)]);
            waitException.ErrorCode := waitError;
            raise waitException;
          end;
        end;
      end;
    except
      StopAndWait(process);
      raise;
    end;
  finally
    lines.Free;
  end;
  result := true;
end;

end.
