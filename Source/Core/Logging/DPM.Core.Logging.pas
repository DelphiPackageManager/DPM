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

unit DPM.Core.Logging;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Diagnostics;

{$SCOPEDENUMS ON}

type
  TLogLevel = (Debug, Verbose, Information, Success, Warning, Error);

  //Errors and warnings carry a code (DPM.Core.Diagnostics, docs\error-codes.md). An error means the
  //operation fails, and is reported once, by the code that detected the problem - callers that see
  //the failure come back don't report it again.
  ILogger = interface
    ['{92B7AF6E-37BC-4315-A59F-275DD5D906A1}']
    procedure Debug(const data : string);
    procedure Verbose(const data : string; const important : boolean = false);
    procedure Information(const data : string; const important : boolean = false);
    //the uncoded Warning and Error are being phased out - every warning and error gets a code.
    procedure Warning(const data : string; const important : boolean = false); overload;
    procedure Warning(const code : TWarningCode; const data : string; const important : boolean = false); overload;
    procedure Error(const data : string); overload;
    procedure Error(const code : TErrorCode; const data : string); overload;
    procedure Success(const data : string; const important : boolean = false);
    //clears the visible log where that is supported (not in the console logger), and starts a new
    //duplicate scope for the calling thread.
    procedure Clear;
    procedure NewLine;
    //starts a new duplicate scope for the calling thread - within a scope an identical warning or error
    //is only shown once. TOperationGuard.Start calls this.
    procedure StartDiagnosticScope;
    //the number of errors reported through this logger by the calling thread, duplicates included.
    //It only ever goes up - take a mark and compare.
    function ErrorCount : integer;

    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);

    property Verbosity : TVerbosity read GetVerbosity write SetVerbosity;
  end;

  ///<summary> What every logger shares - level filtering, formatting warnings and errors
  ///  ('error DPME0001: ...'), suppressing duplicates and counting errors per thread. A descendant just
  ///  writes the finished text somewhere. Safe to call from any thread. </summary>
  TBaseLogger = class(TInterfacedObject, ILogger)
  private
    FVerbosity : TVerbosity;
    FLock : TCriticalSection;
    FErrorCounts : IDictionary<Cardinal, integer>;
    FReported : IDictionary<Cardinal, ISet<string>>;
    procedure Report(const level : TLogLevel; const code : string; const data : string; const important : boolean);
  protected
    function IsEnabled(const level : TLogLevel; const important : boolean) : boolean; virtual;
    //text is ready to show - warnings and errors are already formatted.
    procedure WriteMessage(const level : TLogLevel; const text : string; const important : boolean); virtual; abstract;
    //called for every warning and error, duplicates included (they are not written). code is empty
    //for an uncoded one.
    procedure DiagnosticReported(const level : TLogLevel; const code : string; const data : string; const duplicate : boolean); virtual;
    procedure DoClear; virtual;
    procedure DoNewLine; virtual;
    function GetVerbosity : TVerbosity; virtual;
    procedure SetVerbosity(const value : TVerbosity); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Debug(const data : string);
    procedure Verbose(const data : string; const important : boolean = false);
    procedure Information(const data : string; const important : boolean = false);
    procedure Warning(const data : string; const important : boolean = false); overload;
    procedure Warning(const code : TWarningCode; const data : string; const important : boolean = false); overload;
    procedure Error(const data : string); overload;
    procedure Error(const code : TErrorCode; const data : string); overload;
    procedure Success(const data : string; const important : boolean = false);
    procedure Clear;
    procedure NewLine;
    procedure StartDiagnosticScope;
    function ErrorCount : integer;

    property Verbosity : TVerbosity read GetVerbosity write SetVerbosity;
  end;

  IDemotingLogger = interface(ILogger)
    ['{6E1F0A57-9B1C-4E5B-8C2D-3A4B5C6D7E8F}']
    //the first warning or error logged through it, formatted with its code - '' when there was none.
    function FirstProblem : string;
  end;

  ///<summary> Sends everything to the wrapped logger's Debug output - for speculative work (trial
  ///  resolves, an optional build) whose problems only matter if the caller decides they do. Errors
  ///  logged through it are not counted; FirstProblem lets the caller report one diagnostic of its own. </summary>
  TDemotingLogger = class(TInterfacedObject, IDemotingLogger, ILogger)
  private
    FTarget : ILogger;
    FFirstProblem : string;
    procedure NoteProblem(const text : string);
  protected
    procedure Debug(const data : string);
    procedure Verbose(const data : string; const important : boolean = false);
    procedure Information(const data : string; const important : boolean = false);
    procedure Warning(const data : string; const important : boolean = false); overload;
    procedure Warning(const code : TWarningCode; const data : string; const important : boolean = false); overload;
    procedure Error(const data : string); overload;
    procedure Error(const code : TErrorCode; const data : string); overload;
    procedure Success(const data : string; const important : boolean = false);
    procedure Clear;
    procedure NewLine;
    procedure StartDiagnosticScope;
    function ErrorCount : integer;
    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);
    function FirstProblem : string;
  public
    constructor Create(const target : ILogger);
  end;

  ///<summary> Adds what a warning or error is about to its message - for code that reports problems
  ///  without knowing what they belong to (the parts of a spec don't know the file they came from). </summary>
  TContextLogger = class(TInterfacedObject, ILogger)
  private
    FTarget : ILogger;
    FContext : string;
  protected
    procedure Debug(const data : string);
    procedure Verbose(const data : string; const important : boolean = false);
    procedure Information(const data : string; const important : boolean = false);
    procedure Warning(const data : string; const important : boolean = false); overload;
    procedure Warning(const code : TWarningCode; const data : string; const important : boolean = false); overload;
    procedure Error(const data : string); overload;
    procedure Error(const code : TErrorCode; const data : string); overload;
    procedure Success(const data : string; const important : boolean = false);
    procedure Clear;
    procedure NewLine;
    procedure StartDiagnosticScope;
    function ErrorCount : integer;
    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);
  public
    //context goes in front of every warning and error, eg 'Spec [<file name>] : '.
    constructor Create(const target : ILogger; const context : string);
  end;

  ///<summary> Brackets one operation at a boundary - an installer entry point, a command, an IDE action,
  ///  one item of a batch. It starts a new duplicate scope, and makes sure a failed operation never ends
  ///  silently : if it failed without reporting an error, Finish reports DPME0002, which is always a DPM
  ///  bug - whatever failed should have said why. Error counts are per thread, so start it on the thread
  ///  doing the work (inside an async work function, not in its completion handler). </summary>
  TOperationGuard = record
  private
    FLogger : ILogger;
    FErrorMark : integer;
    FReportCancellation : boolean;
  public
    //reportCancellation : true for the outermost boundary only (the command, the IDE action) so that a
    //cancelled operation is reported once.
    class function Start(const logger : ILogger; const reportCancellation : boolean = false) : TOperationGuard; static;
    //errors reported on this thread since Start.
    function ErrorsReported : integer;
    //for an except block that turns the exception into a failure - reports it once, returns false.
    function Failed(const e : Exception) : boolean;
    //what names the operation for the cancelled/failed messages, eg 'Restore'.
    procedure Finish(const succeeded : boolean; const cancelled : boolean; const what : string);
  end;

///<summary> Reports a caught exception that is being turned into a failure - once, with its code.
///  An EDPMError keeps its code, anything else is unexpected (DPME0001). </summary>
procedure LogException(const logger : ILogger; const e : Exception);

///<summary> The code to report an exception with when adding context to its message - an EDPMError's own
///  code, otherwise defaultCode (what failing at that point means). </summary>
function ErrorCodeOf(const e : Exception; const defaultCode : TErrorCode = TErrorCode.E0001_UnexpectedException) : TErrorCode;

implementation

uses
  WinApi.Windows;

const
  //per thread - plenty for one operation, and stops a runaway loop from growing it forever.
  cMaxDuplicateKeys = 1000;

procedure LogException(const logger : ILogger; const e : Exception);
begin
  if e is EDPMError then
    logger.Error(EDPMError(e).Code, e.Message)
  else if e.ClassType = Exception then
    logger.Error(TErrorCode.E0001_UnexpectedException, e.Message)
  else
    logger.Error(TErrorCode.E0001_UnexpectedException, e.ClassName + ' : ' + e.Message);
end;

function ErrorCodeOf(const e : Exception; const defaultCode : TErrorCode) : TErrorCode;
begin
  if e is EDPMError then
    result := EDPMError(e).Code
  else
    result := defaultCode;
end;

{ TBaseLogger }

constructor TBaseLogger.Create;
begin
  inherited Create;
  FVerbosity := TVerbosity.Normal;
  FLock := TCriticalSection.Create;
  FErrorCounts := TCollections.CreateDictionary<Cardinal, integer>;
  FReported := TCollections.CreateDictionary<Cardinal, ISet<string>>;
end;

destructor TBaseLogger.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TBaseLogger.Report(const level : TLogLevel; const code : string; const data : string; const important : boolean);
var
  threadId : Cardinal;
  count : integer;
  reported : ISet<string>;
  key : string;
  duplicate : boolean;
begin
  key := IntToStr(Ord(level)) + ':' + data;
  FLock.Enter;
  try
    threadId := GetCurrentThreadId;
    if level = TLogLevel.Error then
    begin
      if not FErrorCounts.TryGetValue(threadId, count) then
        count := 0;
      FErrorCounts[threadId] := count + 1;
    end;
    if not FReported.TryGetValue(threadId, reported) then
    begin
      reported := TCollections.CreateSet<string>;
      FReported.Add(threadId, reported);
    end;
    //the code is not part of the key - a message logged uncoded and then raised with a code is the
    //same problem.
    duplicate := reported.Contains(key);
    if (not duplicate) and (reported.Count < cMaxDuplicateKeys) then
      reported.Add(key);
  finally
    FLock.Leave;
  end;

  DiagnosticReported(level, code, data, duplicate);
  if not duplicate then
    WriteMessage(level, FormatDiagnostic(level = TLogLevel.Error, code, data), important);
end;

function TBaseLogger.IsEnabled(const level : TLogLevel; const important : boolean) : boolean;
begin
  case level of
    TLogLevel.Debug : result := FVerbosity >= TVerbosity.Debug;
    TLogLevel.Verbose : result := FVerbosity >= TVerbosity.Detailed;
    TLogLevel.Information,
    TLogLevel.Success : result := important or (FVerbosity >= TVerbosity.Normal);
  else
    //warnings and errors are always shown.
    result := true;
  end;
end;

procedure TBaseLogger.DiagnosticReported(const level : TLogLevel; const code : string; const data : string; const duplicate : boolean);
begin
  //for descendants that want to keep track - the test logger does.
end;

procedure TBaseLogger.DoClear;
begin
end;

procedure TBaseLogger.DoNewLine;
begin
end;

function TBaseLogger.GetVerbosity : TVerbosity;
begin
  result := FVerbosity;
end;

procedure TBaseLogger.SetVerbosity(const value : TVerbosity);
begin
  FVerbosity := value;
end;

procedure TBaseLogger.Debug(const data : string);
begin
  if IsEnabled(TLogLevel.Debug, false) then
    WriteMessage(TLogLevel.Debug, data, false);
end;

procedure TBaseLogger.Verbose(const data : string; const important : boolean);
begin
  if IsEnabled(TLogLevel.Verbose, important) then
    WriteMessage(TLogLevel.Verbose, data, important);
end;

procedure TBaseLogger.Information(const data : string; const important : boolean);
begin
  if IsEnabled(TLogLevel.Information, important) then
    WriteMessage(TLogLevel.Information, data, important);
end;

procedure TBaseLogger.Success(const data : string; const important : boolean);
begin
  if IsEnabled(TLogLevel.Success, important) then
    WriteMessage(TLogLevel.Success, data, important);
end;

procedure TBaseLogger.Warning(const data : string; const important : boolean);
begin
  Report(TLogLevel.Warning, '', data, important);
end;

procedure TBaseLogger.Warning(const code : TWarningCode; const data : string; const important : boolean);
begin
  Report(TLogLevel.Warning, WarningCodeToString(code), data, important);
end;

procedure TBaseLogger.Error(const data : string);
begin
  Report(TLogLevel.Error, '', data, true);
end;

procedure TBaseLogger.Error(const code : TErrorCode; const data : string);
begin
  Report(TLogLevel.Error, ErrorCodeToString(code), data, true);
end;

procedure TBaseLogger.Clear;
begin
  StartDiagnosticScope;
  DoClear;
end;

procedure TBaseLogger.NewLine;
begin
  DoNewLine;
end;

procedure TBaseLogger.StartDiagnosticScope;
var
  reported : ISet<string>;
begin
  FLock.Enter;
  try
    if FReported.TryGetValue(GetCurrentThreadId, reported) then
      reported.Clear;
  finally
    FLock.Leave;
  end;
end;

function TBaseLogger.ErrorCount : integer;
begin
  FLock.Enter;
  try
    if not FErrorCounts.TryGetValue(GetCurrentThreadId, result) then
      result := 0;
  finally
    FLock.Leave;
  end;
end;

{ TDemotingLogger }

constructor TDemotingLogger.Create(const target : ILogger);
begin
  inherited Create;
  FTarget := target;
end;

procedure TDemotingLogger.Clear;
begin
end;

procedure TDemotingLogger.Debug(const data : string);
begin
  FTarget.Debug(data);
end;

procedure TDemotingLogger.Error(const data : string);
begin
  NoteProblem(FormatDiagnostic(true, '', data));
  FTarget.Debug(data);
end;

procedure TDemotingLogger.Error(const code : TErrorCode; const data : string);
begin
  NoteProblem(FormatError(code, data));
  FTarget.Debug(FormatError(code, data));
end;

function TDemotingLogger.FirstProblem : string;
begin
  result := FFirstProblem;
end;

procedure TDemotingLogger.NoteProblem(const text : string);
begin
  if FFirstProblem = '' then
    FFirstProblem := text;
end;

function TDemotingLogger.ErrorCount : integer;
begin
  //nothing logged through here is an error as far as the operation is concerned.
  result := 0;
end;

function TDemotingLogger.GetVerbosity : TVerbosity;
begin
  result := FTarget.Verbosity;
end;

procedure TDemotingLogger.Information(const data : string; const important : boolean);
begin
  FTarget.Debug(data);
end;

procedure TDemotingLogger.NewLine;
begin
end;

procedure TDemotingLogger.SetVerbosity(const value : TVerbosity);
begin
end;

procedure TDemotingLogger.StartDiagnosticScope;
begin
end;

procedure TDemotingLogger.Success(const data : string; const important : boolean);
begin
  FTarget.Debug(data);
end;

procedure TDemotingLogger.Verbose(const data : string; const important : boolean);
begin
  FTarget.Debug(data);
end;

procedure TDemotingLogger.Warning(const data : string; const important : boolean);
begin
  NoteProblem(FormatDiagnostic(false, '', data));
  FTarget.Debug(data);
end;

procedure TDemotingLogger.Warning(const code : TWarningCode; const data : string; const important : boolean);
begin
  NoteProblem(FormatWarning(code, data));
  FTarget.Debug(FormatWarning(code, data));
end;

{ TContextLogger }

constructor TContextLogger.Create(const target : ILogger; const context : string);
begin
  inherited Create;
  FTarget := target;
  FContext := context;
end;

procedure TContextLogger.Clear;
begin
  FTarget.Clear;
end;

procedure TContextLogger.Debug(const data : string);
begin
  FTarget.Debug(data);
end;

procedure TContextLogger.Error(const data : string);
begin
  FTarget.Error(FContext + data);
end;

procedure TContextLogger.Error(const code : TErrorCode; const data : string);
begin
  FTarget.Error(code, FContext + data);
end;

function TContextLogger.ErrorCount : integer;
begin
  result := FTarget.ErrorCount;
end;

function TContextLogger.GetVerbosity : TVerbosity;
begin
  result := FTarget.Verbosity;
end;

procedure TContextLogger.Information(const data : string; const important : boolean);
begin
  FTarget.Information(data, important);
end;

procedure TContextLogger.NewLine;
begin
  FTarget.NewLine;
end;

procedure TContextLogger.SetVerbosity(const value : TVerbosity);
begin
  FTarget.Verbosity := value;
end;

procedure TContextLogger.StartDiagnosticScope;
begin
  FTarget.StartDiagnosticScope;
end;

procedure TContextLogger.Success(const data : string; const important : boolean);
begin
  FTarget.Success(data, important);
end;

procedure TContextLogger.Verbose(const data : string; const important : boolean);
begin
  FTarget.Verbose(data, important);
end;

procedure TContextLogger.Warning(const data : string; const important : boolean);
begin
  FTarget.Warning(FContext + data, important);
end;

procedure TContextLogger.Warning(const code : TWarningCode; const data : string; const important : boolean);
begin
  FTarget.Warning(code, FContext + data, important);
end;

{ TOperationGuard }

class function TOperationGuard.Start(const logger : ILogger; const reportCancellation : boolean) : TOperationGuard;
begin
  result.FLogger := logger;
  result.FReportCancellation := reportCancellation;
  logger.StartDiagnosticScope;
  result.FErrorMark := logger.ErrorCount;
end;

function TOperationGuard.ErrorsReported : integer;
begin
  result := FLogger.ErrorCount - FErrorMark;
end;

function TOperationGuard.Failed(const e : Exception) : boolean;
begin
  LogException(FLogger, e);
  result := false;
end;

procedure TOperationGuard.Finish(const succeeded : boolean; const cancelled : boolean; const what : string);
begin
  if cancelled then
  begin
    if FReportCancellation then
      FLogger.Warning(TWarningCode.W0004_OperationCancelled, what + ' was cancelled.', true);
    exit;
  end;
  if (not succeeded) and (ErrorsReported = 0) then
    FLogger.Error(TErrorCode.E0002_FailedWithoutDetail, what + ' failed without reporting why - please report this as a DPM bug.');
end;

end.
