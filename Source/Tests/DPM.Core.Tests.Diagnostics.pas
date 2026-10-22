unit DPM.Core.Tests.Diagnostics;

interface

uses
  DUnitX.TestFramework;

type
  //The error/warning code catalogue (DPM.Core.Diagnostics) and the shared logger behaviour that
  //keeps a failure down to one reported cause (TBaseLogger, TOperationGuard).
  [TestFixture]
  TDiagnosticsTests = class
  public
    [Test]
    procedure Code_Enums_Are_Four_Bytes;
    [Test]
    procedure Identifiers_Carry_Their_Number;
    [Test]
    procedure Numbers_Are_Unique_And_In_Order;
    [Test]
    procedure Every_Code_Has_A_Title;
    [Test]
    procedure Codes_Are_Formatted;
    [Test]
    procedure Docs_List_Every_Code;

    [Test]
    procedure Logged_Lines_Carry_The_Code;
    [Test]
    procedure Duplicate_Is_Shown_Once_But_Counted;
    [Test]
    procedure Clear_Resets_Duplicates_But_Not_Counts;
    [Test]
    procedure Another_Threads_Errors_Are_Not_Counted;
    [Test]
    procedure Another_Threads_Scope_Does_Not_Reset_This_One;

    [Test]
    procedure Guard_Reports_A_Silent_Failure;
    [Test]
    procedure Guard_Adds_Nothing_When_The_Cause_Was_Reported;
    [Test]
    procedure Guard_Adds_Nothing_On_Success;
    [Test]
    procedure Guard_Starts_A_New_Duplicate_Scope;
    [Test]
    procedure Guard_Reports_Cancellation_Only_When_Asked;
    [Test]
    procedure LogException_Keeps_An_EDPMError_Code;
    [Test]
    procedure LogException_Reports_Other_Exceptions_As_Unexpected;
    [Test]
    procedure Demoting_Logger_Does_Not_Count_Errors;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.TypInfo,
  System.RegularExpressions,
  Spring.Collections,
  DPM.Core.Diagnostics,
  DPM.Core.Logging,
  TestLogger;

type
  //runs a procedure on a thread of its own and waits for it.
  TActionThread = class(TThread)
  private
    FAction : TProc;
  protected
    procedure Execute; override;
  public
    constructor Create(const action : TProc);
  end;

constructor TActionThread.Create(const action : TProc);
begin
  inherited Create(true);
  FAction := action;
end;

procedure TActionThread.Execute;
begin
  FAction();
end;

procedure RunOnThread(const action : TProc);
var
  thread : TActionThread;
begin
  thread := TActionThread.Create(action);
  try
    thread.Start;
    thread.WaitFor;
  finally
    thread.Free;
  end;
end;

//'E1101_PackageNotFound' -> 1101; -1 when the identifier isn't shaped like that.
function NumberInIdentifier(const name : string; const letter : char) : integer;
begin
  result := -1;
  if not TRegEx.IsMatch(name, '^' + letter + '\d{4}_\w+$') then
    exit;
  result := StrToInt(Copy(name, 2, 4));
end;

function FindErrorCodesDoc : string;
var
  folder : string;
  parent : string;
begin
  folder := ExtractFileDir(ParamStr(0));
  while folder <> '' do
  begin
    result := TPath.Combine(folder, 'docs\error-codes.md');
    if FileExists(result) then
      exit;
    parent := ExtractFileDir(folder);
    if SameText(parent, folder) then
      break;
    folder := parent;
  end;
  result := '';
end;

function CountLines(const lines : TStrings; const line : string) : integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to lines.Count - 1 do
    if lines[i] = line then
      Inc(result);
end;

{ TDiagnosticsTests }

procedure TDiagnosticsTests.Code_Enums_Are_Four_Bytes;
begin
  //{$MINENUMSIZE 4} in DPM.Core.Diagnostics - the catalogues will outgrow a byte.
  Assert.AreEqual(4, SizeOf(TErrorCode), 'TErrorCode');
  Assert.AreEqual(4, SizeOf(TWarningCode), 'TWarningCode');
end;

procedure TDiagnosticsTests.Identifiers_Carry_Their_Number;
var
  errorCode : TErrorCode;
  warningCode : TWarningCode;
  name : string;
begin
  for errorCode := Low(TErrorCode) to High(TErrorCode) do
  begin
    name := GetEnumName(TypeInfo(TErrorCode), Ord(errorCode));
    Assert.AreEqual(cErrorInfo[errorCode].Number, NumberInIdentifier(name, 'E'),
      name + ' - the identifier must be E<4 digits>_Name, matching its cErrorInfo number (is cErrorInfo out of order?)');
  end;
  for warningCode := Low(TWarningCode) to High(TWarningCode) do
  begin
    name := GetEnumName(TypeInfo(TWarningCode), Ord(warningCode));
    Assert.AreEqual(cWarningInfo[warningCode].Number, NumberInIdentifier(name, 'W'),
      name + ' - the identifier must be W<4 digits>_Name, matching its cWarningInfo number (is cWarningInfo out of order?)');
  end;
end;

procedure TDiagnosticsTests.Numbers_Are_Unique_And_In_Order;
var
  used : IDictionary<integer, string>;
  errorCode : TErrorCode;
  warningCode : TWarningCode;
  previous : integer;
  number : integer;
  name : string;
begin
  used := TCollections.CreateDictionary<integer, string>;
  previous := 0;
  for errorCode := Low(TErrorCode) to High(TErrorCode) do
  begin
    number := cErrorInfo[errorCode].Number;
    name := ErrorCodeToString(errorCode);
    Assert.IsTrue((number > 0) and (number <= 9999), name + ' is out of range');
    Assert.IsTrue(number > previous, name + ' is out of order - keep TErrorCode in number order');
    if used.ContainsKey(number) then
      Assert.Fail(name + ' reuses the number of ' + used[number]);
    used.Add(number, name);
    previous := number;
  end;
  previous := 0;
  for warningCode := Low(TWarningCode) to High(TWarningCode) do
  begin
    number := cWarningInfo[warningCode].Number;
    name := WarningCodeToString(warningCode);
    Assert.IsTrue((number > 0) and (number <= 9999), name + ' is out of range');
    Assert.IsTrue(number > previous, name + ' is out of order - keep TWarningCode in number order');
    //errors and warnings share one number space.
    if used.ContainsKey(number) then
      Assert.Fail(name + ' reuses the number of ' + used[number]);
    used.Add(number, name);
    previous := number;
  end;
end;

procedure TDiagnosticsTests.Every_Code_Has_A_Title;
var
  errorCode : TErrorCode;
  warningCode : TWarningCode;
begin
  for errorCode := Low(TErrorCode) to High(TErrorCode) do
    Assert.IsTrue(Trim(cErrorInfo[errorCode].Title) <> '', ErrorCodeToString(errorCode) + ' has no title');
  for warningCode := Low(TWarningCode) to High(TWarningCode) do
    Assert.IsTrue(Trim(cWarningInfo[warningCode].Title) <> '', WarningCodeToString(warningCode) + ' has no title');
end;

procedure TDiagnosticsTests.Codes_Are_Formatted;
begin
  Assert.AreEqual('DPME0001', ErrorCodeToString(TErrorCode.E0001_UnexpectedException), false);
  Assert.AreEqual('DPMW0004', WarningCodeToString(TWarningCode.W0004_OperationCancelled), false);
  Assert.AreEqual('error DPME0003: boom', FormatError(TErrorCode.E0003_InternalError, 'boom'), false);
  Assert.AreEqual('warning DPMW0004: stopped', FormatWarning(TWarningCode.W0004_OperationCancelled, 'stopped'), false);
  Assert.AreEqual('error: not coded yet', FormatDiagnostic(true, '', 'not coded yet'), false);
  Assert.AreEqual('warning: not coded yet', FormatDiagnostic(false, '', 'not coded yet'), false);
end;

procedure TDiagnosticsTests.Docs_List_Every_Code;
var
  docFile : string;
  doc : string;
  documented : ISet<string>;
  match : TMatch;
  errorCode : TErrorCode;
  warningCode : TWarningCode;
  known : ISet<string>;
  code : string;
begin
  docFile := FindErrorCodesDoc;
  if docFile = '' then
  begin
    Assert.Pass('docs\error-codes.md was not found above ' + ExtractFileDir(ParamStr(0)) + ' - not checked');
    exit;
  end;
  doc := TFile.ReadAllText(docFile, TEncoding.UTF8);

  documented := TCollections.CreateSet<string>;
  for match in TRegEx.Matches(doc, 'DPM[EW]\d{4}') do
    documented.Add(match.Value);

  known := TCollections.CreateSet<string>;
  for errorCode := Low(TErrorCode) to High(TErrorCode) do
    known.Add(ErrorCodeToString(errorCode));
  for warningCode := Low(TWarningCode) to High(TWarningCode) do
    known.Add(WarningCodeToString(warningCode));

  for code in known do
    Assert.IsTrue(documented.Contains(code), code + ' is not documented in ' + docFile);
  for code in documented do
    Assert.IsTrue(known.Contains(code), docFile + ' documents ' + code + ', which DPM.Core.Diagnostics does not define');
end;

procedure TDiagnosticsTests.Logged_Lines_Carry_The_Code;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'coded error');
  loggerIntf.Warning(TWarningCode.W0004_OperationCancelled, 'coded warning');
  loggerIntf.Error('uncoded error');
  loggerIntf.Warning('uncoded warning');
  Assert.AreEqual(4, logger.Messages.Count, logger.Messages.Text);
  Assert.AreEqual('error: DPME0003: coded error', logger.Messages[0], false);
  Assert.AreEqual('warning: DPMW0004: coded warning', logger.Messages[1], false);
  Assert.AreEqual('error: uncoded error', logger.Messages[2], false);
  Assert.AreEqual('warning: uncoded warning', logger.Messages[3], false);
  Assert.IsTrue(logger.HasError(TErrorCode.E0003_InternalError));
  Assert.IsTrue(logger.HasWarning(TWarningCode.W0004_OperationCancelled));
  Assert.AreEqual(2, loggerIntf.ErrorCount);
  Assert.AreEqual(2, logger.WarningCount);
end;

procedure TDiagnosticsTests.Duplicate_Is_Shown_Once_But_Counted;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'same problem');
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'same problem');
  //logged uncoded and then raised with a code - still the same problem.
  loggerIntf.Error('same problem');
  loggerIntf.Warning(TWarningCode.W0004_OperationCancelled, 'same warning');
  loggerIntf.Warning(TWarningCode.W0004_OperationCancelled, 'same warning');
  //a different level is a different line.
  loggerIntf.Warning('same problem');

  Assert.AreEqual(1, CountLines(logger.Messages, 'error: DPME0003: same problem'), logger.Messages.Text);
  Assert.AreEqual(1, CountLines(logger.Messages, 'warning: DPMW0004: same warning'), logger.Messages.Text);
  Assert.AreEqual(1, CountLines(logger.Messages, 'warning: same problem'), logger.Messages.Text);
  Assert.AreEqual(3, logger.Messages.Count, logger.Messages.Text);
  //duplicates still count, so a test sees a structural repeat even though the user does not.
  Assert.AreEqual(3, loggerIntf.ErrorCount);
  Assert.AreEqual(3, logger.WarningCount);
end;

procedure TDiagnosticsTests.Clear_Resets_Duplicates_But_Not_Counts;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'again');
  loggerIntf.Clear;
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'again');
  Assert.AreEqual(1, logger.Messages.Count, 'shown again after Clear : ' + logger.Messages.Text);
  Assert.AreEqual(2, loggerIntf.ErrorCount, 'Clear must not reset the error count - marks taken before it stay valid');
end;

procedure TDiagnosticsTests.Another_Threads_Errors_Are_Not_Counted;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  countOnThread : integer;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'on the test thread');
  countOnThread := -1;
  RunOnThread(
    procedure
    begin
      loggerIntf.Error(TErrorCode.E0003_InternalError, 'on another thread');
      countOnThread := loggerIntf.ErrorCount;
    end);
  Assert.AreEqual(1, countOnThread, 'the other thread sees its own error only');
  Assert.AreEqual(1, loggerIntf.ErrorCount, 'an error on another thread must not satisfy this thread''s guard');
  Assert.IsTrue(logger.Logged('error', 'on another thread'), 'it is still logged');
end;

procedure TDiagnosticsTests.Another_Threads_Scope_Does_Not_Reset_This_One;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'repeated');
  RunOnThread(
    procedure
    begin
      loggerIntf.StartDiagnosticScope;
    end);
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'repeated');
  Assert.AreEqual(1, CountLines(logger.Messages, 'error: DPME0003: repeated'), logger.Messages.Text);
end;

procedure TDiagnosticsTests.Guard_Reports_A_Silent_Failure;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  guard : TOperationGuard;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  guard := TOperationGuard.Start(loggerIntf);
  guard.Finish(false, false, 'Restore');
  Assert.IsTrue(logger.HasError(TErrorCode.E0002_FailedWithoutDetail), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'Restore failed'), logger.Messages.Text);
  Assert.AreEqual(1, loggerIntf.ErrorCount);
end;

procedure TDiagnosticsTests.Guard_Adds_Nothing_When_The_Cause_Was_Reported;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  guard : TOperationGuard;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //an error from an earlier operation must not count for this one.
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'earlier operation');
  guard := TOperationGuard.Start(loggerIntf);
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'the cause');
  Assert.AreEqual(1, guard.ErrorsReported);
  guard.Finish(false, false, 'Restore');
  Assert.IsFalse(logger.HasError(TErrorCode.E0002_FailedWithoutDetail), logger.Messages.Text);
  Assert.AreEqual(2, loggerIntf.ErrorCount);
end;

procedure TDiagnosticsTests.Guard_Adds_Nothing_On_Success;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  guard : TOperationGuard;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  guard := TOperationGuard.Start(loggerIntf);
  guard.Finish(true, false, 'Restore');
  Assert.AreEqual(0, loggerIntf.ErrorCount);
  Assert.AreEqual(0, logger.WarningCount);
end;

procedure TDiagnosticsTests.Guard_Starts_A_New_Duplicate_Scope;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  guard : TOperationGuard;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //the same failure in two operations (two packs, two compilers in a test run) shows in both.
  guard := TOperationGuard.Start(loggerIntf);
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'same failure');
  guard.Finish(false, false, 'First');
  guard := TOperationGuard.Start(loggerIntf);
  loggerIntf.Error(TErrorCode.E0003_InternalError, 'same failure');
  guard.Finish(false, false, 'Second');
  Assert.AreEqual(2, CountLines(logger.Messages, 'error: DPME0003: same failure'), logger.Messages.Text);
end;

procedure TDiagnosticsTests.Guard_Reports_Cancellation_Only_When_Asked;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  guard : TOperationGuard;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //an inner boundary (an installer entry point) leaves cancellation to the outer one.
  guard := TOperationGuard.Start(loggerIntf);
  guard.Finish(false, true, 'Install');
  Assert.AreEqual(0, loggerIntf.ErrorCount, 'cancelled is not a failure without a cause');
  Assert.AreEqual(0, logger.WarningCount);

  guard := TOperationGuard.Start(loggerIntf, true);
  guard.Finish(false, true, 'Install');
  Assert.AreEqual(0, loggerIntf.ErrorCount, 'cancelled is never an error');
  Assert.IsTrue(logger.HasWarning(TWarningCode.W0004_OperationCancelled), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('warning', 'Install was cancelled'), logger.Messages.Text);
end;

procedure TDiagnosticsTests.LogException_Keeps_An_EDPMError_Code;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  guard : TOperationGuard;
  e : Exception;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  guard := TOperationGuard.Start(loggerIntf);
  e := EDPMError.Create(TErrorCode.E0003_InternalError, 'with a code');
  try
    Assert.IsFalse(guard.Failed(e));
  finally
    e.Free;
  end;
  Assert.AreEqual('error: DPME0003: with a code', logger.Messages[0], false);
  Assert.AreEqual(1, guard.ErrorsReported);
end;

procedure TDiagnosticsTests.LogException_Reports_Other_Exceptions_As_Unexpected;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  e : Exception;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  e := Exception.Create('plain');
  try
    LogException(loggerIntf, e);
  finally
    e.Free;
  end;
  e := EArgumentException.Create('bad argument');
  try
    LogException(loggerIntf, e);
  finally
    e.Free;
  end;
  Assert.AreEqual('error: DPME0001: plain', logger.Messages[0], false);
  Assert.AreEqual('error: DPME0001: EArgumentException : bad argument', logger.Messages[1], false);
end;

procedure TDiagnosticsTests.Demoting_Logger_Does_Not_Count_Errors;
var
  target : TTestLogger;
  targetIntf : ILogger;
  demoting : ILogger;
begin
  target := TTestLogger.Create;
  targetIntf := target;
  demoting := TDemotingLogger.Create(targetIntf);
  demoting.Error(TErrorCode.E0003_InternalError, 'trial failed');
  demoting.Warning(TWarningCode.W0004_OperationCancelled, 'trial warned');
  Assert.AreEqual(0, demoting.ErrorCount);
  Assert.AreEqual(0, targetIntf.ErrorCount, 'a trial''s errors are not the operation''s');
  Assert.AreEqual('debug: error DPME0003: trial failed', target.Messages[0], false);
  Assert.AreEqual('debug: warning DPMW0004: trial warned', target.Messages[1], false);
end;

initialization
  TDUnitX.RegisterTestFixture(TDiagnosticsTests);

end.
