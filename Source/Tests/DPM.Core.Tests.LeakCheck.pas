unit DPM.Core.Tests.LeakCheck;

// With --leakcheck a run that leaves memory allocated fails (exit code 2), so a leak breaks the build instead of
// waiting for someone to read the shutdown report.
//
// This unit is listed first in DPM.Core.Tests.dpr and uses nothing that allocates, so it is initialised before - and
// finalised after - every other unit. Whatever is still allocated when it finalises was allocated during the run
// and never freed, less the blocks registered as expected leaks (Spring4D registers a few on purpose) - the same
// "unexpected" leaks the memory manager's own report (which names the classes) lists after it on stderr.

interface

const
  cExitMemoryLeaked = 2;

type
  TLeakCheck = class
  private
    class var
      FEnabled : boolean;
  public
    class property Enabled : boolean read FEnabled write FEnabled;
  end;

implementation

uses
  Winapi.Windows;

var
  originalMemoryManager : TMemoryManagerEx;
  baselineBlocks : Int64;
  //blocks registered with RegisterExpectedMemoryLeak (less those unregistered) since the baseline.
  expectedLeaks : integer;

function GetAllocatedBlocks : Int64;
var
  state : TMemoryManagerState;
  i : integer;
begin
  GetMemoryManagerState(state);
  result := Int64(state.AllocatedMediumBlockCount) + state.AllocatedLargeBlockCount;
  for i := Low(state.SmallBlockTypeStates) to High(state.SmallBlockTypeStates) do
    Inc(result, state.SmallBlockTypeStates[i].AllocatedBlockCount);
end;

//Only the expected leak registration is hooked - allocation itself still goes straight to the original functions.
function RegisterExpectedLeak(P : Pointer) : boolean;
begin
  result := originalMemoryManager.RegisterExpectedMemoryLeak(P);
  if result then
    InterlockedIncrement(expectedLeaks);
end;

function UnregisterExpectedLeak(P : Pointer) : boolean;
begin
  result := originalMemoryManager.UnregisterExpectedMemoryLeak(P);
  if result then
    InterlockedDecrement(expectedLeaks);
end;

procedure HookExpectedLeaks;
var
  hooked : TMemoryManagerEx;
begin
  GetMemoryManager(originalMemoryManager);
  hooked := originalMemoryManager;
  hooked.RegisterExpectedMemoryLeak := RegisterExpectedLeak;
  hooked.UnregisterExpectedMemoryLeak := UnregisterExpectedLeak;
  SetMemoryManager(hooked);
end;

procedure CheckForLeaks;
var
  leaked : Int64;
begin
  leaked := GetAllocatedBlocks - baselineBlocks - expectedLeaks;
  if leaked <= 0 then
    exit;
  //SysUtils is finalised by now - System's text I/O is all there is, and a failed write must not raise.
  {$I-}
  Writeln(ErrOutput, 'Leak check failed : ', leaked, ' blocks are still allocated at shutdown - the memory manager''s report follows.');
  {$I+}
  IOResult;
  ReportMemoryLeaksOnShutdown := true;
  //a failed test already set the exit code - that says more than the leak does.
  if ExitCode = 0 then
    ExitCode := cExitMemoryLeaked;
end;

initialization
  HookExpectedLeaks;
  baselineBlocks := GetAllocatedBlocks;

finalization
  SetMemoryManager(originalMemoryManager);
  if TLeakCheck.Enabled then
    CheckForLeaks;

end.
