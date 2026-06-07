unit DPM.IDE.EnvironmentVariableManager;

interface

uses
  Spring.Collections,
  DPM.Core.Logging,
  DPM.IDE.PathManager;

type
  // Sets/clears IDE *process* environment variables declared by packages while their design-time
  // components are loaded. Process env only - no registry, no persistence. State is rebuilt each
  // session as packages restore/load, exactly like the PATH manager.
  //
  // The variable's effect is global (one IDE process), so the (project, package) referencers exist
  // only to decide when to remove/restore it - mirroring how design BPLs are tracked in FLoadedBPLs.
  // A var lives while any open project that pulls in the declaring package is loaded; the package
  // dimension is needed because two packages can declare the same variable name.
  IDPMIDEEnvironmentVariableManager = interface
  ['{4F3E2A91-6C5D-4B8E-9F2A-1D7C6B0E54A3}']
    procedure SetVariable(const name : string; const value : string; const lcProject : string; const packageId : string);
    procedure RemoveVariable(const name : string; const value : string; const lcProject : string; const packageId : string);
  end;

  //One package-in-a-project that wants a variable set, and the value it wants.
  TEnvVarRef = record
    Referencer : string; //lcProject + '|' + LowerCase(packageId) - identity for ref counting
    PackageId : string;  //as supplied, for the conflict warning message
    Value : string;      //the value this referencer wants
  end;

  TManagedEnvVar = record
    Name : string;
    OriginalValue : string;
    OriginalExisted : boolean;
    Refs : IList<TEnvVarRef>; //in apply order. Shared (interface) across record copies.
  end;

  TDPMIDEEnvironmentVariableManager = class(TInterfacedObject, IDPMIDEEnvironmentVariableManager)
  private
    FLogger : ILogger;
    FPathManager : IDPMIDEPathManager;
    //key: uppercase variable name (Windows env var names are case-insensitive)
    FManagedVars : IDictionary<string, TManagedEnvVar>;
    function SplitPathDirs(const value : string) : TArray<string>;
    procedure WarnOnConflict(const managed : TManagedEnvVar);
  protected
    procedure SetVariable(const name : string; const value : string; const lcProject : string; const packageId : string);
    procedure RemoveVariable(const name : string; const value : string; const lcProject : string; const packageId : string);
  public
    constructor Create(const logger : ILogger; const pathManager : IDPMIDEPathManager);
  end;

implementation

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils;

{ helpers }

//Read the current process value of a variable, distinguishing "set to empty" from "not set".
function TryGetProcessEnvVar(const name : string; out value : string) : boolean;
var
  len : DWORD;
begin
  value := '';
  len := WinApi.Windows.GetEnvironmentVariable(PChar(name), nil, 0);
  if len = 0 then
  begin
    //ERROR_ENVVAR_NOT_FOUND => genuinely not set.
    result := GetLastError <> ERROR_ENVVAR_NOT_FOUND;
    exit;
  end;
  //len includes the terminating null when the buffer is too small.
  SetLength(value, len - 1);
  WinApi.Windows.GetEnvironmentVariable(PChar(name), PChar(value), len);
  result := true;
end;

procedure SetProcessEnvVar(const name, value : string);
begin
  WinApi.Windows.SetEnvironmentVariable(PChar(name), PChar(value));
end;

procedure ClearProcessEnvVar(const name : string);
begin
  WinApi.Windows.SetEnvironmentVariable(PChar(name), nil);
end;

//Stable identity for a (project, package) pair. '|' is an illegal Windows path char so it cannot
//appear in lcProject - safe separator.
function MakeReferencer(const lcProject, packageId : string) : string;
begin
  result := lcProject + '|' + LowerCase(Trim(packageId));
end;

function IndexOfRef(const refs : IList<TEnvVarRef>; const referencer : string) : integer;
var
  i : integer;
begin
  for i := 0 to refs.Count - 1 do
    if SameText(refs[i].Referencer, referencer) then
      exit(i);
  result := -1;
end;

{ TDPMIDEEnvironmentVariableManager }

constructor TDPMIDEEnvironmentVariableManager.Create(const logger : ILogger; const pathManager : IDPMIDEPathManager);
begin
  inherited Create;
  FLogger := logger;
  FPathManager := pathManager;
  FManagedVars := TCollections.CreateDictionary<string, TManagedEnvVar>;
end;

function TDPMIDEEnvironmentVariableManager.SplitPathDirs(const value : string) : TArray<string>;
var
  list : TStringList;
  i : integer;
  dir : string;
begin
  SetLength(result, 0);
  list := TStringList.Create;
  try
    list.StrictDelimiter := true;
    list.Delimiter := ';';
    list.DelimitedText := value;
    for i := 0 to list.Count - 1 do
    begin
      dir := Trim(list[i]);
      if dir <> '' then
      begin
        SetLength(result, Length(result) + 1);
        result[High(result)] := dir;
      end;
    end;
  finally
    list.Free;
  end;
end;

//Warn when the variable's referencers disagree on the value (the live value is the most recently
//applied one). Identical values never warn.
procedure TDPMIDEEnvironmentVariableManager.WarnOnConflict(const managed : TManagedEnvVar);
var
  i : integer;
  firstValue : string;
  differs : boolean;
  packages : string;
begin
  if managed.Refs.Count < 2 then
    exit;
  firstValue := managed.Refs[0].Value;
  differs := false;
  packages := '';
  for i := 0 to managed.Refs.Count - 1 do
  begin
    if managed.Refs[i].Value <> firstValue then
      differs := true;
    if packages <> '' then
      packages := packages + ', ';
    packages := packages + managed.Refs[i].PackageId;
  end;
  if differs then
    FLogger.Warning('Environment variable [' + managed.Name + '] is set to differing values by packages [' + packages + '] - using the value from the most recently loaded package.');
end;

procedure TDPMIDEEnvironmentVariableManager.SetVariable(const name : string; const value : string; const lcProject : string; const packageId : string);
var
  key : string;
  managed : TManagedEnvVar;
  dir : string;
  referencer : string;
  idx : integer;
  ref : TEnvVarRef;
begin
  //PATH is append-only and delegated to the existing path manager, which ref-counts entries and
  //protects directories that were on PATH at IDE startup.
  if SameText(Trim(name), 'PATH') then
  begin
    for dir in SplitPathDirs(value) do
      FPathManager.EnsurePath(dir);
    exit;
  end;

  key := UpperCase(Trim(name));
  if key = '' then
    exit;

  referencer := MakeReferencer(lcProject, packageId);
  ref.Referencer := referencer;
  ref.PackageId := packageId;
  ref.Value := value;

  if FManagedVars.TryGetValue(key, managed) then
  begin
    //upsert this referencer's desired value
    idx := IndexOfRef(managed.Refs, referencer);
    if idx >= 0 then
      managed.Refs[idx] := ref
    else
      managed.Refs.Add(ref);
    //latest writer wins for the live value
    SetProcessEnvVar(managed.Name, value);
    WarnOnConflict(managed);
    exit;
  end;

  managed.Name := name;
  managed.OriginalExisted := TryGetProcessEnvVar(name, managed.OriginalValue);
  managed.Refs := TCollections.CreateList<TEnvVarRef>;
  managed.Refs.Add(ref);
  FManagedVars[key] := managed;
  SetProcessEnvVar(name, value);
  FLogger.Information('Set environment variable [' + name + ']');
end;

procedure TDPMIDEEnvironmentVariableManager.RemoveVariable(const name : string; const value : string; const lcProject : string; const packageId : string);
var
  key : string;
  managed : TManagedEnvVar;
  dir : string;
  referencer : string;
  idx : integer;
begin
  if SameText(Trim(name), 'PATH') then
  begin
    for dir in SplitPathDirs(value) do
      FPathManager.RemovePath(dir);
    exit;
  end;

  key := UpperCase(Trim(name));
  if not FManagedVars.TryGetValue(key, managed) then
    exit;

  referencer := MakeReferencer(lcProject, packageId);
  idx := IndexOfRef(managed.Refs, referencer);
  if idx >= 0 then
    managed.Refs.Delete(idx);

  if managed.Refs.Count > 0 then
  begin
    //Other packages still want this variable - re-apply the most recently added survivor's value so
    //the live value belongs to a package that is still loaded (last wins).
    SetProcessEnvVar(managed.Name, managed.Refs[managed.Refs.Count - 1].Value);
    exit;
  end;

  //last referencer gone - restore the captured original, or clear it if it didn't exist before.
  if managed.OriginalExisted then
    SetProcessEnvVar(managed.Name, managed.OriginalValue)
  else
    ClearProcessEnvVar(managed.Name);
  FManagedVars.Remove(key);
  FLogger.Information('Cleared environment variable [' + managed.Name + ']');
end;

end.
