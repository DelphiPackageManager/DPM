unit DPM.IDE.PathManager;

interface

uses
  System.Classes;

type
  IDPMIDEPathManager = interface
  ['{31ADC6E0-7B8E-4203-A635-7D02FC4C0FC7}']
    procedure RemovePath(const APath : string);
    procedure EnsurePath(const APath: string);
  end;

  TDPMIDEPathManager = class(TInterfacedObject, IDPMIDEPathManager)
  private
    FPaths : TStringList;
    //the IDE's own bpl folders - $(BDSCOMMONDIR)\Bpl and \Bpl\Win64, normalised.
    FBplFolders : TStringList;
    procedure UpdatePath;
    function InsertIndex : integer;
  protected
    procedure RemovePath(const APath : string);
    procedure EnsurePath(const APath: string);
  public
    constructor Create;
    destructor Destroy;override;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Utils.Strings;

//A PATH entry as a folder to compare - unquoted, without a trailing delimiter.
function NormaliseFolder(const value : string) : string;
begin
  result := Trim(value);
  if (Length(result) >= 2) and (result[1] = '"') and (result[Length(result)] = '"') then
    result := Copy(result, 2, Length(result) - 2);
  result := ExcludeTrailingPathDelimiter(result);
end;

{ TDPMIDEPathManager }

constructor TDPMIDEPathManager.Create;
var
  entries : TArray<string>;
  commonDir : string;
  i: Integer;
begin
  FPaths := TStringList.Create;
  FPaths.CaseSensitive := false;
  FPaths.Sorted := false;
  //Split on ';' only, and keep each entry as it was - DelimitedText also splits on spaces (so
  //'C:\Program Files\Git\cmd' became two entries) and rewrites quoted ones.
  entries := TStringUtils.SplitStr(GetEnvironmentVariable('PATH'), ';', TSplitStringOptions.ExcludeEmpty);
  for i := 0 to Length(entries) -1 do
    FPaths.AddObject(entries[i], TObject(1)); //set the reference count for the path entry so we don't remove ones that
                                              // that were there on startup.

  FBplFolders := TStringList.Create;
  FBplFolders.CaseSensitive := false;
  //the IDE sets it - the public folder packages installed by hand are built into.
  commonDir := GetEnvironmentVariable('BDSCOMMONDIR');
  if commonDir <> '' then
  begin
    FBplFolders.Add(NormaliseFolder(IncludeTrailingPathDelimiter(commonDir) + 'Bpl'));
    FBplFolders.Add(NormaliseFolder(IncludeTrailingPathDelimiter(commonDir) + 'Bpl\Win64'));
  end;
end;

destructor TDPMIDEPathManager.Destroy;
begin
  FBplFolders.Free;
  FPaths.Free;
  inherited;
end;


procedure SetEnvironmentVariable(const Name, Value: string);
begin
  WinApi.Windows.SetEnvironmentVariable(PChar(Name), PChar(Value));
end;

procedure TDPMIDEPathManager.UpdatePath;
var
  value : string;
  i : integer;
begin
  value := '';
  for i := 0 to FPaths.Count - 1 do
  begin
    if i > 0 then
      value := value + ';';
    value := value + FPaths[i];
  end;
  SetEnvironmentVariable('PATH', value);
end;

//In front of the IDE's bpl folder when it is on PATH - packages installed by hand are built into it, and a
//design package DPM loads has to find its own runtime packages before an older copy there. Otherwise at
//the end. Never at the front of PATH: a package's bpl folder can hold any file its copyToBin entries name.
//(git and dpm are run by their full paths, so a git.cmd or dpm.bat there can't stand in for them.)
function TDPMIDEPathManager.InsertIndex : integer;
var
  i : integer;
begin
  for i := 0 to FPaths.Count - 1 do
  begin
    if FBplFolders.IndexOf(NormaliseFolder(FPaths[i])) >= 0 then
      exit(i);
  end;
  result := FPaths.Count;
end;

procedure TDPMIDEPathManager.EnsurePath(const APath: string);
var
  i : integer;
  count : integer;
begin
  i := FPaths.IndexOf(APath);
  if i = -1 then
  begin
    FPaths.InsertObject(InsertIndex, APath, TObject(1));
    UpdatePath;
  end
  else
  begin
    //already there, so just increment the reference count;
    count := Integer(FPaths.Objects[i]);
    Inc(count);
    FPaths.Objects[i] := TObject(count);
  end;
end;

procedure TDPMIDEPathManager.RemovePath(const APath: string);
var
  i : integer;
  count : integer;
begin
  i := FPaths.IndexOf(APath);
  if i = -1 then
    exit;
  count := Integer(FPaths.Objects[i]);
  if count > 1 then
  begin
    Dec(count);
    FPaths.Objects[i] := TObject(count);
   end
  else
  begin
    FPaths.Delete(i);
    UpdatePath;
  end;
end;

end.
