unit DPM.Core.Tests.IDE.PathManager;

// Headless tests for TDPMIDEPathManager. It only touches the process environment, so each test sets the
// test process's PATH and BDSCOMMONDIR to known values first (the manager reads them when it is created)
// and puts the real ones back afterwards.

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TPathManagerTests = class
  private
    FSavedPath : string;
    FSavedCommonDir : string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
  published
    //A package's bpl folder (and PATH values a package declares) never go at the front of PATH - they go
    //after everything already there when the IDE's bpl folder isn't on PATH ...
    procedure EnsurePath_Appends_After_The_Existing_Entries;
    procedure EnsurePath_Appends_Each_New_Folder_In_Turn;
    //... and in front of it when it is, so a design package finds its own runtime packages before an older
    //copy installed there by hand.
    procedure EnsurePath_Goes_In_Front_Of_The_IDE_Bpl_Folder;
    procedure EnsurePath_Finds_The_Bpl_Folder_However_It_Is_Written;
    procedure EnsurePath_Goes_In_Front_Of_The_Win64_Bpl_Folder;
    //entries are split on ';' only - 'C:\Program Files\Git\cmd' must not come back as two entries.
    procedure Entries_With_Spaces_And_Quotes_Are_Kept_As_They_Were;
    procedure Empty_Entries_Are_Dropped;
    procedure RemovePath_Removes_A_Folder_Once_Nothing_Needs_It;
    procedure Startup_Entry_Survives_Ensure_And_Remove;
    procedure Startup_Entry_Is_Not_Added_Twice;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  DPM.IDE.PathManager;

const
  cStartupPath = 'C:\Windows\system32;C:\Program Files\Git\cmd;C:\Tools';
  cPackageA = 'C:\cache\delphi13.0\VSoft.A\1.0.0\bpl\Win32';
  cPackageB = 'C:\cache\delphi13.0\VSoft.B\1.0.0\bpl\Win32';
  cCommonDir = 'C:\Users\Public\Documents\Embarcadero\Studio\37.0';
  cIDEBin = 'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin';

function GetEnv(const name : string) : string;
var
  len : DWORD;
begin
  result := '';
  len := WinApi.Windows.GetEnvironmentVariable(PChar(name), nil, 0);
  if len = 0 then
    exit;
  SetLength(result, len - 1);
  WinApi.Windows.GetEnvironmentVariable(PChar(name), PChar(result), len);
end;

procedure SetPath(const value : string);
begin
  WinApi.Windows.SetEnvironmentVariable('PATH', PChar(value));
end;

{ TPathManagerTests }

procedure TPathManagerTests.Setup;
begin
  FSavedPath := GetEnv('PATH');
  FSavedCommonDir := GetEnv('BDSCOMMONDIR');
  SetPath(cStartupPath);
  //as the IDE sets it - its Bpl folder is not on cStartupPath.
  WinApi.Windows.SetEnvironmentVariable('BDSCOMMONDIR', cCommonDir);
end;

procedure TPathManagerTests.Teardown;
begin
  SetPath(FSavedPath);
  if FSavedCommonDir <> '' then
    WinApi.Windows.SetEnvironmentVariable('BDSCOMMONDIR', PChar(FSavedCommonDir))
  else
    WinApi.Windows.SetEnvironmentVariable('BDSCOMMONDIR', nil);
end;

procedure TPathManagerTests.EnsurePath_Appends_After_The_Existing_Entries;
var
  manager : IDPMIDEPathManager;
begin
  manager := TDPMIDEPathManager.Create;
  manager.EnsurePath(cPackageA);
  Assert.AreEqual(cStartupPath + ';' + cPackageA, GetEnv('PATH'));
end;

procedure TPathManagerTests.EnsurePath_Appends_Each_New_Folder_In_Turn;
var
  manager : IDPMIDEPathManager;
begin
  manager := TDPMIDEPathManager.Create;
  manager.EnsurePath(cPackageA);
  manager.EnsurePath(cPackageB);
  Assert.AreEqual(cStartupPath + ';' + cPackageA + ';' + cPackageB, GetEnv('PATH'));
end;

procedure TPathManagerTests.EnsurePath_Goes_In_Front_Of_The_IDE_Bpl_Folder;
var
  manager : IDPMIDEPathManager;
begin
  //the order the RAD Studio installer leaves PATH in.
  SetPath('C:\Windows\system32;' + cIDEBin + ';' + cCommonDir + '\Bpl;C:\Program Files\Git\cmd');
  manager := TDPMIDEPathManager.Create;
  manager.EnsurePath(cPackageA);
  manager.EnsurePath(cPackageB);
  Assert.AreEqual('C:\Windows\system32;' + cIDEBin + ';' + cPackageA + ';' + cPackageB + ';' + cCommonDir + '\Bpl;C:\Program Files\Git\cmd', GetEnv('PATH'));
  manager.RemovePath(cPackageA);
  Assert.AreEqual('C:\Windows\system32;' + cIDEBin + ';' + cPackageB + ';' + cCommonDir + '\Bpl;C:\Program Files\Git\cmd', GetEnv('PATH'));
end;

procedure TPathManagerTests.EnsurePath_Finds_The_Bpl_Folder_However_It_Is_Written;
var
  manager : IDPMIDEPathManager;
  bplFolder : string;
begin
  //a different case, a trailing delimiter, quotes.
  bplFolder := '"' + UpperCase(cCommonDir) + '\bpl\"';
  SetPath('C:\Windows\system32;' + bplFolder);
  manager := TDPMIDEPathManager.Create;
  manager.EnsurePath(cPackageA);
  Assert.AreEqual('C:\Windows\system32;' + cPackageA + ';' + bplFolder, GetEnv('PATH'));
end;

procedure TPathManagerTests.EnsurePath_Goes_In_Front_Of_The_Win64_Bpl_Folder;
var
  manager : IDPMIDEPathManager;
begin
  SetPath('C:\Windows\system32;' + cCommonDir + '\Bpl\Win64;C:\Tools');
  manager := TDPMIDEPathManager.Create;
  manager.EnsurePath(cPackageA);
  Assert.AreEqual('C:\Windows\system32;' + cPackageA + ';' + cCommonDir + '\Bpl\Win64;C:\Tools', GetEnv('PATH'));
end;

procedure TPathManagerTests.Entries_With_Spaces_And_Quotes_Are_Kept_As_They_Were;
const
  cQuoted = 'C:\a;"C:\Program Files\b";C:\Program Files (x86)\c';
var
  manager : IDPMIDEPathManager;
begin
  SetPath(cQuoted);
  manager := TDPMIDEPathManager.Create;
  manager.EnsurePath(cPackageA);
  Assert.AreEqual(cQuoted + ';' + cPackageA, GetEnv('PATH'));
  manager.RemovePath(cPackageA);
  Assert.AreEqual(cQuoted, GetEnv('PATH'));
end;

procedure TPathManagerTests.Empty_Entries_Are_Dropped;
var
  manager : IDPMIDEPathManager;
begin
  SetPath('C:\a;;C:\b;');
  manager := TDPMIDEPathManager.Create;
  manager.EnsurePath(cPackageA);
  Assert.AreEqual('C:\a;C:\b;' + cPackageA, GetEnv('PATH'));
end;

procedure TPathManagerTests.RemovePath_Removes_A_Folder_Once_Nothing_Needs_It;
var
  manager : IDPMIDEPathManager;
begin
  manager := TDPMIDEPathManager.Create;
  manager.EnsurePath(cPackageA);
  manager.EnsurePath(cPackageB);
  //a second design package from the same folder
  manager.EnsurePath(cPackageA);

  manager.RemovePath(cPackageA);
  Assert.AreEqual(cStartupPath + ';' + cPackageA + ';' + cPackageB, GetEnv('PATH'), 'still needed by the other design package');
  manager.RemovePath(cPackageA);
  Assert.AreEqual(cStartupPath + ';' + cPackageB, GetEnv('PATH'));
  manager.RemovePath(cPackageB);
  Assert.AreEqual(cStartupPath, GetEnv('PATH'));
end;

procedure TPathManagerTests.Startup_Entry_Survives_Ensure_And_Remove;
var
  manager : IDPMIDEPathManager;
begin
  manager := TDPMIDEPathManager.Create;
  //a package declares a folder that was already on PATH when the IDE started
  manager.EnsurePath('C:\Tools');
  manager.RemovePath('C:\Tools');
  Assert.AreEqual(cStartupPath, GetEnv('PATH'));
end;

procedure TPathManagerTests.Startup_Entry_Is_Not_Added_Twice;
var
  manager : IDPMIDEPathManager;
begin
  manager := TDPMIDEPathManager.Create;
  manager.EnsurePath('c:\windows\SYSTEM32');
  Assert.AreEqual(cStartupPath, GetEnv('PATH'), 'already there (case insensitive) - stays where it was');
end;

initialization
  TDUnitX.RegisterTestFixture(TPathManagerTests);

end.
