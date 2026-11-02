unit DPM.Core.Tests.IDE.Options;

// Tests for loading the IDE options file (shared by the IDE plugin and DSpecCreator). The file name is expanded
// from %APPDATA% when the options are created, so each test points APPDATA at a temp folder that does not exist
// yet - a machine nothing DPM has run on - and puts the real one back afterwards.

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TIDEOptionsTests = class
  private
    FSavedAppData : string;
    FAppData : string;
    function OptionsFileName : string;
    procedure WriteOptionsFile(const content : string);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
  published
    //DSpecCreator failed to start when there was no .dpm folder - the options load before anything else creates it.
    procedure LoadOrCreate_Creates_The_Folder_And_The_File;
    procedure LoadOrCreate_Loads_An_Existing_File;
    procedure LoadOrCreate_Keeps_The_Defaults_When_The_File_Cannot_Be_Read;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Diagnostics,
  DPM.IDE.Types,
  DPM.IDE.Options,
  TestLogger;

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

{ TIDEOptionsTests }

procedure TIDEOptionsTests.Setup;
var
  g : TGUID;
begin
  FSavedAppData := GetEnv('APPDATA');
  CreateGUID(g);
  FAppData := TPath.Combine(TPath.GetTempPath, 'dpm-ide-options-' + LowerCase(Copy(GUIDToString(g), 2, 8)));
  WinApi.Windows.SetEnvironmentVariable('APPDATA', PChar(FAppData));
end;

procedure TIDEOptionsTests.Teardown;
begin
  WinApi.Windows.SetEnvironmentVariable('APPDATA', PChar(FSavedAppData));
  if DirectoryExists(FAppData) then
    TDirectory.Delete(FAppData, true);
end;

function TIDEOptionsTests.OptionsFileName : string;
begin
  result := TPath.Combine(TPath.Combine(FAppData, '.dpm'), cDPMIDEOptionsFileName);
end;

procedure TIDEOptionsTests.WriteOptionsFile(const content : string);
begin
  ForceDirectories(ExtractFilePath(OptionsFileName));
  TFile.WriteAllText(OptionsFileName, content);
end;

procedure TIDEOptionsTests.LoadOrCreate_Creates_The_Folder_And_The_File;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  options : IDPMIDEOptions;
  reloaded : IDPMIDEOptions;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  options := TDPMIDEOptions.Create;
  Assert.AreEqual(OptionsFileName, options.FileName);
  Assert.IsFalse(DirectoryExists(ExtractFilePath(OptionsFileName)), 'the test needs the .dpm folder to be missing');

  options.LoadOrCreate(loggerIntf);

  Assert.IsTrue(FileExists(OptionsFileName), 'the options file should have been created');
  Assert.AreEqual(0, logger.WarningCount + logger.ErrorCount, logger.Messages.Text);
  //and it can be read back - raises if not.
  reloaded := TDPMIDEOptions.Create;
  reloaded.LoadFromFile();
end;

procedure TIDEOptionsTests.LoadOrCreate_Loads_An_Existing_File;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  options : IDPMIDEOptions;
begin
  //Quiet is not the default in either build.
  WriteOptionsFile('{"logverbosity":"Quiet"}');
  logger := TTestLogger.Create;
  loggerIntf := logger;
  options := TDPMIDEOptions.Create;

  options.LoadOrCreate(loggerIntf);

  Assert.AreEqual(Ord(TVerbosity.Quiet), Ord(options.LogVerbosity));
  Assert.AreEqual(0, logger.WarningCount + logger.ErrorCount, logger.Messages.Text);
end;

procedure TIDEOptionsTests.LoadOrCreate_Keeps_The_Defaults_When_The_File_Cannot_Be_Read;
const
  cBroken = '{"logverbosity":"Quiet"';
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  options : IDPMIDEOptions;
  defaults : IDPMIDEOptions;
begin
  WriteOptionsFile(cBroken);
  logger := TTestLogger.Create;
  loggerIntf := logger;
  options := TDPMIDEOptions.Create;
  defaults := TDPMIDEOptions.Create;

  options.LoadOrCreate(loggerIntf);

  Assert.IsTrue(logger.HasWarning(TWarningCode.W9108_IDEOptionsUnreadable), logger.Messages.Text);
  Assert.AreEqual(Ord(defaults.LogVerbosity), Ord(options.LogVerbosity));
  //left for whoever wants to look at it - saving the options replaces it.
  Assert.AreEqual(cBroken, TFile.ReadAllText(OptionsFileName), false);
end;

initialization
  TDUnitX.RegisterTestFixture(TIDEOptionsTests);

end.
