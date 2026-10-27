unit DPM.Core.Tests.Sources.Manager;

// dpm sources update changes what it is given and nothing else, and what it saves is a source DPM can use.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSourcesManagerTests = class
  public
    //update set every field from the command line - what wasn't given was cleared (the url, the credentials).
    [Test]
    procedure Update_Of_The_Password_Keeps_The_Rest;
    [Test]
    procedure Update_Of_The_Url_Keeps_The_Credentials;
    [Test]
    procedure Update_To_An_Invalid_Url_Is_Refused;
    //--type was never applied.
    [Test]
    procedure Update_Of_The_Type_Is_Applied;
    //what the url says it is, as for add.
    [Test]
    procedure Update_To_A_Git_Url_Makes_It_A_Git_Registry;
    [Test]
    procedure Update_To_A_File_Uri_For_A_Server_Is_Refused;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.Diagnostics,
  DPM.Core.Logging,
  DPM.Core.Options.Sources,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Sources.Interfaces,
  DPM.Core.Sources.Manager,
  TestLogger;

type
  //the configuration, in memory - saving it counts.
  TMemoryConfigManager = class(TInterfacedObject, IConfigurationManager)
  private
    FConfig : IConfiguration;
    FSaveCount : integer;
  protected
    function LoadConfig(const configFile : string) : IConfiguration;
    function NewConfig : IConfiguration;
    function EnsureDefaultConfig : boolean;
    function SaveConfig(const configuration : IConfiguration; const fileName : string = '') : boolean;
  public
    constructor Create(const config : IConfiguration);
    property SaveCount : integer read FSaveCount;
  end;

{ TMemoryConfigManager }

constructor TMemoryConfigManager.Create(const config : IConfiguration);
begin
  inherited Create;
  FConfig := config;
end;

function TMemoryConfigManager.EnsureDefaultConfig : boolean;
begin
  result := true;
end;

function TMemoryConfigManager.LoadConfig(const configFile : string) : IConfiguration;
begin
  result := FConfig;
end;

function TMemoryConfigManager.NewConfig : IConfiguration;
begin
  result := FConfig;
end;

function TMemoryConfigManager.SaveConfig(const configuration : IConfiguration; const fileName : string) : boolean;
begin
  Inc(FSaveCount);
  result := true;
end;

{ helpers }

//one source, Gallery - a DPM server with credentials.
function GalleryConfig : IConfiguration;
begin
  result := TConfiguration.Create(TTestLogger.Create);
  result.Sources.Add(TSourceConfig.Create('Gallery', 'https://example.invalid/index.json', TSourceType.DPMServer, 'someone', 'secret', true));
end;

function UpdateOptions : TSourcesOptions;
begin
  result := TSourcesOptions.Create;
  result.ConfigFile := 'dpm.config.yaml';
  result.Name := 'Gallery';
end;

function Gallery(const config : IConfiguration) : ISourceConfig;
begin
  result := config.Sources[0];
end;

{ TSourcesManagerTests }

procedure TSourcesManagerTests.Update_Of_The_Password_Keeps_The_Rest;
var
  config : IConfiguration;
  configManager : IConfigurationManager;
  manager : ISourcesManager;
  options : TSourcesOptions;
begin
  config := GalleryConfig;
  configManager := TMemoryConfigManager.Create(config);
  manager := TSourcesManager.Create(TTestLogger.Create, configManager);
  options := UpdateOptions;
  try
    options.Password := 'new secret';
    Assert.IsTrue(manager.UpdateSource(options));
  finally
    options.Free;
  end;
  Assert.AreEqual('new secret', Gallery(config).Password);
  Assert.AreEqual('https://example.invalid/index.json', Gallery(config).Source);
  Assert.AreEqual('someone', Gallery(config).UserName);
  Assert.IsTrue(Gallery(config).SourceType = TSourceType.DPMServer);
end;

procedure TSourcesManagerTests.Update_Of_The_Url_Keeps_The_Credentials;
var
  config : IConfiguration;
  configManager : IConfigurationManager;
  manager : ISourcesManager;
  options : TSourcesOptions;
begin
  config := GalleryConfig;
  configManager := TMemoryConfigManager.Create(config);
  manager := TSourcesManager.Create(TTestLogger.Create, configManager);
  options := UpdateOptions;
  try
    options.Source := 'https://mirror.example.invalid/index.json';
    Assert.IsTrue(manager.UpdateSource(options));
  finally
    options.Free;
  end;
  Assert.AreEqual('https://mirror.example.invalid/index.json', Gallery(config).Source);
  Assert.AreEqual('someone', Gallery(config).UserName);
  Assert.AreEqual('secret', Gallery(config).Password);
end;

procedure TSourcesManagerTests.Update_To_An_Invalid_Url_Is_Refused;
var
  config : IConfiguration;
  memory : TMemoryConfigManager;
  configManager : IConfigurationManager;
  logger : TTestLogger;
  loggerIntf : ILogger;
  manager : ISourcesManager;
  options : TSourcesOptions;
  updated : boolean;
begin
  config := GalleryConfig;
  memory := TMemoryConfigManager.Create(config);
  configManager := memory;
  logger := TTestLogger.Create;
  loggerIntf := logger;
  manager := TSourcesManager.Create(loggerIntf, configManager);
  options := UpdateOptions;
  try
    options.Source := 'ftp://nowhere.example';
    updated := manager.UpdateSource(options);
  finally
    options.Free;
  end;
  Assert.IsFalse(updated);
  Assert.IsTrue(logger.HasError(TErrorCode.E1002_InvalidSource), logger.Messages.Text);
  Assert.AreEqual(0, memory.SaveCount, 'nothing is saved');
  Assert.AreEqual('https://example.invalid/index.json', Gallery(config).Source, 'nor changed');
end;

procedure TSourcesManagerTests.Update_Of_The_Type_Is_Applied;
var
  config : IConfiguration;
  configManager : IConfigurationManager;
  manager : ISourcesManager;
  options : TSourcesOptions;
begin
  config := GalleryConfig;
  configManager := TMemoryConfigManager.Create(config);
  manager := TSourcesManager.Create(TTestLogger.Create, configManager);
  options := UpdateOptions;
  try
    options.SourceType := TSourceType.GitRegistry;
    Assert.IsTrue(manager.UpdateSource(options));
  finally
    options.Free;
  end;
  Assert.IsTrue(Gallery(config).SourceType = TSourceType.GitRegistry);
  Assert.AreEqual('https://example.invalid/index.json', Gallery(config).Source);
end;

procedure TSourcesManagerTests.Update_To_A_Git_Url_Makes_It_A_Git_Registry;
var
  config : IConfiguration;
  configManager : IConfigurationManager;
  manager : ISourcesManager;
  options : TSourcesOptions;
begin
  config := GalleryConfig;
  configManager := TMemoryConfigManager.Create(config);
  manager := TSourcesManager.Create(TTestLogger.Create, configManager);
  options := UpdateOptions;
  try
    options.Source := 'git@github.com:example/registry.git';
    Assert.IsTrue(manager.UpdateSource(options));
  finally
    options.Free;
  end;
  Assert.IsTrue(Gallery(config).SourceType = TSourceType.GitRegistry);
end;

procedure TSourcesManagerTests.Update_To_A_File_Uri_For_A_Server_Is_Refused;
var
  config : IConfiguration;
  configManager : IConfigurationManager;
  logger : TTestLogger;
  loggerIntf : ILogger;
  manager : ISourcesManager;
  options : TSourcesOptions;
  updated : boolean;
begin
  config := GalleryConfig;
  configManager := TMemoryConfigManager.Create(config);
  logger := TTestLogger.Create;
  loggerIntf := logger;
  manager := TSourcesManager.Create(loggerIntf, configManager);
  options := UpdateOptions;
  try
    options.Source := 'file:///c:/packages';
    updated := manager.UpdateSource(options);
  finally
    options.Free;
  end;
  Assert.IsFalse(updated);
  Assert.IsTrue(logger.HasError(TErrorCode.E1002_InvalidSource), logger.Messages.Text);
end;

initialization
  TDUnitX.RegisterTestFixture(TSourcesManagerTests);

end.
