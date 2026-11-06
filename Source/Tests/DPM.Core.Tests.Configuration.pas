unit DPM.Core.Tests.Configuration;

// The compilerOutput section of dpm.config.yaml - how package compiles show their output.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCompilerOutputConfigTests = class
  public
    [Test]
    procedure Defaults_When_The_Section_Is_Missing;
    //case doesn't matter, as for the other enum-like values in the file.
    [Test]
    procedure Values_Are_Read;
    [Test]
    procedure Values_Survive_A_Save_And_Load;
    [Test]
    procedure Invalid_Log_Level_Warns_And_Uses_The_Default;
    [Test]
    procedure Invalid_MSBuild_Verbosity_Warns_And_Uses_The_Default;
    //saving a configuration must not add the section to every file.
    [Test]
    procedure Defaults_Are_Not_Saved;
  end;

implementation

uses
  System.SysUtils,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Diagnostics,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  TestLogger;

const
  cBase = 'packageCacheLocation: c:/cache' + sLineBreak;

function LoadConfig(const logger : ILogger; const yaml : string) : IConfiguration;
begin
  result := TConfiguration.Create(logger);
  result.LoadFromYAML(TYAML.LoadFromString(yaml).Root);
end;

function SaveConfig(const config : IConfiguration) : string;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  config.SaveToYAML(doc.Root);
  result := TYAML.WriteToString(doc);
end;

procedure TCompilerOutputConfigTests.Defaults_When_The_Section_Is_Missing;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  config : IConfiguration;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  config := LoadConfig(loggerIntf, cBase);
  Assert.IsTrue(config.CompilerOutput.LogLevel = TCompilerOutputLogLevel.Verbose, 'logLevel');
  Assert.IsTrue(config.CompilerOutput.MSBuildVerbosity = TCompilerVerbosity.Normal, 'msbuildVerbosity');
  Assert.AreEqual(0, logger.WarningCount, logger.Messages.Text);
end;

procedure TCompilerOutputConfigTests.Values_Are_Read;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  config : IConfiguration;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  config := LoadConfig(loggerIntf, cBase + 'compilerOutput:' + sLineBreak +
                                   '  logLevel: Information' + sLineBreak +
                                   '  msbuildVerbosity: DETAILED' + sLineBreak);
  Assert.IsTrue(config.CompilerOutput.LogLevel = TCompilerOutputLogLevel.Information, 'logLevel');
  Assert.IsTrue(config.CompilerOutput.MSBuildVerbosity = TCompilerVerbosity.Detailed, 'msbuildVerbosity');
  Assert.AreEqual(0, logger.WarningCount, logger.Messages.Text);
end;

procedure TCompilerOutputConfigTests.Values_Survive_A_Save_And_Load;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  config : IConfiguration;
  reloaded : IConfiguration;
  yaml : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  config := TConfiguration.Create(loggerIntf);
  config.CompilerOutput.LogLevel := TCompilerOutputLogLevel.Information;
  config.CompilerOutput.MSBuildVerbosity := TCompilerVerbosity.Diagnostic;
  yaml := SaveConfig(config);
  Assert.Contains(yaml, 'compilerOutput');
  reloaded := LoadConfig(loggerIntf, yaml);
  Assert.IsTrue(reloaded.CompilerOutput.LogLevel = TCompilerOutputLogLevel.Information, 'logLevel : ' + yaml);
  Assert.IsTrue(reloaded.CompilerOutput.MSBuildVerbosity = TCompilerVerbosity.Diagnostic, 'msbuildVerbosity : ' + yaml);
end;

procedure TCompilerOutputConfigTests.Invalid_Log_Level_Warns_And_Uses_The_Default;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  config : IConfiguration;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  config := LoadConfig(loggerIntf, cBase + 'compilerOutput:' + sLineBreak +
                                   '  logLevel: loud' + sLineBreak +
                                   '  msbuildVerbosity: minimal' + sLineBreak);
  Assert.IsTrue(config.CompilerOutput.LogLevel = TCompilerOutputLogLevel.Verbose, 'the default is used');
  //the other value is still read.
  Assert.IsTrue(config.CompilerOutput.MSBuildVerbosity = TCompilerVerbosity.Minimal, 'msbuildVerbosity');
  Assert.IsTrue(logger.HasWarning(TWarningCode.W0204_ConfigurationValueInvalid), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('warning', 'loud'), 'the warning names the value : ' + logger.Messages.Text);
end;

procedure TCompilerOutputConfigTests.Invalid_MSBuild_Verbosity_Warns_And_Uses_The_Default;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  config : IConfiguration;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  config := LoadConfig(loggerIntf, cBase + 'compilerOutput:' + sLineBreak +
                                   '  msbuildVerbosity: chatty' + sLineBreak);
  Assert.IsTrue(config.CompilerOutput.MSBuildVerbosity = TCompilerVerbosity.Normal, 'the default is used');
  Assert.IsTrue(logger.HasWarning(TWarningCode.W0204_ConfigurationValueInvalid), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('warning', 'chatty'), 'the warning names the value : ' + logger.Messages.Text);
end;

procedure TCompilerOutputConfigTests.Defaults_Are_Not_Saved;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  config : IConfiguration;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  config := TConfiguration.Create(loggerIntf);
  Assert.DoesNotContain(SaveConfig(config), 'compilerOutput');
end;

initialization
  TDUnitX.RegisterTestFixture(TCompilerOutputConfigTests);

end.
