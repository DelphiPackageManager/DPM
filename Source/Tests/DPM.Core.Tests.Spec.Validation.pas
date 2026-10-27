unit DPM.Core.Tests.Spec.Validation;

// What the spec reader accepts and refuses, and what it says when it refuses.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSpecValidationTests = class
  public
    //a git package's versions come from the repository's tags - the spec doesn't need one.
    [Test]
    procedure Git_Package_Spec_Needs_No_Version;
    [Test]
    procedure Dpm_Package_Spec_Needs_A_Version;

    //the oldest DPM that can read the package is what the spec says - not the version of DPM that wrote it.
    [Test]
    procedure Spec_Writes_The_Minimum_Client_Version_It_Declares;
    [Test]
    procedure Spec_Without_A_Minimum_Client_Version_Writes_None;

    //an entry's platforms narrow it - one DPM can't read is refused, not dropped (no platforms means all of them).
    [Test]
    [TestCase('Build', 'build')]
    [TestCase('Design', 'design')]
    [TestCase('CopyLocal', 'copyLocal')]
    [TestCase('PackageDefinition', 'package definitions')]
    procedure Entry_With_An_Unknown_Platform_Is_Refused(const entryKind : string);

    //the spellings of copyToLib a source entry accepts all mean what they say.
    [Test]
    [TestCase('Canonical', 'copyToLib')]
    [TestCase('LowerT', 'copytoLib')]
    [TestCase('AllLower', 'copytolib')]
    procedure Source_Entry_Reads_CopyToLib(const key : string);

    //a value of the wrong shape is reported naming the key - not a bare 'Value is not a sequence'.
    [Test]
    procedure Scalar_Where_A_List_Belongs_Names_The_Key;
    [Test]
    procedure Template_List_Of_The_Wrong_Shape_Names_The_List;
    [Test]
    procedure List_Item_That_Is_Not_A_Mapping_Names_The_List;
  end;

  //Editing a spec's templates (DSpecCreator) - the target platforms name them, and go on naming the right one.
  [TestFixture]
  TSpecTemplateEditTests = class
  public
    [Test]
    procedure Renaming_A_Template_Renames_It_In_The_Target_Platforms;
    //left naming a template that isn't there - the spec then says it's not valid, rather than packing nothing.
    [Test]
    procedure Deleting_A_Template_Leaves_Its_Target_Platforms_Without_One;
    [Test]
    procedure Renaming_To_The_Name_Of_Another_Template_Is_Refused;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Diagnostics,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  TestLogger;

const
  cMetadataWithoutVersion =
    'metadata:' + sLineBreak +
    '  id: VSoft.App' + sLineBreak +
    '  description: spec validation' + sLineBreak +
    '  authors:' + sLineBreak +
    '    - DPM' + sLineBreak +
    '  license: Apache-2.0' + sLineBreak;

  cMetadata =
    cMetadataWithoutVersion +
    '  version: 1.0.0' + sLineBreak;

  cTargetsAndTemplate =
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32]' + sLineBreak +
    '    template: default' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak +
    '    source:' + sLineBreak +
    '      - src: .\source\*.pas' + sLineBreak;

//reads the spec text - the logger has what it reported.
function ReadSpec(const logger : TTestLogger; const text : string) : IPackageSpec;
var
  loggerRef : ILogger;
  reader : IPackageSpecReader;
begin
  loggerRef := logger;
  reader := TPackageSpecReader.Create(loggerRef);
  result := reader.ReadSpecString(text);
end;

{ TSpecValidationTests }

procedure TSpecValidationTests.Git_Package_Spec_Needs_No_Version;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, 'packageKind: git' + sLineBreak + cMetadataWithoutVersion + cTargetsAndTemplate);
  Assert.IsNotNull(spec);
  Assert.IsTrue(spec.IsValid, logger.Messages.Text);
  Assert.IsTrue(spec.PackageKind = TDPMPackageKind.git);
end;

procedure TSpecValidationTests.Dpm_Package_Spec_Needs_A_Version;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, cMetadataWithoutVersion + cTargetsAndTemplate);
  Assert.IsNotNull(spec);
  Assert.IsFalse(spec.IsValid, 'a dpm package needs a version');
  Assert.IsTrue(logger.HasError(TErrorCode.E6003_SpecElementMissing), logger.Messages.Text);
end;

procedure TSpecValidationTests.Spec_Writes_The_Minimum_Client_Version_It_Declares;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
  yaml : string;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, 'min dpm client version: 0.9.0' + sLineBreak + cMetadata + cTargetsAndTemplate);
  Assert.IsTrue(spec.IsValid, logger.Messages.Text);
  yaml := spec.GenerateDspecYAML(spec.MetaData.Version);
  Assert.IsTrue(Pos('min dpm client version: 0.9.0', yaml) > 0, yaml);
end;

procedure TSpecValidationTests.Spec_Without_A_Minimum_Client_Version_Writes_None;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
  yaml : string;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, cMetadata + cTargetsAndTemplate);
  Assert.IsTrue(spec.IsValid, logger.Messages.Text);
  yaml := spec.GenerateDspecYAML(spec.MetaData.Version);
  Assert.AreEqual(0, Pos('min dpm client version', yaml), yaml);
end;

procedure TSpecValidationTests.Entry_With_An_Unknown_Platform_Is_Refused(const entryKind : string);
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
  entry : string;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  if entryKind = 'copyLocal' then
    entry := '      - src: bin\*.dll' + sLineBreak
  else if entryKind = 'package definitions' then
    entry := '      - project: packages\Pkg.dproj' + sLineBreak +
             '        files:' + sLineBreak +
             '          - source\*.pas' + sLineBreak
  else
    entry := '      - project: packages\Pkg.dproj' + sLineBreak;
  //OSX64 is the IDE's name - DPM's is MacOS64.
  spec := ReadSpec(logger, cMetadata + cTargetsAndTemplate +
                   '    ' + entryKind + ':' + sLineBreak + entry +
                   '        platforms: [Win32, OSX64]' + sLineBreak);
  Assert.IsNotNull(spec);
  Assert.IsFalse(spec.IsValid, 'an unknown platform in a ' + entryKind + ' entry');
  Assert.IsTrue(logger.HasError(TErrorCode.E6004_SpecValueInvalid), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', '[OSX64]'), logger.Messages.Text);
end;

procedure TSpecValidationTests.Source_Entry_Reads_CopyToLib(const key : string);
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, cMetadata + cTargetsAndTemplate + '        ' + key + ': true' + sLineBreak);
  Assert.IsTrue(spec.IsValid, logger.Messages.Text);
  Assert.IsTrue(spec.FindTemplate('default').SourceEntries[0].CopyToLib, key + ': true');
end;

procedure TSpecValidationTests.Scalar_Where_A_List_Belongs_Names_The_Key;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, StringReplace(cMetadata, '  authors:' + sLineBreak + '    - DPM' + sLineBreak, '  authors: DPM' + sLineBreak, []) +
                           cTargetsAndTemplate);
  Assert.IsNotNull(spec, 'the spec is still read - its problems are reported');
  Assert.IsFalse(spec.IsValid);
  Assert.IsTrue(logger.Logged('error', 'The [authors] of the metadata must be a list'), logger.Messages.Text);
end;

procedure TSpecValidationTests.Template_List_Of_The_Wrong_Shape_Names_The_List;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, cMetadata + cTargetsAndTemplate + '    build: packages\Pkg.dproj' + sLineBreak);
  Assert.IsNotNull(spec);
  Assert.IsFalse(spec.IsValid);
  Assert.IsTrue(logger.Logged('error', 'The [build] of the template [default] must be a list'), logger.Messages.Text);
end;

procedure TSpecValidationTests.List_Item_That_Is_Not_A_Mapping_Names_The_List;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  //the project's path without the 'project:' key.
  spec := ReadSpec(logger, cMetadata + cTargetsAndTemplate +
                           '    build:' + sLineBreak +
                           '      - packages\Pkg.dproj' + sLineBreak);
  Assert.IsNotNull(spec);
  Assert.IsFalse(spec.IsValid);
  Assert.IsTrue(logger.Logged('error', 'Item 1 of the [build] of the template [default]'), logger.Messages.Text);
end;

{ TSpecTemplateEditTests }

const
  cTwoTemplates =
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32]' + sLineBreak +
    '    template: default' + sLineBreak +
    '  - compiler: 13.0' + sLineBreak +
    '    platforms: [Win32]' + sLineBreak +
    '    template: other' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak +
    '    source:' + sLineBreak +
    '      - src: .\source\*.pas' + sLineBreak +
    '  - name: other' + sLineBreak +
    '    source:' + sLineBreak +
    '      - src: .\other\*.pas' + sLineBreak;

procedure TSpecTemplateEditTests.Renaming_A_Template_Renames_It_In_The_Target_Platforms;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, cMetadata + cTwoTemplates);
  spec.RenameTemplate('default', 'main');
  Assert.AreEqual('main', spec.TargetPlatforms[0].TemplateName);
  Assert.AreEqual('other', spec.TargetPlatforms[1].TemplateName, 'only the platforms that named it');
  Assert.IsNotNull(spec.FindTemplate('main'));
end;

procedure TSpecTemplateEditTests.Deleting_A_Template_Leaves_Its_Target_Platforms_Without_One;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, cMetadata + cTwoTemplates);
  spec.DeleteTemplate('other');
  Assert.AreEqual('', spec.TargetPlatforms[1].TemplateName);
  Assert.AreEqual('default', spec.TargetPlatforms[0].TemplateName);
  Assert.IsNull(spec.FindTemplate('other'));
end;

procedure TSpecTemplateEditTests.Renaming_To_The_Name_Of_Another_Template_Is_Refused;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  spec : IPackageSpec;
  raised : boolean;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  spec := ReadSpec(logger, cMetadata + cTwoTemplates);
  raised := false;
  try
    spec.RenameTemplate('default', 'OTHER');
  except
    on e : Exception do
      raised := true;
  end;
  Assert.IsTrue(raised, 'two templates of one name');
  Assert.IsNotNull(spec.FindTemplate('default'), 'nothing renamed');
  Assert.AreEqual('default', spec.TargetPlatforms[0].TemplateName);
end;

initialization
  TDUnitX.RegisterTestFixture(TSpecValidationTests);
  TDUnitX.RegisterTestFixture(TSpecTemplateEditTests);

end.
