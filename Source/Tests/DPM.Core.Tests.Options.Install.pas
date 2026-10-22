unit DPM.Core.Tests.Options.Install;

interface

uses
  DUnitX.TestFramework,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Install;

type
  [TestFixture]
  TInstallOptionsTests = class
  private
    FLogger : ILogger;
    FOptions : TInstallOptions;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure Single_Id_Becomes_One_Request;

    [Test]
    procedure Single_Id_Takes_Version_Option;

    [Test]
    procedure List_With_Inline_Versions_Parsed;

    [Test]
    procedure Duplicate_Id_Is_An_Error;

    [Test]
    procedure Invalid_Id_Is_An_Error;

    [Test]
    procedure Invalid_Inline_Version_Is_An_Error;

    [Test]
    procedure Version_Option_With_Several_Packages_Is_An_Error;

    [Test]
    procedure Version_Option_And_Inline_Version_Is_An_Error;

    [Test]
    procedure Existing_Package_File_Stays_A_Package_File;

    [Test]
    procedure Clone_Copies_Packages;

    [Test]
    procedure AddPackage_Sets_First_Package_As_PackageId;

    [Test]
    procedure Missing_Config_File_Fails_Validation;

    [Test]
    procedure Errors_Carry_Codes;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  TestLogger,
  DPM.Core.Diagnostics;

{ TInstallOptionsTests }

procedure TInstallOptionsTests.Setup;
begin
  FLogger := TTestLogger.Create;
  FOptions := TInstallOptions.Create;
  FOptions.ConfigFile := 'dpm.config.yaml';
  FOptions.ProjectPath := 'c:\test\project.dproj';
end;

procedure TInstallOptionsTests.Teardown;
begin
  FOptions.Free;
  FOptions := nil;
  FLogger := nil;
end;

procedure TInstallOptionsTests.Single_Id_Becomes_One_Request;
begin
  FOptions.PackageId := 'VSoft.YAML';
  Assert.IsTrue(FOptions.Validate(FLogger));
  Assert.AreEqual(1, Length(FOptions.Packages));
  Assert.AreEqual('VSoft.YAML', FOptions.Packages[0].PackageId, false);
  Assert.IsTrue(FOptions.Packages[0].Version.IsEmpty, 'no version means latest');
  Assert.AreEqual('', FOptions.PackageFile, 'a package id is not a package file');
end;

procedure TInstallOptionsTests.Single_Id_Takes_Version_Option;
begin
  FOptions.PackageId := 'VSoft.YAML';
  FOptions.VersionString := '2.0.0';
  Assert.IsTrue(FOptions.Validate(FLogger));
  Assert.AreEqual('2.0.0', FOptions.Packages[0].Version.ToStringNoMeta);
  Assert.AreEqual('2.0.0', FOptions.Version.ToStringNoMeta, 'Version mirrors the single package');
end;

procedure TInstallOptionsTests.List_With_Inline_Versions_Parsed;
begin
  FOptions.PackageId := 'Spring4D.Core@2.0.3-beta.3, Spring4D.Extensions';
  Assert.IsTrue(FOptions.Validate(FLogger));
  Assert.AreEqual(2, Length(FOptions.Packages));
  Assert.AreEqual('Spring4D.Core', FOptions.Packages[0].PackageId, false);
  Assert.AreEqual('2.0.3-beta.3', FOptions.Packages[0].Version.ToStringNoMeta);
  Assert.AreEqual('Spring4D.Extensions', FOptions.Packages[1].PackageId, false);
  Assert.IsTrue(FOptions.Packages[1].Version.IsEmpty, 'no inline version means latest');
  Assert.AreEqual('Spring4D.Core', FOptions.PackageId, false, 'PackageId mirrors the first package');
  Assert.AreEqual('2.0.3-beta.3', FOptions.Version.ToStringNoMeta, 'Version mirrors the first package');
end;

procedure TInstallOptionsTests.Missing_Config_File_Fails_Validation;
begin
  //it used to log the error and still say the options were valid.
  FOptions.PackageId := 'Spring4D.Core';
  FOptions.ConfigFile := '';
  Assert.IsFalse(FOptions.Validate(FLogger));
  Assert.IsFalse(FOptions.IsValid);
  Assert.IsTrue((FLogger as TTestLogger).HasError(TErrorCode.E0101_MissingArgument), (FLogger as TTestLogger).Messages.Text);
end;

procedure TInstallOptionsTests.Errors_Carry_Codes;
begin
  FOptions.PackageId := 'Spring4D.Core,spring4d.core';
  Assert.IsFalse(FOptions.Validate(FLogger));
  Assert.IsTrue((FLogger as TTestLogger).HasError(TErrorCode.E0103_ConflictingArguments), (FLogger as TTestLogger).Messages.Text);
  Assert.AreEqual(1, FLogger.ErrorCount, (FLogger as TTestLogger).Messages.Text);
end;

procedure TInstallOptionsTests.Duplicate_Id_Is_An_Error;
begin
  FOptions.PackageId := 'Spring4D.Core,spring4d.core@2.0.3';
  Assert.IsFalse(FOptions.Validate(FLogger));
  Assert.IsTrue((FLogger as TTestLogger).Logged('error', 'more than once'));
end;

procedure TInstallOptionsTests.Invalid_Id_Is_An_Error;
begin
  FOptions.PackageId := 'Spring4D.Core,NotAPackage';
  Assert.IsFalse(FOptions.Validate(FLogger));
  Assert.IsTrue((FLogger as TTestLogger).Logged('error', 'NotAPackage'));
end;

procedure TInstallOptionsTests.Invalid_Inline_Version_Is_An_Error;
begin
  FOptions.PackageId := 'Spring4D.Core@x.y';
  Assert.IsFalse(FOptions.Validate(FLogger));
  Assert.IsTrue((FLogger as TTestLogger).Logged('error', 'x.y'));
end;

procedure TInstallOptionsTests.Version_Option_With_Several_Packages_Is_An_Error;
begin
  FOptions.PackageId := 'Spring4D.Core,Spring4D.Extensions';
  FOptions.VersionString := '2.0.3';
  Assert.IsFalse(FOptions.Validate(FLogger));
  Assert.IsTrue((FLogger as TTestLogger).Logged('error', 'single package'));
end;

procedure TInstallOptionsTests.Version_Option_And_Inline_Version_Is_An_Error;
begin
  FOptions.PackageId := 'Spring4D.Core@2.0.3';
  FOptions.VersionString := '2.0.3';
  Assert.IsFalse(FOptions.Validate(FLogger));
  Assert.IsTrue((FLogger as TTestLogger).Logged('error', 'specified twice'));
end;

procedure TInstallOptionsTests.Existing_Package_File_Stays_A_Package_File;
var
  fileName : string;
begin
  fileName := TPath.Combine(TPath.GetTempPath, 'VSoft.OptionsTest-delphi12.0-win32-1.0.0.dpkg');
  TFile.WriteAllText(fileName, 'not really a package');
  try
    FOptions.PackageId := fileName;
    Assert.IsTrue(FOptions.Validate(FLogger));
    Assert.AreEqual(fileName, FOptions.PackageFile, 'an existing file path is a package file');
    Assert.AreEqual(0, Length(FOptions.Packages), 'a package file is not parsed as a package list');
  finally
    TFile.Delete(fileName);
  end;
end;

procedure TInstallOptionsTests.Clone_Copies_Packages;
var
  clone : TInstallOptions;
begin
  FOptions.PackageId := 'Spring4D.Core@2.0.3,Spring4D.Extensions@2.0.3';
  Assert.IsTrue(FOptions.Validate(FLogger));
  clone := FOptions.Clone;
  try
    Assert.AreEqual(2, Length(clone.Packages));
    Assert.AreEqual('Spring4D.Extensions', clone.Packages[1].PackageId, false);
  finally
    clone.Free;
  end;
end;

procedure TInstallOptionsTests.AddPackage_Sets_First_Package_As_PackageId;
begin
  FOptions.AddPackage('Spring4D.Core', TPackageVersion.Parse('2.0.3'));
  FOptions.AddPackage('Spring4D.Extensions', TPackageVersion.Parse('2.0.3'));
  Assert.AreEqual('Spring4D.Core', FOptions.PackageId, false);
  Assert.IsTrue(FOptions.Validate(FLogger));
  Assert.AreEqual(2, Length(FOptions.Packages), 'packages added in code are kept as they are');
end;

initialization
  TDUnitX.RegisterTestFixture(TInstallOptionsTests);

end.
