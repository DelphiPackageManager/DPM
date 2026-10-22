unit DPM.Core.Tests.Registry.Catalog;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TRegistryCatalogTests = class
  private
    FRootDir : string;
    procedure WritePackageFolder(const id : string; const repositoryUrl : string; const withDependency : boolean);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure FolderRegistry_LoadsPackageSpec;

    [Test]
    procedure GetPackageSpec_IsCaseInsensitive;

    [Test]
    procedure GetPackageSpec_UnknownId_ReturnsNil;

    [Test]
    procedure GetPackageIds_ReturnsAllPackages;
    [Test]
    procedure GitRegistry_ExplicitRefreshFailure_IsAnError;
    [Test]
    procedure GitRegistry_CloneFailureDuringAQuery_IsAWarning;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Git.Interfaces,
  DPM.Core.Git.Client,
  DPM.Core.Registry.Interfaces,
  DPM.Core.Registry.Catalog,
  TestLogger,
  DPM.Core.Diagnostics;

type
  //every git command fails, as it does when the registry url is wrong.
  TFailingGitClient = class(TInterfacedObject, IGitClient)
  protected
    function IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;
    function LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;
    function LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;
    function Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
    function Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
    function Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
    function LastError : string;
  end;

{ TFailingGitClient }

function TFailingGitClient.Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
begin
  result := false;
end;

function TFailingGitClient.Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
begin
  result := false;
end;

function TFailingGitClient.IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;
begin
  result := true;
end;

function TFailingGitClient.LastError : string;
begin
  result := 'fatal: repository not found';
end;

function TFailingGitClient.LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;
begin
  commit := '';
  result := false;
end;

function TFailingGitClient.LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;
begin
  tags := nil;
  result := false;
end;

function TFailingGitClient.Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
begin
  result := false;
end;

{ TRegistryCatalogTests }

procedure TRegistryCatalogTests.WritePackageFolder(const id : string; const repositoryUrl : string; const withDependency : boolean);
var
  folder : string;
  lines : TStringList;
begin
  folder := TPath.Combine(FRootDir, id);
  TDirectory.CreateDirectory(folder);
  lines := TStringList.Create;
  try
    lines.Add('packageKind: git');
    lines.Add('metadata:');
    lines.Add('  id: ' + id);
    lines.Add('  description: A git registry test package');
    lines.Add('  authors:');
    lines.Add('    - Vincent Parrett');
    lines.Add('  license: Apache-2.0');
    lines.Add('  repositoryUrl: ' + repositoryUrl);
    lines.Add('targetPlatforms:');
    lines.Add('  - compiler: 12.0');
    lines.Add('    platforms: [Win32, Win64]');
    lines.Add('    template: default');
    lines.Add('templates:');
    lines.Add('  - name: default');
    if withDependency then
    begin
      lines.Add('    dependencies:');
      lines.Add('      - id: Some.Dependency');
      lines.Add('        version: "[1.0.0,2.0.0)"');
    end;
    lines.Add('    source:');
    lines.Add('      - src: .\source\*.pas');
    lines.Add('        dest: src');
    //SaveToFile uses platform (CRLF) line endings.
    lines.SaveToFile(TPath.Combine(folder, id + '.dspec.yaml'));
  finally
    lines.Free;
  end;
end;

procedure TRegistryCatalogTests.Setup;
begin
  FRootDir := TPath.Combine(TPath.GetTempPath, 'dpm_catalog_test');
  if TDirectory.Exists(FRootDir) then
    TDirectory.Delete(FRootDir, true);
  TDirectory.CreateDirectory(FRootDir);
  WritePackageFolder('Test.GitPackage', 'https://github.com/example/Test.GitPackage.git', true);
  WritePackageFolder('Test.Other', 'https://github.com/example/Test.Other.git', false);
end;

procedure TRegistryCatalogTests.TearDown;
begin
  if (FRootDir <> '') and TDirectory.Exists(FRootDir) then
    TDirectory.Delete(FRootDir, true);
end;

function CreateCatalog(const rootDir : string) : IRegistryCatalog;
var
  logger : ILogger;
  gitClient : IGitClient;
begin
  logger := TTestLogger.Create;
  gitClient := TGitClient.Create(logger);
  //folder source - git client is unused, registries folder irrelevant.
  result := TRegistryCatalog.Create(logger, gitClient, 'testreg', rootDir, '', 60);
end;

procedure TRegistryCatalogTests.FolderRegistry_LoadsPackageSpec;
var
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
  spec : IPackageSpec;
begin
  catalog := CreateCatalog(FRootDir);
  ct := TCancellationTokenSourceFactory.Create.Token;
  spec := catalog.GetPackageSpec(ct, 'Test.GitPackage');
  Assert.IsNotNull(spec, 'spec should be found');
  Assert.AreEqual('Test.GitPackage', spec.MetaData.Id);
  Assert.IsTrue(spec.PackageKind = TDPMPackageKind.git, 'packageKind should be git');
  Assert.AreEqual('https://github.com/example/Test.GitPackage.git', spec.MetaData.RepositoryUrl);
end;

procedure TRegistryCatalogTests.GetPackageSpec_IsCaseInsensitive;
var
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
begin
  catalog := CreateCatalog(FRootDir);
  ct := TCancellationTokenSourceFactory.Create.Token;
  Assert.IsNotNull(catalog.GetPackageSpec(ct, 'test.gitpackage'), 'lookup should be case-insensitive');
end;

procedure TRegistryCatalogTests.GetPackageSpec_UnknownId_ReturnsNil;
var
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
begin
  catalog := CreateCatalog(FRootDir);
  ct := TCancellationTokenSourceFactory.Create.Token;
  Assert.IsNull(catalog.GetPackageSpec(ct, 'No.Such.Package'));
end;

procedure TRegistryCatalogTests.GetPackageIds_ReturnsAllPackages;
var
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
  ids : IList<string>;
begin
  catalog := CreateCatalog(FRootDir);
  ct := TCancellationTokenSourceFactory.Create.Token;
  ids := catalog.GetPackageIds(ct);
  Assert.AreEqual(2, ids.Count);
end;

procedure TRegistryCatalogTests.GitRegistry_ExplicitRefreshFailure_IsAnError;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  catalog := TRegistryCatalog.Create(loggerIntf, TFailingGitClient.Create, 'gitreg', 'https://example.invalid/registry.git',
                                     TPath.Combine(FRootDir, 'registries'), 60);
  //dpm sources refresh - the refresh is what was asked for, so its failure is the error.
  Assert.IsFalse(catalog.EnsureUpdated(TCancellationTokenSourceFactory.Create.Token, true));
  Assert.IsTrue(logger.HasError(TErrorCode.E1203_GitCommandFailed), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'repository not found'), 'the error says what git said : ' + logger.Messages.Text);
  Assert.IsFalse(logger.HasWarning(TWarningCode.W1004_SourceUnavailable), logger.Messages.Text);
end;

procedure TRegistryCatalogTests.GitRegistry_CloneFailureDuringAQuery_IsAWarning;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  catalog := TRegistryCatalog.Create(loggerIntf, TFailingGitClient.Create, 'gitreg', 'https://example.invalid/registry.git',
                                     TPath.Combine(FRootDir, 'registries'), 60);
  //a query may find the package in another source - the one that needed it decides.
  Assert.IsFalse(catalog.EnsureUpdated(TCancellationTokenSourceFactory.Create.Token, false));
  Assert.IsTrue(logger.HasWarning(TWarningCode.W1004_SourceUnavailable), logger.Messages.Text);
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
end;

initialization
  TDUnitX.RegisterTestFixture(TRegistryCatalogTests);

end.
