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
  TestLogger;

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

initialization
  TDUnitX.RegisterTestFixture(TRegistryCatalogTests);

end.
