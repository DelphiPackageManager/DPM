unit DPM.Core.Tests.Package.InstallerContext;

interface

uses
  DUnitX.TestFramework,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.InstallerContext,
  DPM.Core.Package.Installer.Interfaces;

type
  ///<summary>Exposes the protected committed graph lookup so tests can check what an operation
  /// leaves recorded.</summary>
  TTestInstallerContext = class(TCorePackageInstallerContext)
  public
    function ProjectGraph(const projectFile : string) : IPackageReference;
  end;

  [TestFixture]
  TInstallerContextTests = class
  private
    FLogger : ILogger;
    FContext : TTestInstallerContext;
    FContextIntf : IPackageInstallerContext;
    procedure AddResolution(const projectFile : string; const id : string; const version : string);
    function FoundVersion(const projectFile : string; const id : string) : string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure Records_Outside_Operation_Are_Visible_To_Other_Projects;

    [Test]
    procedure Operation_Hides_Committed_Records_Of_Projects_In_Scope;

    [Test]
    procedure Operation_Leaves_Projects_Out_Of_Scope_Visible;

    [Test]
    procedure Staged_Records_Visible_To_Other_Projects_Not_Own;

    [Test]
    procedure CommitProject_Promotes_Staged_Resolutions_And_Graph;

    [Test]
    procedure EndOperation_Discards_Uncommitted_State;

    [Test]
    procedure Nested_BeginOperation_Raises;

    [Test]
    procedure RemoveProject_During_Operation_Drops_Staged_State;

    [Test]
    procedure Clear_Ends_Operation;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Constants,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Reference,
  DPM.Core.Dependency.Resolution,
  TestLogger;

const
  cCompiler = TCompilerVersion.Delphi12_0;
  cProjectA = 'C:\test\a.dproj';
  cProjectB = 'C:\test\b.dproj';
  cProjectC = 'C:\test\c.dproj';

function ProjectList(const projectFile : string) : IList<string>;
begin
  result := TCollections.CreateList<string>;
  result.Add(projectFile);
end;

function MakeGraph(const id : string; const version : string) : IPackageReference;
var
  packageVersion : TPackageVersion;
begin
  packageVersion := TPackageVersion.Parse(version);
  result := TPackageReference.CreateRoot(cCompiler);
  result.AddChild(id, packageVersion, TVersionRange.Create(packageVersion));
end;

{ TTestInstallerContext }

function TTestInstallerContext.ProjectGraph(const projectFile : string) : IPackageReference;
begin
  result := GetProjectGraph(projectFile);
end;

{ TInstallerContextTests }

procedure TInstallerContextTests.Setup;
begin
  FLogger := TTestLogger.Create;
  FContext := TTestInstallerContext.Create(FLogger);
  FContextIntf := FContext;
end;

procedure TInstallerContextTests.Teardown;
begin
  FContextIntf := nil;
  FContext := nil;
  FLogger := nil;
end;

procedure TInstallerContextTests.AddResolution(const projectFile : string; const id : string; const version : string);
var
  info : IPackageInfo;
  resolutions : TArray<IResolvedPackage>;
begin
  info := TPackageInfo.Create('test-source', id, TPackageVersion.Parse(version), cCompiler, '', '');
  SetLength(resolutions, 1);
  resolutions[0] := TResolvedPackage.Create(info, TVersionRange.Create(info.Version), cRootNode, projectFile);
  FContextIntf.RecordResolutions(projectFile, resolutions);
end;

function TInstallerContextTests.FoundVersion(const projectFile : string; const id : string) : string;
var
  resolution : IResolvedPackage;
begin
  resolution := FContextIntf.FindPackageResolution(projectFile, id);
  if resolution = nil then
    result := ''
  else
    result := resolution.PackageInfo.Version.ToStringNoMeta;
end;

procedure TInstallerContextTests.Records_Outside_Operation_Are_Visible_To_Other_Projects;
begin
  AddResolution(cProjectA, 'Base', '1.0.1');
  Assert.AreEqual('1.0.1', FoundVersion(cProjectB, 'Base'), 'another project should see A''s resolution');
  Assert.AreEqual('', FoundVersion(cProjectA, 'Base'), 'a project never sees its own resolution');
end;

procedure TInstallerContextTests.Operation_Hides_Committed_Records_Of_Projects_In_Scope;
var
  projects : IList<string>;
begin
  // A and B are both being upgraded. B's old resolution is about to be replaced, so it must not
  // constrain A while A is being processed.
  AddResolution(cProjectB, 'Base', '1.0.1');
  projects := TCollections.CreateList<string>;
  projects.Add(cProjectA);
  projects.Add(cProjectB);
  FContextIntf.BeginOperation(projects);
  Assert.AreEqual('', FoundVersion(cProjectA, 'Base'), 'B is in the operation - its committed record must be hidden');
  FContextIntf.EndOperation;
  Assert.AreEqual('1.0.1', FoundVersion(cProjectA, 'Base'), 'after the operation B''s record is visible again');
end;

procedure TInstallerContextTests.Operation_Leaves_Projects_Out_Of_Scope_Visible;
begin
  AddResolution(cProjectC, 'Base', '1.0.1');
  FContextIntf.BeginOperation(ProjectList(cProjectA));
  Assert.AreEqual('1.0.1', FoundVersion(cProjectA, 'Base'), 'C is not in the operation - it still constrains A');
  FContextIntf.EndOperation;
end;

procedure TInstallerContextTests.Staged_Records_Visible_To_Other_Projects_Not_Own;
var
  projects : IList<string>;
begin
  // A was processed first in the operation and staged Base 1.0.3. B, processed next, must be
  // checked against A's NEW resolution.
  AddResolution(cProjectA, 'Base', '1.0.1');
  projects := TCollections.CreateList<string>;
  projects.Add(cProjectA);
  projects.Add(cProjectB);
  FContextIntf.BeginOperation(projects);
  AddResolution(cProjectA, 'Base', '1.0.3');
  Assert.AreEqual('1.0.3', FoundVersion(cProjectB, 'Base'), 'B should see A''s staged resolution');
  Assert.AreEqual('', FoundVersion(cProjectA, 'Base'), 'A must not see its own staged resolution');
  FContextIntf.EndOperation;
end;

procedure TInstallerContextTests.CommitProject_Promotes_Staged_Resolutions_And_Graph;
begin
  AddResolution(cProjectA, 'Base', '1.0.1');
  FContextIntf.RecordGraph(cProjectA, MakeGraph('Base', '1.0.1'));
  FContextIntf.BeginOperation(ProjectList(cProjectA));
  AddResolution(cProjectA, 'Base', '1.0.3');
  FContextIntf.RecordGraph(cProjectA, MakeGraph('Base', '1.0.3'));
  Assert.AreEqual('1.0.1', FContext.ProjectGraph(cProjectA).FindTopLevelChild('Base').Version.ToStringNoMeta, 'the staged graph is not the recorded one until committed');

  FContextIntf.CommitProject(cProjectA);
  Assert.AreEqual('1.0.3', FContext.ProjectGraph(cProjectA).FindTopLevelChild('Base').Version.ToStringNoMeta, 'commit should promote the staged graph');
  Assert.AreEqual('1.0.3', FoundVersion(cProjectB, 'Base'), 'commit should promote the staged resolutions');
  FContextIntf.EndOperation;
  Assert.AreEqual('1.0.3', FoundVersion(cProjectB, 'Base'), 'committed state survives the end of the operation');
end;

procedure TInstallerContextTests.EndOperation_Discards_Uncommitted_State;
begin
  AddResolution(cProjectA, 'Base', '1.0.1');
  FContextIntf.RecordGraph(cProjectA, MakeGraph('Base', '1.0.1'));
  FContextIntf.BeginOperation(ProjectList(cProjectA));
  AddResolution(cProjectA, 'Base', '1.0.3');
  FContextIntf.RecordGraph(cProjectA, MakeGraph('Base', '1.0.3'));
  FContextIntf.EndOperation;

  Assert.AreEqual('1.0.1', FoundVersion(cProjectB, 'Base'), 'an aborted operation must leave the old resolution');
  Assert.AreEqual('1.0.1', FContext.ProjectGraph(cProjectA).FindTopLevelChild('Base').Version.ToStringNoMeta, 'an aborted operation must leave the old graph');
end;

procedure TInstallerContextTests.Nested_BeginOperation_Raises;
begin
  FContextIntf.BeginOperation(ProjectList(cProjectA));
  Assert.WillRaise(
    procedure
    begin
      FContextIntf.BeginOperation(ProjectList(cProjectB));
    end, EInvalidOpException);
  FContextIntf.EndOperation;
end;

procedure TInstallerContextTests.RemoveProject_During_Operation_Drops_Staged_State;
begin
  FContextIntf.BeginOperation(ProjectList(cProjectA));
  AddResolution(cProjectA, 'Base', '1.0.3');
  FContextIntf.RemoveProject(cProjectA);
  Assert.AreEqual('', FoundVersion(cProjectB, 'Base'), 'a removed project''s staged resolutions must go');
  FContextIntf.CommitProject(cProjectA);
  Assert.AreEqual('', FoundVersion(cProjectB, 'Base'), 'committing a removed project records nothing');
  FContextIntf.EndOperation;
end;

procedure TInstallerContextTests.Clear_Ends_Operation;
begin
  FContextIntf.BeginOperation(ProjectList(cProjectA));
  AddResolution(cProjectA, 'Base', '1.0.3');
  FContextIntf.Clear;
  Assert.AreEqual('', FoundVersion(cProjectB, 'Base'), 'clear must drop staged resolutions');
  Assert.WillNotRaise(
    procedure
    begin
      FContextIntf.BeginOperation(ProjectList(cProjectA));
    end, EInvalidOpException, 'clear ends the operation, so a new one can begin');
  FContextIntf.EndOperation;
end;

initialization
  TDUnitX.RegisterTestFixture(TInstallerContextTests);

end.
