unit DPM.Core.Tests.Package.Dependencies;

// What DPM reads and compares about a package's dependencies : a BOM check sees a dependency added as well as
// one removed, package ids compare without case wherever they are hashed, a spec can't list one dependency
// twice, and a feed version whose dependency range can't be read is skipped rather than read without it.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TPackageDependencyTests = class
  public
    [Test]
    procedure Bom_Comparison_Sees_An_Added_Dependency;

    [Test]
    procedure Bom_Comparison_Sees_A_Removed_Dependency;

    [Test]
    procedure Package_Info_Comparer_Ignores_The_Id_Case;

    [Test]
    procedure List_Items_Ignore_The_Id_Case;

    [Test]
    procedure List_Item_Comparer_Agrees_With_Its_Hash;

    [Test]
    procedure Spec_With_A_Dependency_Listed_Twice_Is_Invalid;

    [Test]
    procedure Feed_Version_With_An_Unreadable_Range_Is_Skipped;

    //$version$ - the package's own version, filled in at pack time - in any case.
    [Test]
    procedure Spec_Dependency_Version_Token_Ignores_Case;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Defaults,
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Diagnostics,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Reference,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Package.ListItem,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  TestLogger;

const
  cCompiler = TCompilerVersion.Delphi12_0;

//a root holding VSoft.App 1.0.0 - what a BOM file loads, and what the installer compares it with.
function NewAppGraph : IPackageReference;
var
  version : TPackageVersion;
begin
  version := TPackageVersion.Parse('1.0.0');
  result := TPackageReference.CreateRoot(cCompiler);
  result.AddChild('VSoft.App', version, TVersionRange.Create(version));
end;

procedure AddDependency(const graph : IPackageReference; const id : string);
var
  version : TPackageVersion;
begin
  version := TPackageVersion.Parse('1.0.0');
  graph.FindTopLevelChild('VSoft.App').AddChild(id, version, TVersionRange.Create(version));
end;

function SpecWithDependencies(const firstId : string; const secondId : string) : string;
begin
  result :=
    'metadata:' + sLineBreak +
    '  id: VSoft.App' + sLineBreak +
    '  version: 1.0.0' + sLineBreak +
    '  description: two dependencies' + sLineBreak +
    '  authors:' + sLineBreak +
    '    - DPM' + sLineBreak +
    '  license: Apache-2.0' + sLineBreak +
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32]' + sLineBreak +
    '    template: default' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak +
    '    dependencies:' + sLineBreak +
    '      - id: ' + firstId + sLineBreak +
    '        version: 1.0.0' + sLineBreak +
    '      - id: ' + secondId + sLineBreak +
    '        version: 1.0.1' + sLineBreak +
    '    source:' + sLineBreak +
    '      - src: .\source\*.pas' + sLineBreak;
end;

{ TPackageDependencyTests }

procedure TPackageDependencyTests.Bom_Comparison_Sees_An_Added_Dependency;
var
  bom : IPackageReference;
  current : IPackageReference;
begin
  bom := NewAppGraph;
  AddDependency(bom, 'VSoft.Base');
  current := NewAppGraph;
  AddDependency(current, 'VSoft.Base');
  Assert.IsTrue(bom.FindTopLevelChild('VSoft.App').AreEqual(current.FindTopLevelChild('VSoft.App')), 'precondition : the same dependencies compare equal');

  //the package now depends on VSoft.Extra as well - the compile the BOM would skip needs it.
  AddDependency(current, 'VSoft.Extra');
  Assert.IsFalse(bom.FindTopLevelChild('VSoft.App').AreEqual(current.FindTopLevelChild('VSoft.App')), 'a dependency added since the BOM was written');
end;

procedure TPackageDependencyTests.Bom_Comparison_Sees_A_Removed_Dependency;
var
  bom : IPackageReference;
  current : IPackageReference;
begin
  bom := NewAppGraph;
  AddDependency(bom, 'VSoft.Base');
  AddDependency(bom, 'VSoft.Extra');
  current := NewAppGraph;
  AddDependency(current, 'VSoft.Base');
  Assert.IsFalse(bom.FindTopLevelChild('VSoft.App').AreEqual(current.FindTopLevelChild('VSoft.App')), 'a dependency removed since the BOM was written');
end;

procedure TPackageDependencyTests.Package_Info_Comparer_Ignores_The_Id_Case;
var
  comparer : IEqualityComparer<IPackageInfo>;
  upper : IPackageInfo;
  lower : IPackageInfo;
  distinct : ISet<IPackageInfo>;
begin
  comparer := TPackageInfoComparer.Create;
  //the same version from two sources that don't agree on the casing.
  upper := TPackageInfo.Create('one', 'VSoft.Foo', TPackageVersion.Parse('1.0.0'), cCompiler, '', '');
  lower := TPackageInfo.Create('two', 'vsoft.foo', TPackageVersion.Parse('1.0.0'), cCompiler, '', '');
  Assert.IsTrue(comparer.Equals(upper, lower), 'precondition : the ids differ only in case');
  Assert.AreEqual(comparer.GetHashCode(upper), comparer.GetHashCode(lower), 'equal items must hash the same');

  distinct := TCollections.CreateSet<IPackageInfo>(comparer);
  distinct.Add(upper);
  distinct.Add(lower);
  Assert.AreEqual(1, distinct.Count, 'the versions query dedupes with this comparer');
end;

procedure TPackageDependencyTests.List_Items_Ignore_The_Id_Case;
var
  upper : IPackageListItem;
  lower : IPackageListItem;
begin
  upper := TPackageListItem.Create('VSoft.Foo', cCompiler, TPackageVersion.Parse('1.0.0'), [TDPMPlatform.Win32]);
  lower := TPackageListItem.Create('vsoft.foo', cCompiler, TPackageVersion.Parse('1.0.0'), [TDPMPlatform.Win64]);
  Assert.IsTrue(upper.IsSamePackageId(lower), 'one package, whatever the casing');
  Assert.IsTrue(upper.IsSamePackageVersion(lower), 'one package version, whatever the casing');
end;

procedure TPackageDependencyTests.List_Item_Comparer_Agrees_With_Its_Hash;
var
  comparer : IEqualityComparer<IPackageListItem>;
  upper : IPackageListItem;
  lower : IPackageListItem;
begin
  comparer := TPackageListItemEqualityComparer.Create;
  //Equals ignores the platforms - the hash must too.
  upper := TPackageListItem.Create('VSoft.Foo', cCompiler, TPackageVersion.Parse('1.0.0'), [TDPMPlatform.Win32]);
  lower := TPackageListItem.Create('vsoft.foo', cCompiler, TPackageVersion.Parse('1.0.0'), [TDPMPlatform.Win64]);
  Assert.IsTrue(comparer.Equals(upper, lower));
  Assert.AreEqual(comparer.GetHashCode(upper), comparer.GetHashCode(lower), 'equal items must hash the same');
end;

procedure TPackageDependencyTests.Spec_With_A_Dependency_Listed_Twice_Is_Invalid;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  reader : IPackageSpecReader;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  reader := TPackageSpecReader.Create(loggerRef);
  spec := reader.ReadSpecString(SpecWithDependencies('VSoft.Base', 'VSoft.Extra'));
  Assert.IsNotNull(spec);
  Assert.IsTrue(spec.IsValid, 'precondition : the spec is otherwise valid - ' + logger.Messages.Text);

  //one package, two ranges.
  spec := reader.ReadSpecString(SpecWithDependencies('VSoft.Base', 'vsoft.base'));
  Assert.IsNotNull(spec);
  Assert.IsFalse(spec.IsValid, 'a dependency listed twice');
  Assert.IsTrue(logger.HasError(TErrorCode.E6004_SpecValueInvalid));
  Assert.IsTrue(logger.Logged('error', 'listed more than once'), logger.Messages.Text);
end;

procedure TPackageDependencyTests.Feed_Version_With_An_Unreadable_Range_Is_Skipped;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  json : TJsonObject;
  info : IPackageInfo;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  json := TJsonObject.Parse('{"id":"VSoft.Foo","compiler":"12.0","version":"1.0.0","dependencies":[{"packageId":"VSoft.Bar","versionRange":"[1.0.0,]"}]}') as TJsonObject;
  try
    Assert.IsTrue(TPackageInfo.TryLoadFromJson(loggerRef, json, 'feed', info), 'precondition : the json is otherwise readable');
    Assert.AreEqual(1, info.Dependencies.Count);
  finally
    json.Free;
  end;

  //read without the dependency, the version would install without VSoft.Bar.
  json := TJsonObject.Parse('{"id":"VSoft.Foo","compiler":"12.0","version":"1.0.0","dependencies":[{"packageId":"VSoft.Bar","versionRange":"not a range"}]}') as TJsonObject;
  try
    Assert.IsFalse(TPackageInfo.TryLoadFromJson(loggerRef, json, 'feed', info), 'a version with a dependency that can not be read');
    Assert.IsTrue(logger.HasWarning(TWarningCode.W1207_PackageEntryUnreadable));
    Assert.IsTrue(logger.Logged('warning', 'VSoft.Bar'), logger.Messages.Text);
  finally
    json.Free;
  end;
end;

procedure TPackageDependencyTests.Spec_Dependency_Version_Token_Ignores_Case;
var
  logger : TTestLogger;
  loggerRef : ILogger;
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  dependency : ISpecDependency;
begin
  logger := TTestLogger.Create;
  loggerRef := logger;
  reader := TPackageSpecReader.Create(loggerRef);
  spec := reader.ReadSpecString(StringReplace(SpecWithDependencies('VSoft.Base', 'VSoft.Extra'), '        version: 1.0.1', '        version: $Version$', []));
  Assert.IsNotNull(spec);
  Assert.IsTrue(spec.IsValid, logger.Messages.Text);
  dependency := spec.FindTemplate('default').FindDependency('VSoft.Extra');
  Assert.IsNotNull(dependency);
  dependency.ResolveVersionToken(TPackageVersion.Parse('1.2.3'));
  Assert.AreEqual('1.2.3', dependency.Version.ToString);
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageDependencyTests);

end.
