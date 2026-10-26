unit DPM.Core.Tests.Cache.PackageId;

// A package id names folders in the package cache - {cache}\{compiler}\{id}\{version} - and everything DPM
// reads, writes and builds for the package is under that folder. The id comes from wherever the package did
// (a feed's json, the spec inside a .dpkg), so one that is not a valid package id ('..\..\x', 'c:\x') is
// refused where it comes in, and again where a cache path is made from it.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TPackageIdTests = class
  public
    [Test]
    [TestCase('Parent', '..\..\Evil.Package')]
    [TestCase('Nested', 'VSoft.Foo\..\..\..\Windows')]
    [TestCase('ForwardSlash', 'VSoft.Foo/../../Evil')]
    [TestCase('Rooted', '\Windows\Evil')]
    [TestCase('Drive', 'c:\Windows')]
    [TestCase('DotDot', '..')]
    procedure Cache_Refuses_An_Invalid_Id(const id : string);
    [Test]
    procedure Cache_Refuses_An_Empty_Id;
    [Test]
    procedure Cache_Accepts_A_Valid_Id;

    [Test]
    procedure Feed_Json_With_An_Invalid_Id_Is_Refused;
    [Test]
    procedure Feed_Json_With_A_Valid_Id_Is_Read;
    //a feed's searchPaths is ';' separated - a space is part of a path.
    [Test]
    procedure Feed_Json_Search_Paths_Keep_Their_Spaces;
    [Test]
    procedure Package_Spec_With_An_Invalid_Id_Is_Refused;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Diagnostics,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Cache,
  TestLogger;

const
  cCacheRoot = 'c:\dpmcache';

function NewCache : IPackageCache;
var
  logger : ILogger;
begin
  logger := TTestLogger.Create;
  result := TPackageCache.Create(logger, TPackageSpecReader.Create(logger), nil, nil, nil, nil, nil);
  result.Location := cCacheRoot;
end;

//Calls the cache's path builders with the id - returns the code of the EDPMError they raised, if any.
procedure CheckCacheRefuses(const id : string);
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  refused : integer;
begin
  cache := NewCache;
  identity := TPackageIdentity.Create('test', id, TPackageVersion.Parse('1.0.0'), TCompilerVersion.Delphi12_0);
  refused := 0;
  try
    cache.GetPackagePath(identity);
  except
    on e : EDPMError do
      if e.Code = TErrorCode.E3017_InvalidPackageId then
        Inc(refused);
  end;
  try
    cache.GetPackagePath(id, '1.0.0', TCompilerVersion.Delphi12_0);
  except
    on e : EDPMError do
      if e.Code = TErrorCode.E3017_InvalidPackageId then
        Inc(refused);
  end;
  try
    cache.GetPackageFileFolder(identity);
  except
    on e : EDPMError do
      if e.Code = TErrorCode.E3017_InvalidPackageId then
        Inc(refused);
  end;
  Assert.AreEqual(3, refused, 'every cache path made from [' + id + '] must be refused');
end;

{ TPackageIdTests }

procedure TPackageIdTests.Cache_Refuses_An_Invalid_Id(const id : string);
begin
  CheckCacheRefuses(id);
end;

procedure TPackageIdTests.Cache_Refuses_An_Empty_Id;
begin
  CheckCacheRefuses('');
end;

procedure TPackageIdTests.Cache_Accepts_A_Valid_Id;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
begin
  cache := NewCache;
  identity := TPackageIdentity.Create('test', 'VSoft.Foo', TPackageVersion.Parse('1.2.3'), TCompilerVersion.Delphi12_0);
  Assert.AreEqual(TPath.Combine(TPath.GetFullPath(cCacheRoot), CompilerToString(TCompilerVersion.Delphi12_0) + '\VSoft.Foo\1.2.3'),
                  cache.GetPackagePath(identity), true);
end;

procedure TPackageIdTests.Feed_Json_With_An_Invalid_Id_Is_Refused;
var
  logger : ILogger;
  json : TJsonObject;
  info : IPackageInfo;
begin
  logger := TTestLogger.Create;
  json := TJsonObject.Parse('{"id":"..\\..\\Evil.Package","compiler":"12.0","version":"1.0.0"}') as TJsonObject;
  try
    Assert.IsFalse(TPackageInfo.TryLoadFromJson(logger, json, 'feed', info), 'an entry with a path for an id must not be read');
  finally
    json.Free;
  end;
end;

procedure TPackageIdTests.Feed_Json_With_A_Valid_Id_Is_Read;
var
  logger : ILogger;
  json : TJsonObject;
  info : IPackageInfo;
begin
  logger := TTestLogger.Create;
  json := TJsonObject.Parse('{"id":"VSoft.Foo","compiler":"12.0","version":"1.0.0"}') as TJsonObject;
  try
    Assert.IsTrue(TPackageInfo.TryLoadFromJson(logger, json, 'feed', info), 'precondition : the json is otherwise readable');
    Assert.AreEqual('VSoft.Foo', info.Id, false);
  finally
    json.Free;
  end;
end;

procedure TPackageIdTests.Feed_Json_Search_Paths_Keep_Their_Spaces;
var
  logger : ILogger;
  json : TJsonObject;
  metadata : IPackageMetadata;
begin
  logger := TTestLogger.Create;
  json := TJsonObject.Parse('{"id":"VSoft.Foo","compiler":"12.0","version":"1.0.0","searchPaths":"Source;Source\\Some Folder"}') as TJsonObject;
  try
    Assert.IsTrue(TPackageMetadata.TryLoadFromJson(logger, json, 'feed', metadata));
    Assert.AreEqual(2, metadata.SearchPaths.Count);
    Assert.AreEqual('Source', metadata.SearchPaths[0], false);
    Assert.AreEqual('Source\Some Folder', metadata.SearchPaths[1], false);
  finally
    json.Free;
  end;
end;

procedure TPackageIdTests.Package_Spec_With_An_Invalid_Id_Is_Refused;
var
  logger : ILogger;
  reader : IPackageSpecReader;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  reader := TPackageSpecReader.Create(logger);
  //what a hand built .dpkg could carry - reading it only logs the bad id.
  spec := reader.ReadSpecString(
    'metadata:' + sLineBreak +
    '  id: ''..\..\Evil.Package''' + sLineBreak +
    '  version: 1.0.0' + sLineBreak +
    '  description: an id with a path in it' + sLineBreak +
    '  authors:' + sLineBreak +
    '    - DPM' + sLineBreak +
    '  license: Apache-2.0' + sLineBreak +
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32]' + sLineBreak +
    '    template: default' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak +
    '    source:' + sLineBreak +
    '      - src: .\source\*.pas' + sLineBreak);
  Assert.IsNotNull(spec, 'precondition : the spec reader returns the spec');
  Assert.WillRaise(
    procedure
    begin
      TPackageInfo.CreateFromManifest('folder', spec, '', '');
    end, Exception);
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageIdTests);

end.
