unit DPM.Core.Tests.Cache.Icons;

// The package cache keeps the icons the IDE downloads for package versions that aren't installed - beside the
// package files, {cache}\{compiler}\{id}\{version}.icon.svg|png - and remembers for a while when a source said a
// version has no icon ({version}.icon.none), so the IDE doesn't ask it again every time the view opens.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TPackageCacheIconTests = class
  private
    FCacheRoot : string;
    FIdFolder : string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Saved_Svg_Icon_Is_Returned;
    [Test]
    procedure Saved_Png_Icon_Is_Returned;
    [Test]
    procedure Saved_Icon_Is_Kept_Beside_The_Package_Files;
    //the IDE shows the icon it just saved - reading its stream from the start.
    [Test]
    procedure Saving_Reads_The_Whole_Icon_And_Rewinds_It;
    [Test]
    procedure Nothing_Saved_Means_No_Icon_And_Not_Missing;
    [Test]
    procedure Missing_Icon_Is_Remembered;
    [Test]
    procedure Missing_Icon_Is_Forgotten_After_The_Retry_Period;
    [Test]
    procedure Saving_An_Icon_Forgets_That_It_Was_Missing;
    [Test]
    procedure Installed_Package_Icon_Wins_Over_A_Saved_One;
    [Test]
    procedure Saved_Icon_Is_For_Its_Version_Only;
    [Test]
    procedure Remove_Package_Removes_The_Saved_Icons;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.Constants,
  DPM.Core.Logging,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Package.Icon,
  DPM.Core.Package.Cache.Receipt,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Cache;

const
  cPackageId = 'Sample.Pkg';
  cSvgContent = '<svg xmlns="http://www.w3.org/2000/svg"/>';
  cPngContent = 'not really a png, the cache does not look inside';

function NewCache(const cacheRoot : string) : IPackageCache;
var
  logger : ILogger;
  specReader : IPackageSpecReader;
begin
  //held in locals - an interfaced object created in a parameter position can leak.
  logger := TTestLogger.Create;
  specReader := TPackageSpecReader.Create(logger);
  result := TPackageCache.Create(logger, specReader, nil, nil, nil, nil, nil);
  result.Location := cacheRoot;
end;

function NewIdentity(const version : string) : IPackageIdentity;
begin
  result := TPackageIdentity.Create('', cPackageId, TPackageVersion.Parse(version), TCompilerVersion.Delphi12_0);
end;

function NewIcon(const kind : TPackageIconKind; const content : string) : IPackageIcon;
var
  stream : TMemoryStream;
  bytes : TBytes;
begin
  bytes := TEncoding.UTF8.GetBytes(content);
  stream := TMemoryStream.Create;
  if Length(bytes) > 0 then
    stream.WriteBuffer(bytes[0], Length(bytes));
  //the icon owns the stream.
  result := CreatePackageIcon(kind, stream);
end;

function StreamText(const stream : TStream) : string;
var
  bytes : TBytes;
begin
  stream.Position := 0;
  SetLength(bytes, stream.Size);
  if Length(bytes) > 0 then
    stream.ReadBuffer(bytes[0], Length(bytes));
  result := TEncoding.UTF8.GetString(bytes);
end;

{ TPackageCacheIconTests }

procedure TPackageCacheIconTests.Setup;
var
  g : TGUID;
begin
  CreateGUID(g);
  FCacheRoot := TPath.Combine(TPath.GetTempPath, 'dpm-cache-icons-' + LowerCase(Copy(GUIDToString(g), 2, 36)));
  ForceDirectories(FCacheRoot);
  FIdFolder := TPath.Combine(FCacheRoot, 'delphi12.0' + PathDelim + cPackageId);
end;

procedure TPackageCacheIconTests.TearDown;
begin
  if DirectoryExists(FCacheRoot) then
    TDirectory.Delete(FCacheRoot, true);
end;

procedure TPackageCacheIconTests.Saved_Svg_Icon_Is_Returned;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  icon : IPackageIcon;
  found : IPackageIcon;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');
  icon := NewIcon(TPackageIconKind.ikSvg, cSvgContent);

  cache.SavePackageIcon(identity, icon);

  Assert.IsTrue(cache.TryGetPackageIcon(identity, found), 'the saved icon should be found');
  Assert.IsNotNull(found);
  Assert.IsTrue(found.Kind = TPackageIconKind.ikSvg, 'should be an svg');
  Assert.AreEqual(cSvgContent, StreamText(found.Stream), false);
end;

procedure TPackageCacheIconTests.Saved_Png_Icon_Is_Returned;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  icon : IPackageIcon;
  found : IPackageIcon;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');
  icon := NewIcon(TPackageIconKind.ikPng, cPngContent);

  cache.SavePackageIcon(identity, icon);

  Assert.IsTrue(cache.TryGetPackageIcon(identity, found), 'the saved icon should be found');
  Assert.IsNotNull(found);
  Assert.IsTrue(found.Kind = TPackageIconKind.ikPng, 'should be a png');
  Assert.AreEqual(cPngContent, StreamText(found.Stream), false);
end;

procedure TPackageCacheIconTests.Saved_Icon_Is_Kept_Beside_The_Package_Files;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  icon : IPackageIcon;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');
  icon := NewIcon(TPackageIconKind.ikSvg, cSvgContent);

  cache.SavePackageIcon(identity, icon);

  Assert.IsTrue(FileExists(TPath.Combine(FIdFolder, '1.2.3.icon.svg')), 'expected the icon beside the package files');
  //not in a version folder - that would make the version look like a broken install.
  Assert.IsFalse(DirectoryExists(TPath.Combine(FIdFolder, '1.2.3')), 'no version folder should be created');
end;

procedure TPackageCacheIconTests.Saving_Reads_The_Whole_Icon_And_Rewinds_It;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  icon : IPackageIcon;
  found : IPackageIcon;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');
  icon := NewIcon(TPackageIconKind.ikSvg, cSvgContent);
  //wherever the stream happens to be, the whole icon is saved.
  icon.Stream.Position := 5;

  cache.SavePackageIcon(identity, icon);

  Assert.AreEqual(Int64(0), icon.Stream.Position, 'the icon should be rewound for whoever shows it next');
  Assert.IsTrue(cache.TryGetPackageIcon(identity, found));
  Assert.AreEqual(cSvgContent, StreamText(found.Stream), false);
end;

procedure TPackageCacheIconTests.Nothing_Saved_Means_No_Icon_And_Not_Missing;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  found : IPackageIcon;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');

  Assert.IsFalse(cache.TryGetPackageIcon(identity, found), 'nothing was saved');
  Assert.IsNull(found);
  Assert.IsFalse(cache.IsPackageIconMissing(identity), 'nothing was recorded');
end;

procedure TPackageCacheIconTests.Missing_Icon_Is_Remembered;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  found : IPackageIcon;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');

  cache.SavePackageIconMissing(identity);

  Assert.IsTrue(cache.IsPackageIconMissing(identity), 'the source said there is no icon');
  Assert.IsFalse(cache.TryGetPackageIcon(identity, found), 'there is still no icon');
  Assert.IsTrue(FileExists(TPath.Combine(FIdFolder, '1.2.3.icon.none')), 'expected the record beside the package files');
end;

procedure TPackageCacheIconTests.Missing_Icon_Is_Forgotten_After_The_Retry_Period;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  missingFile : string;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');
  cache.SavePackageIconMissing(identity);
  missingFile := TPath.Combine(FIdFolder, '1.2.3.icon.none');
  Assert.IsTrue(FileExists(missingFile), 'expected the record');

  TFile.SetLastWriteTime(missingFile, Now - (cIconMissingRetryDays + 1));

  Assert.IsFalse(cache.IsPackageIconMissing(identity), 'an old answer should be asked again');
end;

procedure TPackageCacheIconTests.Saving_An_Icon_Forgets_That_It_Was_Missing;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  icon : IPackageIcon;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');
  cache.SavePackageIconMissing(identity);
  icon := NewIcon(TPackageIconKind.ikSvg, cSvgContent);

  cache.SavePackageIcon(identity, icon);

  Assert.IsFalse(cache.IsPackageIconMissing(identity), 'it has an icon now');
  Assert.IsFalse(FileExists(TPath.Combine(FIdFolder, '1.2.3.icon.none')), 'the record should be gone');
end;

procedure TPackageCacheIconTests.Installed_Package_Icon_Wins_Over_A_Saved_One;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  icon : IPackageIcon;
  found : IPackageIcon;
  packageFolder : string;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');
  icon := NewIcon(TPackageIconKind.ikPng, cPngContent);
  cache.SavePackageIcon(identity, icon);

  //a finished install - the spec and a receipt in the version folder - with its own icon.
  packageFolder := TPath.Combine(FIdFolder, '1.2.3');
  ForceDirectories(packageFolder);
  TFile.WriteAllText(TPath.Combine(packageFolder, cPackageDspecFile), 'id: Sample.Pkg');
  TFile.WriteAllText(TPath.Combine(packageFolder, cReceiptFileName), 'receipt');
  TFile.WriteAllText(TPath.Combine(packageFolder, cIconFileSVG), cSvgContent);

  Assert.IsTrue(cache.TryGetPackageIcon(identity, found));
  Assert.IsTrue(found.Kind = TPackageIconKind.ikSvg, 'the installed package''s own icon should be used');
  Assert.AreEqual(cSvgContent, StreamText(found.Stream), false);
end;

procedure TPackageCacheIconTests.Saved_Icon_Is_For_Its_Version_Only;
var
  cache : IPackageCache;
  icon : IPackageIcon;
  found : IPackageIcon;
begin
  cache := NewCache(FCacheRoot);
  icon := NewIcon(TPackageIconKind.ikSvg, cSvgContent);

  cache.SavePackageIcon(NewIdentity('1.2.3'), icon);
  cache.SavePackageIconMissing(NewIdentity('1.2.3'));

  Assert.IsFalse(cache.TryGetPackageIcon(NewIdentity('1.2.4'), found), 'another version has no icon saved');
  Assert.IsFalse(cache.IsPackageIconMissing(NewIdentity('1.2.4')), 'nothing was recorded for another version');
end;

procedure TPackageCacheIconTests.Remove_Package_Removes_The_Saved_Icons;
var
  cache : IPackageCache;
  identity : IPackageIdentity;
  icon : IPackageIcon;
  found : IPackageIcon;
begin
  cache := NewCache(FCacheRoot);
  identity := NewIdentity('1.2.3');
  icon := NewIcon(TPackageIconKind.ikSvg, cSvgContent);
  cache.SavePackageIcon(identity, icon);
  //written after the icon, so both are there.
  cache.SavePackageIconMissing(identity);

  cache.RemovePackage(identity);

  Assert.IsFalse(cache.TryGetPackageIcon(identity, found), 'the saved icon should be gone');
  Assert.IsFalse(cache.IsPackageIconMissing(identity), 'the record should be gone');
  Assert.IsFalse(DirectoryExists(FIdFolder), 'nothing should be left in the id folder');
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageCacheIconTests);

end.
