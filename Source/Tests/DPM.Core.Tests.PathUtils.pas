unit DPM.Core.Tests.PathUtils;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TPathUtilsTests = class
  public
    [Test]
    [TestCase('Simple', 'c:\test\foo\..\bar,c:\test\bar')]
    [TestCase('SimpleDot', 'c:\test\foo\.\bar,c:\test\foo\bar')]
    [TestCase('UNC', '\\test\foo\..\bar,\\test\bar')]
    [TestCase('Unrooted', 'test\foo\..\bar,test\bar')]
    [TestCase('ForwardSlashDot', 'c:\test\.\foo\bar,c:\test\foo\bar')]
    [TestCase('MixedSlashesDot', 'c:\test\.\packages/Rad Studio 12/X.dproj,c:\test\packages\Rad Studio 12\X.dproj')]
    [TestCase('AllForwardDot', 'c:/test/./foo/bar,c:\test\foo\bar')]
    procedure TestCompressRelativePath(const input, expected : string);

    [Test]
    [TestCase('UNCBase1', '\\test,\foo\..\bar,\\test\bar')]
    [TestCase('UNCBase2', '\\test,foo\..\bar,\\test\bar')]
    [TestCase('UNCBase3', '\\test\,foo\..\bar,\\test\bar')]
    [TestCase('UNCBase4', '\\test\,\foo\..\bar,\\test\bar')]
    [TestCase('Rooted', 'c:\test\,foo\..\bar,c:\test\bar')]
    [TestCase('PosixRelative', 'c:\test\,./foo/bar,c:\test\foo\bar')]
    procedure TestCompressRelativePathWithBase(const base, input, expected : string);

    [Test]
    procedure TestIsRelativePath;

    [Test]
    [TestCase('Subfolder', './source/*.pas,source')]
    [TestCase('Recursive', './source/**/*.pas,source')]
    [TestCase('Nested', './a/b/*.pas,a\b')]
    [TestCase('BackslashInput', '.\src\*.pas,src')]
    [TestCase('NoDotSlash', 'source/*.pas,source')]
    [TestCase('PartialFileName', './source/Test*.pas,source')]
    [TestCase('Question', './source/test?.pas,source')]
    procedure TestGlobBaseDir(const input, expected : string);

    //cases whose expected result is '' (root / single file) - kept out of TestCase
    //to avoid trailing-empty-value parsing differences.
    [Test]
    procedure TestGlobBaseDir_RootOrFile_ReturnsEmpty;

    [Test]
    [TestCase('NewExt', 'foo.dspec,True')]
    [TestCase('LegacyExt', 'foo.dspec.yaml,True')]
    [TestCase('NewExtWithPath', 'c:\packages\MyLib.dspec,True')]
    [TestCase('LegacyExtWithPath', 'c:\packages\MyLib.dspec.yaml,True')]
    [TestCase('PlainYaml', 'foo.yaml,False')]
    [TestCase('UnrelatedExt', 'foo.txt,False')]
    [TestCase('StrayPrefix', 'foo.dspecbak,False')]
    procedure TestIsDspecFile(const fileName : string; const expected : boolean);

    [Test]
    [TestCase('RelativeForwardSlash', 'c:\proj,sub/x.dproj,c:\proj\sub\x.dproj')]
    [TestCase('RelativeParentForwardSlash', 'c:\proj\build,../Source/X.dproj,c:\proj\Source\X.dproj')]
    [TestCase('RelativeDotSlash', 'c:\proj,./X.dproj,c:\proj\X.dproj')]
    [TestCase('RelativeBackslash', 'c:\proj,sub\x.dproj,c:\proj\sub\x.dproj')]
    [TestCase('RootedBackslash', 'c:\proj,d:\abs\X.dproj,d:\abs\X.dproj')]
    //the case that broke `dpm sbom --outdir` - a rooted path written with posix separators
    //is not matched by IsPathRooted, so it used to get appended to the base.
    [TestCase('RootedForwardSlash', 'c:\proj,d:/other/X.dproj,d:\other\X.dproj')]
    [TestCase('BaseTrailingDelimiter', 'c:\proj\,sub/x.dproj,c:\proj\sub\x.dproj')]
    [TestCase('UNCBase', '\\srv\share,sub/x.dproj,\\srv\share\sub\x.dproj')]
    procedure TestToAbsolutePath(const base, input, expected : string);

    [Test]
    procedure TestToAbsolutePath_Empty_ReturnsEmpty;

    //the case that broke `dpm restore <group>` when the group path used posix separators -
    //ExtractFilePath returned 'c:' so every member dproj was looked for under the drive root.
    [Test]
    [TestCase('GroupForwardSlash', 'c:/src/all.groupproj,IDE/App.dproj,c:\src\IDE\App.dproj')]
    [TestCase('GroupBackslash', 'c:\src\all.groupproj,IDE\App.dproj,c:\src\IDE\App.dproj')]
    [TestCase('GroupMixed', 'c:/src/all.groupproj,IDE\App.dproj,c:\src\IDE\App.dproj')]
    [TestCase('MemberParentRelative', 'c:/src/build/all.groupproj,../IDE/App.dproj,c:\src\IDE\App.dproj')]
    [TestCase('MemberDotSlash', 'c:/src/all.groupproj,./App.dproj,c:\src\App.dproj')]
    [TestCase('MemberRootedBackslash', 'c:/src/all.groupproj,d:\abs\App.dproj,d:\abs\App.dproj')]
    [TestCase('MemberRootedForwardSlash', 'c:/src/all.groupproj,d:/abs/App.dproj,d:\abs\App.dproj')]
    [TestCase('UNCGroup', '\\srv\share\all.groupproj,IDE/App.dproj,\\srv\share\IDE\App.dproj')]
    procedure TestResolveRelativeToFile(const baseFile, path, expected : string);

    [Test]
    procedure TestResolveRelativeToFile_EmptyPath_ReturnsEmpty;

    //Paths in a package spec (build/design projects, search paths, copy patterns) must stay inside the
    //package folder - the folder itself counts, a sibling that merely shares its name as a prefix doesn't.
    [Test]
    [TestCase('Child', 'c:\cache\pkg,c:\cache\pkg\src\a.pas,True')]
    [TestCase('FolderItself', 'c:\cache\pkg,c:\cache\pkg,True')]
    [TestCase('FolderTrailingDelimiter', 'c:\cache\pkg\,c:\cache\pkg,True')]
    [TestCase('PathTrailingDelimiter', 'c:\cache\pkg,c:\cache\pkg\src\,True')]
    [TestCase('IgnoresCase', 'c:\cache\pkg,C:\CACHE\PKG\Src\A.pas,True')]
    [TestCase('DotDotStaysInside', 'c:\cache\pkg,c:\cache\pkg\src\..\lib\a.dcu,True')]
    [TestCase('ForwardSlashes', 'c:\cache\pkg,c:/cache/pkg/src/a.pas,True')]
    [TestCase('SiblingWithSamePrefix', 'c:\cache\pkg,c:\cache\pkg2\a.pas,False')]
    [TestCase('Parent', 'c:\cache\pkg,c:\cache,False')]
    [TestCase('DotDotEscapes', 'c:\cache\pkg,c:\cache\pkg\..\other\a.pas,False')]
    [TestCase('DotDotEscapesFromDeep', 'c:\cache\pkg,c:\cache\pkg\src\..\..\..\Windows\a.dll,False')]
    [TestCase('OtherDrive', 'c:\cache\pkg,d:\cache\pkg\a.pas,False')]
    [TestCase('Unc', 'c:\cache\pkg,\\server\share\a.pas,False')]
    procedure TestIsPathInFolder(const folder, path : string; const expected : boolean);

    [Test]
    procedure TestIsPathInFolder_Empty_IsFalse;

    //A spec path is relative to the package folder, '/' or '\' separated, and may start with one separator
    //for the package root ('/packages/X.dproj').
    [Test]
    [TestCase('LeadingSlash', 'c:\cache\pkg,/packages/X.dproj,c:\cache\pkg\packages\X.dproj')]
    [TestCase('LeadingBackslash', 'c:\cache\pkg,\packages\X.dproj,c:\cache\pkg\packages\X.dproj')]
    [TestCase('DotSlash', 'c:\cache\pkg,.\source\Pkg.dproj,c:\cache\pkg\source\Pkg.dproj')]
    [TestCase('Bare', 'c:\cache\pkg,source\Pkg.dproj,c:\cache\pkg\source\Pkg.dproj')]
    [TestCase('DotDotStaysInside', 'c:\cache\pkg,source/../packages/X.dproj,c:\cache\pkg\packages\X.dproj')]
    [TestCase('FolderTrailingDelimiter', 'c:\cache\pkg\,/packages/X.dproj,c:\cache\pkg\packages\X.dproj')]
    [TestCase('Folder', 'c:\cache\pkg,source/,c:\cache\pkg\source\')]
    procedure TestTryResolveInFolder_Inside(const folder, path, expected : string);

    [Test]
    [TestCase('Parent', 'c:\cache\pkg,../Evil.dproj')]
    [TestCase('LeadingSlashParent', 'c:\cache\pkg,/../../Evil.dproj')]
    [TestCase('DeepParent', 'c:\cache\pkg,source/../../../../Windows/System32/Evil.dproj')]
    [TestCase('SiblingWithSamePrefix', 'c:\cache\pkg,../pkg2/Evil.dproj')]
    //absolute paths are refused even when they point inside the package
    [TestCase('AbsoluteInside', 'c:\cache\pkg,c:\cache\pkg\Pkg.dproj')]
    [TestCase('AbsoluteForwardSlash', 'c:\cache\pkg,c:/Windows/Evil.dproj')]
    [TestCase('DriveRelative', 'c:\cache\pkg,c:Evil.dproj')]
    [TestCase('UncForwardSlash', 'c:\cache\pkg,//server/share/Evil.dproj')]
    [TestCase('Unc', 'c:\cache\pkg,\\server\share\Evil.dproj')]
    [TestCase('LongPathPrefix', 'c:\cache\pkg,\\?\c:\cache\pkg\Pkg.dproj')]
    procedure TestTryResolveInFolder_Refused(const folder, path : string);
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.Path;

{ TPathUtilsTests }

procedure TPathUtilsTests.TestCompressRelativePath(const input,  expected: string);
var
  actual : string;
begin
  actual := TPathUtils.CompressRelativePath(input);
  Assert.AreEqual(expected, actual);
end;

procedure TPathUtilsTests.TestCompressRelativePathWithBase(const base, input, expected: string);
var
  actual : string;
begin
  actual := TPathUtils.CompressRelativePath(Trim(base), Trim(input));
  Assert.AreEqual(Trim(expected), actual);
end;

function IsRelativePath2(const Path: string): Boolean;
var
  L: Integer;
begin
  L := Length(Path);
  Result := (L > 0) and (Path[1] <> PathDelim)
    {$IFDEF MSWINDOWS}and (L > 1) and (Path[2] <> ':'){$ENDIF MSWINDOWS};
end;

procedure TPathUtilsTests.TestIsRelativePath;
begin
//  Assert.IsTrue((IsRelativePath2('\\..\test')));
    Assert.IsFalse((TPathUtils.IsRelativePath('\\..\test')));
end;

procedure TPathUtilsTests.TestGlobBaseDir(const input, expected : string);
begin
  Assert.AreEqual(Trim(expected), TPathUtils.GlobBaseDir(Trim(input)));
end;

procedure TPathUtilsTests.TestGlobBaseDir_RootOrFile_ReturnsEmpty;
begin
  Assert.AreEqual('', TPathUtils.GlobBaseDir('./*.pas'));
  Assert.AreEqual('', TPathUtils.GlobBaseDir('*.pas'));
  Assert.AreEqual('', TPathUtils.GlobBaseDir('./LICENSE'));
end;

procedure TPathUtilsTests.TestToAbsolutePath(const base, input, expected : string);
begin
  Assert.AreEqual(Trim(expected), TPathUtils.ToAbsolutePath(Trim(input), Trim(base)));
end;

procedure TPathUtilsTests.TestToAbsolutePath_Empty_ReturnsEmpty;
begin
  Assert.AreEqual('', TPathUtils.ToAbsolutePath('', 'c:\proj'));
  Assert.AreEqual('', TPathUtils.ToAbsolutePath('   ', 'c:\proj'));
end;

procedure TPathUtilsTests.TestResolveRelativeToFile(const baseFile, path, expected : string);
begin
  Assert.AreEqual(Trim(expected), TPathUtils.ResolveRelativeToFile(Trim(baseFile), Trim(path)), false);
end;

procedure TPathUtilsTests.TestResolveRelativeToFile_EmptyPath_ReturnsEmpty;
begin
  Assert.AreEqual('', TPathUtils.ResolveRelativeToFile('c:\src\all.groupproj', ''), false);
  Assert.AreEqual('', TPathUtils.ResolveRelativeToFile('c:\src\all.groupproj', '   '), false);
end;

procedure TPathUtilsTests.TestIsDspecFile(const fileName : string; const expected : boolean);
begin
  Assert.AreEqual(expected, TPathUtils.IsDspecFile(Trim(fileName)));
end;

procedure TPathUtilsTests.TestIsPathInFolder(const folder, path : string; const expected : boolean);
begin
  Assert.AreEqual(expected, TPathUtils.IsPathInFolder(folder, path), folder + ' / ' + path);
end;

procedure TPathUtilsTests.TestIsPathInFolder_Empty_IsFalse;
begin
  Assert.IsFalse(TPathUtils.IsPathInFolder('', 'c:\cache\pkg\a.pas'), 'no folder');
  Assert.IsFalse(TPathUtils.IsPathInFolder('c:\cache\pkg', ''), 'no path');
end;

procedure TPathUtilsTests.TestTryResolveInFolder_Inside(const folder, path, expected : string);
var
  resolved : string;
begin
  Assert.IsTrue(TPathUtils.TryResolveInFolder(folder, path, resolved), path);
  Assert.AreEqual(expected, resolved, true, path);
end;

procedure TPathUtilsTests.TestTryResolveInFolder_Refused(const folder, path : string);
var
  resolved : string;
begin
  Assert.IsFalse(TPathUtils.TryResolveInFolder(folder, path, resolved), path + ' resolved to ' + resolved);
  Assert.AreEqual('', resolved, 'nothing to use when refused');
end;

initialization
  TDUnitX.RegisterTestFixture(TPathUtilsTests);

end.
