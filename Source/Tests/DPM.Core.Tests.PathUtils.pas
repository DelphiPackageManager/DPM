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
    procedure TestCompressRelativePath(const input, expected : string);

    [Test]
    [TestCase('UNCBase1', '\\test,\foo\..\bar,\\test\bar')]
    [TestCase('UNCBase2', '\\test,foo\..\bar,\\test\bar')]
    [TestCase('UNCBase3', '\\test\,foo\..\bar,\\test\bar')]
    [TestCase('UNCBase4', '\\test\,\foo\..\bar,\\test\bar')]
    [TestCase('Rooted', 'c:\test\,foo\..\bar,c:\test\bar')]
    procedure TestCompressRelativePathWithBase(const base, input, expected : string);

    [Test]
    procedure TestIsRelativePath;

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

initialization
  TDUnitX.RegisterTestFixture(TPathUtilsTests);

end.
