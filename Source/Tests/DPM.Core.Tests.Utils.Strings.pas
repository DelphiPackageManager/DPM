{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Core.Tests.Utils.Strings;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TStringUtilsTests = class
  public
    //Package links (project, licence, repository) are opened by the IDE when clicked - only web links
    //may be, anything else (file:, a \\server\share\program.exe path, shell:) could start a program.
    [Test]
    [TestCase('Http', 'http://example.com')]
    [TestCase('Https', 'https://example.com')]
    [TestCase('PathAndQuery', 'https://github.com/VSoft/DPM/blob/master/LICENSE.txt?raw=true#L1')]
    [TestCase('UpperCaseScheme', 'HTTPS://EXAMPLE.COM')]
    [TestCase('Port', 'http://localhost:8080/x')]
    procedure IsWebUrl_AcceptsWebLinks(const value : string);

    [Test]
    [TestCase('FileUrl', 'file:///C:/Windows/System32/calc.exe')]
    [TestCase('FileHost', 'file://server/share/program.exe')]
    [TestCase('UncPath', '\\server\share\program.exe')]
    [TestCase('LocalPath', 'C:\Windows\notepad.exe')]
    [TestCase('RelativePath', 'program.exe')]
    [TestCase('JavaScript', 'javascript:alert(1)')]
    [TestCase('Ftp', 'ftp://example.com/file')]
    [TestCase('Mailto', 'mailto:someone@example.com')]
    [TestCase('Shell', 'shell:startup')]
    [TestCase('SearchMs', 'search-ms:query=x')]
    [TestCase('BackslashScheme', 'http:\\example.com')]
    [TestCase('SchemeOnly', 'https://')]
    [TestCase('NoHost', 'https:///path')]
    [TestCase('SchemeInside', 'xhttps://example.com')]
    procedure IsWebUrl_RejectsEverythingElse(const value : string);

    //spaces and quotes have no place in a url - a browser's command line is built around it.
    [Test]
    procedure IsWebUrl_RejectsWhitespaceQuotesAndControlCharacters;
  published
    procedure Join_Empty_ReturnsEmptyString;
    procedure Join_SingleValue_HasNoSeparator;
    procedure Join_MultipleValues_AreSeparated;
    procedure Join_PreservesEmptyElements;
    //the --sources value - a source's name may contain a space.
    procedure SplitTrimmed_KeepsSpacesInsideItems;
    procedure SplitTrimmed_TrimsAndDropsEmptyItems;
  end;

implementation

uses
  DPM.Core.Utils.Strings;

//TStringUtils.Join stands in for string.Join, which does not exist before XE3 - the Core units
//that use it are compiled into the XE2 IDE plugin.

procedure TStringUtilsTests.Join_Empty_ReturnsEmptyString;
var
  values : TArray<string>;
begin
  SetLength(values, 0);
  Assert.AreEqual('', TStringUtils.Join('; ', values));
end;

procedure TStringUtilsTests.Join_SingleValue_HasNoSeparator;
var
  values : TArray<string>;
begin
  SetLength(values, 1);
  values[0] := 'one';
  Assert.AreEqual('one', TStringUtils.Join('; ', values), false);
end;

procedure TStringUtilsTests.Join_MultipleValues_AreSeparated;
var
  values : TArray<string>;
begin
  SetLength(values, 3);
  values[0] := 'one';
  values[1] := 'two';
  values[2] := 'three';
  Assert.AreEqual('one; two; three', TStringUtils.Join('; ', values), false);
end;

procedure TStringUtilsTests.Join_PreservesEmptyElements;
var
  values : TArray<string>;
begin
  //An empty element still occupies a slot - matches string.Join, and means a caller can see that
  //something in the list was blank rather than silently losing it.
  SetLength(values, 3);
  values[0] := 'a';
  values[1] := '';
  values[2] := 'c';
  Assert.AreEqual('a||c', TStringUtils.Join('|', values), false);
end;

procedure TStringUtilsTests.IsWebUrl_AcceptsWebLinks(const value : string);
begin
  Assert.IsTrue(TStringUtils.IsWebUrl(value), value);
end;

procedure TStringUtilsTests.IsWebUrl_RejectsEverythingElse(const value : string);
begin
  Assert.IsFalse(TStringUtils.IsWebUrl(value), value);
end;

procedure TStringUtilsTests.IsWebUrl_RejectsWhitespaceQuotesAndControlCharacters;
begin
  Assert.IsFalse(TStringUtils.IsWebUrl(''), 'empty');
  Assert.IsFalse(TStringUtils.IsWebUrl(' https://example.com'), 'leading space');
  Assert.IsFalse(TStringUtils.IsWebUrl('https://example.com/a b'), 'space');
  Assert.IsFalse(TStringUtils.IsWebUrl('https://example.com/" --some-switch "'), 'quote');
  Assert.IsFalse(TStringUtils.IsWebUrl('https://example.com/'#9'x'), 'tab');
  Assert.IsFalse(TStringUtils.IsWebUrl('https://example.com/'#13#10'x'), 'line break');
end;

procedure TStringUtilsTests.SplitTrimmed_KeepsSpacesInsideItems;
var
  items : TArray<string>;
begin
  items := TStringUtils.SplitTrimmed('DPM,My Private Feed', ',');
  Assert.AreEqual(2, Length(items));
  Assert.AreEqual('DPM', items[0], false);
  Assert.AreEqual('My Private Feed', items[1], false);
end;

procedure TStringUtilsTests.SplitTrimmed_TrimsAndDropsEmptyItems;
var
  items : TArray<string>;
begin
  items := TStringUtils.SplitTrimmed(' DPM , ,My Feed ,', ',');
  Assert.AreEqual(2, Length(items));
  Assert.AreEqual('DPM', items[0], false);
  Assert.AreEqual('My Feed', items[1], false);
  Assert.AreEqual(0, Length(TStringUtils.SplitTrimmed('', ',')));
end;

initialization
  TDUnitX.RegisterTestFixture(TStringUtilsTests);

end.
