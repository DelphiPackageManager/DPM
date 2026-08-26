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
  published
    procedure Join_Empty_ReturnsEmptyString;
    procedure Join_SingleValue_HasNoSeparator;
    procedure Join_MultipleValues_AreSeparated;
    procedure Join_PreservesEmptyElements;
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

initialization
  TDUnitX.RegisterTestFixture(TStringUtilsTests);

end.
