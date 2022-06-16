{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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

unit DependencyVersionTests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TVersionRangeTests = class
  public
    [Test]
    procedure Will_Fail_on_empty_string;

    [Test]
    procedure Will_Fail_on_fixed_major_minor;

    [Test]
    procedure Can_Parse_ValidFixed_Version;

    [Test]
    procedure Will_Normalize_To_Fixed;


    [Test]
    procedure Can_Parse_ExclusiveRange;

    [Test]
    procedure Can_Parse_ExclusiveRange_OpenEnded;

    [Test]
    procedure Can_Parse_ExclusiveRange_OpenStart;


    [Test]
    procedure Will_Fail_On_Exclusive_Range_Open_No_Gap;

    [Test]
    procedure Will_Fail_On_ExclusiveRange_NoComma;

    [Test]
    procedure Can_Parse_InclusiveRange;

    [Test]
    procedure Will_Fail_On_Empty_Exlusive_Range;

    [Test]
    procedure Will_Fail_On_InvalidSemver;

    [Test]
    procedure Will_Fail_On_Invalid_Float;
  end;

implementation

uses
  DPM.Core.Types,
  DPM.Core.Dependency.Version;

{ TdepVersionTests }

procedure TVersionRangeTests.Can_Parse_ExclusiveRange;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsTrue(TVersionRange.TryParseWithError('(1.0.1,1.0.3)', depVersion, error), error);
  Assert.IsTrue(depVersion.IsValid);
  Assert.IsFalse(depVersion.MinVersionIsInclusive);
  Assert.IsFalse(depVersion.MaxVersionIsInclusive);
end;

procedure TVersionRangeTests.Can_Parse_ExclusiveRange_OpenEnded;
var
  depVersion :TVersionRange;
  maxVersion :TPackageVersion;
  error :string;
begin
  Assert.IsTrue(TVersionRange.TryParseWithError('(1.0.1,)', depVersion, error), error);
  Assert.IsTrue(depVersion.IsValid);
  Assert.IsFalse(depVersion.MinVersionIsInclusive);
  maxVersion := TPackageVersion.Create(1, 0, High(word));
  Assert.IsTrue(maxVersion = depVersion.MaxVersion);


end;

procedure TVersionRangeTests.Can_Parse_ExclusiveRange_OpenStart;
var
  depVersion :TVersionRange;
  minVersion :TPackageVersion;
  error :string;
begin
  Assert.IsTrue(TVersionRange.TryParseWithError('(,1.0.2)', depVersion, error), error);
  Assert.IsTrue(depVersion.IsValid);
  Assert.IsFalse(depVersion.MinVersionIsInclusive);
  minVersion := TPackageVersion.Create(1, 0, 0);
  Assert.IsTrue(minVersion = depVersion.MinVersion);
end;

procedure TVersionRangeTests.Can_Parse_InclusiveRange;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsTrue(TVersionRange.TryParseWithError('[1.0.1, 1.0.3]', depVersion, error), error);
  Assert.IsTrue(depVersion.IsValid);
  Assert.IsTrue(depVersion.MinVersionIsInclusive);
  Assert.IsTrue(depVersion.MaxVersionIsInclusive);
  Assert.AreEqual('[1.0.1, 1.0.3]', depVersion.ToString);
end;

procedure TVersionRangeTests.Can_Parse_ValidFixed_Version;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsTrue(TVersionRange.TryParseWithError('1.0.1', depVersion, error), error);
  Assert.IsTrue(depVersion.IsValid);
  Assert.IsTrue(depVersion.IsFixed);
  Assert.AreEqual<TPackageVersion>(depVersion.MinVersion, depVersion.MaxVersion);
end;

procedure TVersionRangeTests.Will_Fail_On_Empty_Exlusive_Range;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsFalse(TVersionRange.TryParseWithError('(1.0.1,1.0.2)', depVersion, error), error);
  Assert.IsFalse(depVersion.IsValid);

end;

procedure TVersionRangeTests.Will_Fail_on_empty_string;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsFalse(TVersionRange.TryParseWithError('', depVersion, error), error);
end;

procedure TVersionRangeTests.Will_Fail_On_ExclusiveRange_NoComma;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsFalse(TVersionRange.TryParseWithError('(1.0.1)', depVersion, error), error);
  Assert.IsFalse(depVersion.IsValid);
end;

procedure TVersionRangeTests.Will_Fail_On_Exclusive_Range_Open_No_Gap;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsFalse(TVersionRange.TryParseWithError('(,1.0.1)', depVersion, error), error);
  Assert.IsFalse(depVersion.IsValid);

end;

procedure TVersionRangeTests.Will_Fail_on_fixed_major_minor;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsFalse(TVersionRange.TryParseWithError('1.0', depVersion, error), error);
end;

procedure TVersionRangeTests.Will_Fail_On_InvalidSemver;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsFalse(TVersionRange.TryParseWithError('(1.0.1.2,1.0.2.2)', depVersion, error), error);
  Assert.IsFalse(depVersion.IsValid);
end;

procedure TVersionRangeTests.Will_Fail_On_Invalid_Float;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsFalse(TVersionRange.TryParseWithError('[1.0.1,1.2.3]', depVersion, error), error);
  Assert.IsFalse(depVersion.IsValid);
end;

procedure TVersionRangeTests.Will_Normalize_To_Fixed;
var
  depVersion :TVersionRange;
  error :string;
begin
  Assert.IsTrue(TVersionRange.TryParseWithError('[1.0.1,1.0.2)', depVersion, error), error);
  Assert.IsTrue(depVersion.IsValid);
  Assert.IsTrue(depVersion.MinVersionIsInclusive);
  Assert.IsFalse(depVersion.MaxVersionIsInclusive);
  depVersion.Normalize;
  Assert.IsTrue(depVersion.IsFixed);
  Assert.AreEqual('1.0.1', depVersion.ToString);
end;

initialization
  TDUnitX.RegisterTestFixture(TVersionRangeTests);

end.

