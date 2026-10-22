{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

    [Test]
    procedure IsBundledSentinel_True_For_Sentinel;

    [Test]
    procedure IsBundledSentinel_False_For_Normal_Ranges;

    [Test]
    procedure IsCompatibleNewer_True_Above_Range_On_Same_Major;

    [Test]
    procedure IsCompatibleNewer_False_Within_Range;

    [Test]
    procedure IsCompatibleNewer_False_Below_Range;

    [Test]
    procedure IsCompatibleNewer_False_On_Another_Major;

    [Test]
    procedure IsCompatibleNewer_True_At_Exclusive_Max;

    [Test]
    procedure IsSatisfiedBy_Honours_Exclusive_Bounds;

    [Test]
    procedure No_Intersection_At_Exclusive_Max;
  end;

implementation

uses
  DPM.Core.Types,
  DPM.Core.Constants,
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

procedure TVersionRangeTests.IsBundledSentinel_True_For_Sentinel;
var
  fromConst : TVersionRange;
  fromFixed : TVersionRange;
  fromRange : TVersionRange;
begin
  //the sentinel parses both as a single fixed version and as an explicit fixed range.
  fromConst := TVersionRange.Parse(cBundledDependencyVersion);
  Assert.IsTrue(fromConst.IsBundledSentinel, 'cBundledDependencyVersion must be detected as the sentinel');

  fromFixed := TVersionRange.Parse('999.999.999');
  Assert.IsTrue(fromFixed.IsBundledSentinel, '999.999.999 must be detected as the sentinel');

  fromRange := TVersionRange.Parse('[999.999.999,999.999.999]');
  Assert.IsTrue(fromRange.IsBundledSentinel, '[999.999.999,999.999.999] must be detected as the sentinel');
end;

procedure TVersionRangeTests.IsBundledSentinel_False_For_Normal_Ranges;
var
  fixed : TVersionRange;
  openEnded : TVersionRange;
begin
  fixed := TVersionRange.Parse('1.0.0');
  Assert.IsFalse(fixed.IsBundledSentinel, 'a normal fixed version is not the sentinel');

  openEnded := TVersionRange.Parse('[1.0.0,]');
  Assert.IsFalse(openEnded.IsBundledSentinel, 'a normal range is not the sentinel');
end;

procedure TVersionRangeTests.IsCompatibleNewer_True_Above_Range_On_Same_Major;
var
  range : TVersionRange;
begin
  //[1.2.4,] only floats the patch - 1.3.0 is outside it, but a minor bump is compatible by semver.
  range := TVersionRange.Parse('[1.2.4,]');
  Assert.IsTrue(range.IsCompatibleNewer(TPackageVersion.Parse('1.3.0')), 'a newer minor on the same major');
  Assert.IsTrue(range.IsCompatibleNewer(TPackageVersion.Parse('1.12.1')), 'a much newer minor on the same major');

  range := TVersionRange.Parse('1.2.4');
  Assert.IsTrue(range.IsCompatibleNewer(TPackageVersion.Parse('1.2.5')), 'a newer patch than an exact pin');
end;

procedure TVersionRangeTests.IsCompatibleNewer_False_Within_Range;
var
  range : TVersionRange;
begin
  range := TVersionRange.Parse('[1.2.4,]');
  Assert.IsFalse(range.IsCompatibleNewer(TPackageVersion.Parse('1.2.4')), 'the minimum is in the range');
  Assert.IsFalse(range.IsCompatibleNewer(TPackageVersion.Parse('1.2.9')), 'a floating patch is in the range');
end;

procedure TVersionRangeTests.IsCompatibleNewer_False_Below_Range;
var
  range : TVersionRange;
begin
  range := TVersionRange.Parse('[1.2.4,]');
  Assert.IsFalse(range.IsCompatibleNewer(TPackageVersion.Parse('1.2.3')), 'an older patch');
  Assert.IsFalse(range.IsCompatibleNewer(TPackageVersion.Parse('1.1.9')), 'an older minor');
  Assert.IsFalse(range.IsCompatibleNewer(TPackageVersion.Parse('0.9.0')), 'an older major');
end;

procedure TVersionRangeTests.IsCompatibleNewer_False_On_Another_Major;
var
  range : TVersionRange;
begin
  range := TVersionRange.Parse('[1.2.4,]');
  Assert.IsFalse(range.IsCompatibleNewer(TPackageVersion.Parse('2.0.0')), 'a new major is a breaking change');
end;

procedure TVersionRangeTests.IsCompatibleNewer_True_At_Exclusive_Max;
var
  range : TVersionRange;
begin
  //the max itself is outside an exclusive range - above it, not below.
  range := TVersionRange.Parse('[1.2.4,1.2.9)');
  Assert.IsTrue(range.IsCompatibleNewer(TPackageVersion.Parse('1.2.9')));
end;

procedure TVersionRangeTests.IsSatisfiedBy_Honours_Exclusive_Bounds;
var
  range : TVersionRange;
begin
  range := TVersionRange.Parse('(1.0.12,1.0.57)');
  Assert.IsFalse(range.IsSatisfiedBy(TPackageVersion.Parse('1.0.12')), 'exclusive min');
  Assert.IsTrue(range.IsSatisfiedBy(TPackageVersion.Parse('1.0.13')));
  Assert.IsTrue(range.IsSatisfiedBy(TPackageVersion.Parse('1.0.56')));
  Assert.IsFalse(range.IsSatisfiedBy(TPackageVersion.Parse('1.0.57')), 'exclusive max');

  range := TVersionRange.Parse('[1.0.12,1.0.57)');
  Assert.IsTrue(range.IsSatisfiedBy(TPackageVersion.Parse('1.0.12')), 'inclusive min');
  Assert.IsFalse(range.IsSatisfiedBy(TPackageVersion.Parse('1.0.57')), 'exclusive max');

  range := TVersionRange.Parse('[1.0.12,1.0.57]');
  Assert.IsTrue(range.IsSatisfiedBy(TPackageVersion.Parse('1.0.57')), 'inclusive max');
end;

procedure TVersionRangeTests.No_Intersection_At_Exclusive_Max;
var
  left : TVersionRange;
  right : TVersionRange;
  intersection : TVersionRange;
begin
  //1.0.5 is the left range's exclusive max - nothing satisfies both.
  left := TVersionRange.Parse('[1.0.0,1.0.5)');
  right := TVersionRange.Parse('1.0.5');
  Assert.IsFalse(left.TryGetIntersectingRange(right, intersection));
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

