unit DPM.Core.Tests.Utils.DateTime;

// Tests for TDPMDateTimeUtils — the System.DateUtils ISO 8601 polyfill that
// keeps the codebase building on XE2..XE6. The tests must pass on every
// supported compiler, so the assertions focus on the *contract* (round-trip,
// known values, malformed-input handling) and avoid leaning on either side
// of the {$IF CompilerVersion >= 28} gate.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDateTimeUtilsTests = class
  public
    [Test] procedure FormatUtc_KnownDateTime;
    [Test] procedure ParseUtc_KnownDateTime;
    [Test] procedure RoundTripUtc_PreservesValue;
    [Test] procedure RoundTripUtc_PreservesMilliseconds;
    [Test] procedure RoundTripLocal_PreservesValue;

    [Test] procedure ParseAccepts_SpaceSeparator;
    [Test] procedure ParseAccepts_LowercaseT;
    [Test] procedure ParseAccepts_LowercaseZ;
    [Test] procedure ParseAccepts_NoFractionalSeconds;
    [Test] procedure ParseAccepts_OneDigitFraction;
    [Test] procedure ParseAccepts_PositiveOffset;
    [Test] procedure ParseAccepts_NegativeOffset;
    [Test] procedure ParseAccepts_OffsetWithoutColon;

    [Test] procedure ParseRejects_EmptyString;
    [Test] procedure ParseRejects_BogusInput;
    [Test] procedure ParseRejects_NonDigitYear;

    [Test] procedure TryParse_ReturnsFalse_OnBadInput;
    [Test] procedure TryParse_ReturnsTrue_AndValue_OnGoodInput;

    [Test] procedure Offset_Plus_Subtracts_From_UTC;
    [Test] procedure Offset_Minus_Adds_To_UTC;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  DPM.Core.Utils.DateTime;

procedure TDateTimeUtilsTests.FormatUtc_KnownDateTime;
var
  dt : TDateTime;
  s : string;
begin
  dt := EncodeDateTime(2026, 5, 22, 10, 0, 3, 0);
  s := TDPMDateTimeUtils.DateToISO8601(dt, True);
  // Polyfill and RTL must agree on the canonical UTC form.
  Assert.AreEqual('2026-05-22T10:00:03.000Z', s);
end;

procedure TDateTimeUtilsTests.ParseUtc_KnownDateTime;
var
  dt : TDateTime;
  expected : TDateTime;
begin
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-05-22T10:00:03.000Z', True);
  expected := EncodeDateTime(2026, 5, 22, 10, 0, 3, 0);
  Assert.IsTrue(SameDateTime(dt, expected),
    Format('parsed %s vs expected %s',
      [DateTimeToStr(dt), DateTimeToStr(expected)]));
end;

procedure TDateTimeUtilsTests.RoundTripUtc_PreservesValue;
var
  original, recovered : TDateTime;
  s : string;
begin
  original := EncodeDateTime(2024, 11, 30, 23, 59, 58, 0);
  s := TDPMDateTimeUtils.DateToISO8601(original, True);
  recovered := TDPMDateTimeUtils.ISO8601ToDate(s, True);
  Assert.IsTrue(SameDateTime(original, recovered),
    Format('round trip lost data: %s vs %s', [DateTimeToStr(original), DateTimeToStr(recovered)]));
end;

procedure TDateTimeUtilsTests.RoundTripUtc_PreservesMilliseconds;
var
  original, recovered : TDateTime;
  s : string;
begin
  original := EncodeDateTime(2025, 1, 2, 3, 4, 5, 678);
  s := TDPMDateTimeUtils.DateToISO8601(original, True);
  recovered := TDPMDateTimeUtils.ISO8601ToDate(s, True);
  Assert.AreEqual(678, MilliSecondOf(recovered),
    'milliseconds lost in round trip');
end;

procedure TDateTimeUtilsTests.RoundTripLocal_PreservesValue;
var
  original, recovered : TDateTime;
  s : string;
begin
  // Same idea but without the UTC normalisation step — the local-time path
  // must also round-trip without drift.
  original := EncodeDateTime(2026, 7, 4, 12, 0, 0, 0);
  s := TDPMDateTimeUtils.DateToISO8601(original, False);
  recovered := TDPMDateTimeUtils.ISO8601ToDate(s, False);
  Assert.IsTrue(SameDateTime(original, recovered),
    Format('local round trip lost data: %s vs %s',
      [DateTimeToStr(original), DateTimeToStr(recovered)]));
end;

procedure TDateTimeUtilsTests.ParseAccepts_SpaceSeparator;
var
  dt : TDateTime;
begin
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-01-15 12:30:45Z', True);
  Assert.AreEqual(12, HourOf(dt));
  Assert.AreEqual(30, MinuteOf(dt));
  Assert.AreEqual(45, SecondOf(dt));
end;

procedure TDateTimeUtilsTests.ParseAccepts_LowercaseT;
var
  dt : TDateTime;
begin
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-01-15t12:30:45Z', True);
  Assert.AreEqual(12, HourOf(dt));
end;

procedure TDateTimeUtilsTests.ParseAccepts_LowercaseZ;
var
  dt : TDateTime;
begin
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-01-15T12:30:45z', True);
  Assert.AreEqual(45, SecondOf(dt));
end;

procedure TDateTimeUtilsTests.ParseAccepts_NoFractionalSeconds;
var
  dt : TDateTime;
begin
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-05-22T10:00:00Z', True);
  Assert.AreEqual(0, MilliSecondOf(dt));
end;

procedure TDateTimeUtilsTests.ParseAccepts_OneDigitFraction;
var
  dt : TDateTime;
begin
  // ".5s" should parse to 500ms.
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-05-22T10:00:00.5Z', True);
  Assert.AreEqual(500, MilliSecondOf(dt));
end;

procedure TDateTimeUtilsTests.ParseAccepts_PositiveOffset;
var
  dt : TDateTime;
begin
  // 10:00 at +02:00 -> 08:00 UTC
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-05-22T10:00:00+02:00', True);
  Assert.AreEqual(8, HourOf(dt));
end;

procedure TDateTimeUtilsTests.ParseAccepts_NegativeOffset;
var
  dt : TDateTime;
begin
  // 10:00 at -05:00 -> 15:00 UTC
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-05-22T10:00:00-05:00', True);
  Assert.AreEqual(15, HourOf(dt));
end;

procedure TDateTimeUtilsTests.ParseAccepts_OffsetWithoutColon;
var
  dt : TDateTime;
begin
  // ISO 8601 allows "+0200" as well as "+02:00".
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-05-22T10:00:00+0200', True);
  Assert.AreEqual(8, HourOf(dt));
end;

procedure TDateTimeUtilsTests.ParseRejects_EmptyString;
begin
  Assert.WillRaise(
    procedure begin TDPMDateTimeUtils.ISO8601ToDate('', True); end,
    EConvertError);
end;

procedure TDateTimeUtilsTests.ParseRejects_BogusInput;
begin
  Assert.WillRaise(
    procedure begin TDPMDateTimeUtils.ISO8601ToDate('not-a-date', True); end,
    EConvertError);
end;

procedure TDateTimeUtilsTests.ParseRejects_NonDigitYear;
begin
  Assert.WillRaise(
    procedure begin TDPMDateTimeUtils.ISO8601ToDate('abcd-05-22T10:00:00Z', True); end,
    EConvertError);
end;

procedure TDateTimeUtilsTests.TryParse_ReturnsFalse_OnBadInput;
var
  dt : TDateTime;
begin
  Assert.IsFalse(TDPMDateTimeUtils.TryISO8601ToDate('garbage', dt, True));
end;

procedure TDateTimeUtilsTests.TryParse_ReturnsTrue_AndValue_OnGoodInput;
var
  dt : TDateTime;
begin
  Assert.IsTrue(TDPMDateTimeUtils.TryISO8601ToDate('2026-05-22T10:00:03Z', dt, True));
  Assert.AreEqual(2026, YearOf(dt));
  Assert.AreEqual(5,    MonthOf(dt));
  Assert.AreEqual(22,   DayOf(dt));
end;

procedure TDateTimeUtilsTests.Offset_Plus_Subtracts_From_UTC;
var
  dt : TDateTime;
begin
  // 14:30 at +05:30 (India) -> 09:00 UTC
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-06-01T14:30:00+05:30', True);
  Assert.AreEqual(9, HourOf(dt));
  Assert.AreEqual(0, MinuteOf(dt));
end;

procedure TDateTimeUtilsTests.Offset_Minus_Adds_To_UTC;
var
  dt : TDateTime;
begin
  // 18:00 at -08:00 (Pacific) -> 02:00 UTC next day
  dt := TDPMDateTimeUtils.ISO8601ToDate('2026-06-01T18:00:00-08:00', True);
  Assert.AreEqual(2, HourOf(dt));
  Assert.AreEqual(2, DayOf(dt));
end;

initialization
  TDUnitX.RegisterTestFixture(TDateTimeUtilsTests);

end.
