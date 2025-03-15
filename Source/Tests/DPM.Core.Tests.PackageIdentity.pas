unit DPM.Core.Tests.PackageIdentity;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DUnitX.TestFramework;

type
  TPackageIdentityTestFixtue = class
  private
    FLogger : ILogger;
  public

    [SetupFixture]
    procedure FixtureSetup;

    //same thing failed randomly at runtime with a regex error
    //makes no sense.
    [Test]
    [TestCase('Spring4D.Core', 'Spring4D.Core-12.0-Win32-2.0.1')]
    procedure TestCreateFromString(const filename : string);
  end;

implementation

uses
  TestLogger,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes;


{ TPackageIdentityTestFixtue }

procedure TPackageIdentityTestFixtue.FixtureSetup;
begin
  FLogger := TTestLogger.Create;
end;

procedure TPackageIdentityTestFixtue.TestCreateFromString(const filename: string);
var
  identity : IPackageIdentity;
begin

  Assert.IsTrue(TPackageIdentity.TryCreateFromString(FLogger, fileName, 'test', identity));

end;



initialization
  TDUnitX.RegisterTestFixture(TPackageIdentityTestFixtue);
end.
