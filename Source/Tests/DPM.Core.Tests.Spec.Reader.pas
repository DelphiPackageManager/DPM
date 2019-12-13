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

unit DPM.Core.Tests.Spec.Reader;

interface

uses
  DPM.Core.Spec.Interfaces,
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TSpecReaderTests = class

  public
    [SetupFixture]
    procedure FixtureSetup;

    [TearDownFixture]
    procedure FixtureTearDown;
  published
    procedure Test_can_load_core_spec;
  end;

implementation


uses
  Winapi.ActiveX,
  System.SysUtils,
  TestLogger,
  DPM.Core.Spec.Reader;


{ TSpecReaderTests }

procedure TSpecReaderTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TSpecReaderTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TSpecReaderTests.Test_can_load_core_spec;
var
  spec : IPackageSpec;
  reader : IPackageSpecReader;
//  lastError : string;
  filePath : string;
begin
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  filePath := ExtractFilePath(ParamStr(0)) + '..\..\..\DPM.Core.dspec';
  spec := reader.ReadSpec(filePath);
  Assert.IsNotNull(spec);

end;

initialization
  //TDUnitX.RegisterTestFixture(TSpecReaderTests);

end.
