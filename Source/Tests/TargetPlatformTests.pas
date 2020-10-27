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

unit TargetPlatformTests;

interface

uses
  DUnitX.TestFramework;


type
  {$M+}
  [TestFixture]
  TDPMTargetPlatformTestFixture = class(TObject)
  published
    procedure Can_Parse_Valid_String;
    procedure Will_Throw_on_invalid_compiler_value;
    procedure Will_Throw_on_invalid_platform_value;
    procedure Will_Throw_on_empty_string;
    procedure Will_Throw_on_missing_platform;
  end;



implementation

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.TargetPlatform;

{ TDPMTargetPlatformTestFixture }

procedure TDPMTargetPlatformTestFixture.Can_Parse_Valid_String;
var
  value :TTargetPlatform;
begin
  Assert.IsTrue(TTargetPlatform.TryParse('RSXE7.Win32', value));
  Assert.IsTrue(TTargetPlatform.TryParse('10.3.Win32', value));
end;


procedure TDPMTargetPlatformTestFixture.Will_Throw_on_empty_string;
begin
  Assert.WillRaise(
    procedure
    var
      value :TTargetPlatform;
    begin
      value := TTargetPlatform.Parse('');
    end
    , EArgumentException);

end;

procedure TDPMTargetPlatformTestFixture.Will_Throw_on_invalid_compiler_value;
begin
  Assert.WillRaise(
    procedure
    var
      value :TTargetPlatform;
    begin
      value := TTargetPlatform.Parse('RSXE9.Win32');
    end
    , EArgumentOutOfRangeException);


end;

procedure TDPMTargetPlatformTestFixture.Will_Throw_on_invalid_platform_value;
begin
  Assert.WillRaise(
    procedure
    var
      value :TTargetPlatform;
    begin
      value := TTargetPlatform.Parse('RSXE7.Win23');
    end
    , EArgumentOutOfRangeException);


end;


procedure TDPMTargetPlatformTestFixture.Will_Throw_on_missing_platform;
begin
  Assert.WillRaise(
    procedure
    var
      value :TTargetPlatform;
    begin
      value := TTargetPlatform.Parse('XE2.');
    end
    , EArgumentException);

end;

initialization
  TDUnitX.RegisterTestFixture(TDPMTargetPlatformTestFixture);

end.

