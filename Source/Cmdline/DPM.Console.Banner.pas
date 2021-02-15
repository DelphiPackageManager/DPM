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

unit DPM.Console.Banner;

interface
uses
  DPM.Console.Writer;

procedure ShowBanner(const consoleWriter : IConsoleWriter);

implementation

uses
  System.SysUtils,
  DPM.Console.Utils;

procedure ShowBanner(const consoleWriter : IConsoleWriter);
begin
  Assert(consoleWriter <> nil, 'No console writer available');
  consoleWriter.WriteLine('');
  consoleWriter.SetColour(ccBrightAqua, ccDefault);
  consoleWriter.WriteLine('DPM - Delphi Package Manager - Version : ' + TDPMWindowsUtils.GetVersionString);
  consoleWriter.SetColour(ccBrightWhite);
  consoleWriter.WriteLine('� 2019-2021 Vincent Parrett and Contributors');
  //consoleWriter.WriteLine('License - http://www.apache.org/licenses/LICENSE-2.0');
  consoleWriter.WriteLine('');
end;

end.
