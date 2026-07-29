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

unit DPM.IDE.Types;

interface

uses
  WinApi.Windows,
  Vcl.Graphics,
  DPM.Core.Constants,
  DPM.Core.Types;

{$SCOPEDENUMS ON}

type
  TDPMPackageStatus = (NotInstalled,
    Installed, //latest version installed.
    UpdateAvailable //installed but not on the latest version
    );

  TDPMEditViewMode = (vmProject, vmGroup);

  TDPMSearchOption = (IncludePrerelease, IncludeCommercial, IncludeTrial);

  TDPMSearchOptions = set of TDPMSearchOption;

  /// <summary>
  ///  Which DPM releases the IDE update check will offer.
  ///  Beta is a SUPERSET of Stable, not a separate track - a beta channel user
  ///  is still offered a newer stable release, because the check only ever
  ///  excludes prereleases and then takes the highest version.
  /// </summary>
  TDPMUpdateChannel = (Stable, Beta);


const
  //The current IDE version to TCompilerVersion.
  {$IFDEF VER370}IDECompilerVersion = TCompilerVersion.Delphi13_0; {$ENDIF}
  {$IFDEF VER360}IDECompilerVersion = TCompilerVersion.Delphi12_0; {$ENDIF}
  {$IFDEF VER350}IDECompilerVersion = TCompilerVersion.Delphi11_0; {$ENDIF}
  {$IFDEF VER340}IDECompilerVersion = TCompilerVersion.Delphi10_4; {$ENDIF}
  {$IFDEF VER330}IDECompilerVersion = TCompilerVersion.Delphi10_3; {$ENDIF}
  {$IFDEF VER320}IDECompilerVersion = TCompilerVersion.Delphi10_2; {$ENDIF}
  {$IFDEF VER310}IDECompilerVersion = TCompilerVersion.Delphi10_1; {$ENDIF}
  {$IFDEF VER300}IDECompilerVersion = TCompilerVersion.Delphi10_0; {$ENDIF}
  {$IFDEF VER290}IDECompilerVersion = TCompilerVersion.DelphiXE8; {$ENDIF}
  {$IFDEF VER280}IDECompilerVersion = TCompilerVersion.DelphiXE7; {$ENDIF}
  {$IFDEF VER270}IDECompilerVersion = TCompilerVersion.DelphiXE6; {$ENDIF}
  {$IFDEF VER260}IDECompilerVersion = TCompilerVersion.DelphiXE5; {$ENDIF}
  {$IFDEF VER250}IDECompilerVersion = TCompilerVersion.DelphiXE4; {$ENDIF}
  {$IFDEF VER240}IDECompilerVersion = TCompilerVersion.DelphiXE3; {$ENDIF}
  {$IFDEF VER230}IDECompilerVersion = TCompilerVersion.DelphiXE2; {$ENDIF}

  cDPMIDEOptionsFileName = 'dpm-ide.config';
  cDPMIDEDefaultOptionsFile = cDefaultDPMFolder + '\' + cDPMIDEOptionsFileName;

  //Link colours, shared so every clickable url in the view looks the same.
  //TColor literals are $00BBGGRR. The light colour is the one the package
  //details panel has always used - it reads well on the light themes but is
  //far too dark against the dark themes, hence the brightened variant.
  cDPMLinkColorLight = TColor($00C57321); //#2173C5
  cDPMLinkColorDark = TColor($00F5A84F);  //#4FA8F5


///<summary>
///  Rec.601 luma. Decide light vs dark from the colour we are actually about
///  to paint on, rather than from the style name - the two can disagree.
///</summary>
function IsDarkBackground(const backgroundColor : TColor) : boolean;

///<summary>
///  The link colour to use when painting on backgroundColor.
///</summary>
function GetLinkColor(const backgroundColor : TColor) : TColor;

implementation

function IsDarkBackground(const backgroundColor : TColor) : boolean;
var
  rgbValue : longint;
  luma : integer;
begin
  rgbValue := ColorToRGB(backgroundColor);
  luma := (GetRValue(rgbValue) * 299 + GetGValue(rgbValue) * 587 + GetBValue(rgbValue) * 114) div 1000;
  result := luma < 128;
end;

function GetLinkColor(const backgroundColor : TColor) : TColor;
begin
  if IsDarkBackground(backgroundColor) then
    result := cDPMLinkColorDark
  else
    result := cDPMLinkColorLight;
end;

end.

