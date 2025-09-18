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


const
  //The current IDE version to TCompilerVersion.
  {$IFDEF VER370}IDECompilerVersion = TCompilerVersion.Delphi13; {$ENDIF}
  {$IFDEF VER360}IDECompilerVersion = TCompilerVersion.Delphi12; {$ENDIF}
  {$IFDEF VER350}IDECompilerVersion = TCompilerVersion.Delphi11; {$ENDIF}
  {$IFDEF VER340}IDECompilerVersion = TCompilerVersion.Delphi10.4; {$ENDIF}
  {$IFDEF VER330}IDECompilerVersion = TCompilerVersion.Delphi10.3; {$ENDIF}
  {$IFDEF VER320}IDECompilerVersion = TCompilerVersion.Delphi10.2; {$ENDIF}
  {$IFDEF VER310}IDECompilerVersion = TCompilerVersion.Delphi10.1; {$ENDIF}
  {$IFDEF VER300}IDECompilerVersion = TCompilerVersion.Delphi10; {$ENDIF}
  {$IFDEF VER290}IDECompilerVersion = TCompilerVersion.DelphiXE8; {$ENDIF}
  {$IFDEF VER280}IDECompilerVersion = TCompilerVersion.DelphiXE7; {$ENDIF}
  {$IFDEF VER270}IDECompilerVersion = TCompilerVersion.DelphiXE6; {$ENDIF}
  {$IFDEF VER260}IDECompilerVersion = TCompilerVersion.DelphiXE5; {$ENDIF}
  {$IFDEF VER250}IDECompilerVersion = TCompilerVersion.DelphiXE4; {$ENDIF}
  {$IFDEF VER240}IDECompilerVersion = TCompilerVersion.DelphiXE3; {$ENDIF}
  {$IFDEF VER230}IDECompilerVersion = TCompilerVersion.DelphiXE2; {$ENDIF}

  cDPMIDEOptionsFileName = 'dpm-ide.config';
  cDPMIDEDefaultOptionsFile = cDefaultDPMFolder + '\' + cDPMIDEOptionsFileName;

implementation

end.

