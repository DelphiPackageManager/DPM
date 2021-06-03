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

  TDPMCurrentTab = (Search, Installed, Updates, Conflicts);

  TDPMSearchOption = (IncludePrerelease, IncludeCommercial, IncludeTrial);

  TDPMSearchOptions = set of TDPMSearchOption;


const
  //The current IDE version to TCompilerVersion.
  {$IFDEF VER350}IDECompilerVersion = TCompilerVersion.RS10_5; {$ENDIF}
  {$IFDEF VER340}IDECompilerVersion = TCompilerVersion.RS10_4; {$ENDIF}
  {$IFDEF VER330}IDECompilerVersion = TCompilerVersion.RS10_3; {$ENDIF}
  {$IFDEF VER320}IDECompilerVersion = TCompilerVersion.RS10_2; {$ENDIF}
  {$IFDEF VER310}IDECompilerVersion = TCompilerVersion.RS10_1; {$ENDIF}
  {$IFDEF VER300}IDECompilerVersion = TCompilerVersion.RS10_0; {$ENDIF}
  {$IFDEF VER290}IDECompilerVersion = TCompilerVersion.RSXE8; {$ENDIF}
  {$IFDEF VER280}IDECompilerVersion = TCompilerVersion.RSXE7; {$ENDIF}
  {$IFDEF VER270}IDECompilerVersion = TCompilerVersion.RSXE6; {$ENDIF}
  {$IFDEF VER260}IDECompilerVersion = TCompilerVersion.RSXE5; {$ENDIF}
  {$IFDEF VER250}IDECompilerVersion = TCompilerVersion.RSXE4; {$ENDIF}
  {$IFDEF VER240}IDECompilerVersion = TCompilerVersion.RSXE3; {$ENDIF}
  {$IFDEF VER230}IDECompilerVersion = TCompilerVersion.RSXE2; {$ENDIF}
  {$IFDEF VER220}IDECompilerVersion = TCompilerVersion.RSXE; {$ENDIF}
  {$IFDEF VER210}IDECompilerVersion = TCompilerVersion.RS2010; {$ENDIF}

  cDPMIDEOptionsFileName = 'dpm-ide.config';
  cDPMIDEDefaultOptionsFile = cDefaultDPMFolder + '\' + cDPMIDEOptionsFileName;

implementation

end.

