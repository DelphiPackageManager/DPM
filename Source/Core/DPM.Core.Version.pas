{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright (c) 2019 Vincent Parrett and contributors             }
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

unit DPM.Core.Version;

interface

uses
  DPM.Core.Types;

const
  //The semantic version of this build. This is the SINGLE SOURCE OF TRUTH for
  //"what version am I" when talking to github releases - releases are tagged
  //with semver (eg v0.1.10-alpha).
  //
  //It cannot be derived from the win32 version resource : that resource only
  //holds four integers (FileVersion=0.9.243.0) with no way to carry a
  //prerelease label. The dproj does stash the semver in the 'Comments' key, but
  //nothing keeps that in sync and it is currently stale (0.1.9-alpha).
  //
  //BUILD NOTE : CI stamps this line - the value committed here is only what a
  //local dev build reports, and is not expected to be current. It is
  //deliberately a single line matching
  //  cDPMSemVer = '<semver>';
  //so that a regex replace can rewrite it.
  //Kept roughly in step with the FileVersion resource (0.9.243.0) and the
  //-beta tag convention, so an unstamped build reports something plausible
  //rather than claiming to be a version that never shipped.
  cDPMSemVer = '0.9.242-beta';

type
  TDPMVersion = class
    /// <summary>
    ///  The version of this executable as a semantic version. Returns
    ///  TPackageVersion.Empty if cDPMSemVer is not parsable (ie the build
    ///  stamped something invalid) - callers must handle that, since comparing
    ///  against Empty would report every release as an upgrade.
    /// </summary>
    class function CurrentVersion : TPackageVersion; static;

    /// <summary>
    ///  cDPMSemVer verbatim, for display purposes.
    /// </summary>
    class function CurrentVersionString : string; static;
  end;

implementation

{ TDPMVersion }

class function TDPMVersion.CurrentVersion : TPackageVersion;
begin
  if not TPackageVersion.TryParse(cDPMSemVer, result) then
    result := TPackageVersion.Empty;
end;

class function TDPMVersion.CurrentVersionString : string;
begin
  result := cDPMSemVer;
end;

end.
