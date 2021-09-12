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


unit DPM.Console.Types;

interface

(*
config      Gets or sets NuGet config values.

delete      Deletes a package from the server.

help (?)    Displays general help information and help information about other commands.

install     Installs a package using the specified sources. If no sources are specified, all sources defined in the NuGet configuration file are used. If the
            configuration file specifies no sources, uses the default NuGet feed.

list        Displays a list of packages from a given source. If no sources are specified, all sources defined in %AppData%\NuGet\NuGet.config are used. If NuG
            et.config specifies no sources, uses the default NuGet feed.

pack        Creates a NuGet package based on the specified nuspec or project file.

push        Pushes a package to the server and publishes it.
            NuGet's default configuration is obtained by loading %AppData%\NuGet\NuGet.config, then loading any nuget.config or .nuget\nuget.config starting f
            rom root of drive and ending in current directory.

restore     Restores NuGet packages.

setApiKey   Saves an API key for a given server URL. When no URL is provided API key is saved for the NuGet gallery.

sources     Provides the ability to manage list of sources located in %AppData%\NuGet\NuGet.config

spec        Generates a nuspec for a new package. If this command is run in the same folder as a project file (.csproj, .vbproj, .fsproj), it will create a to
            kenized nuspec file.

update      Update packages to latest available versions. This command also updates NuGet.exe itself.
*)

{$SCOPEDENUMS ON}
type
  TDPMCommand = (Cache,
                 Config,
                 Delete,
                 ExitCodes,
                 Help,
                 Install,
                 Uninstall,
                 Feed,
                 List,
                 Pack,
                 Push,
                 Restore,
                 SetAPIKey,
                 Sign,
                 Sources,
                 Spec,
                 Update,
                 Verify,
                 Why,
                 Info,
                 None,
                 Invalid);

const
  CommandString : array[TDPMCommand] of string =
                      ('cache',
                       'config',
                       'delete',
                       'exitcodes',
                       'help',
                       'install',
                       'uninstall',
                       'feed',
                       'list',
                       'pack',
                       'push',
                       'restore',
                       'setapikey',
                       'sign',
                       'sources',
                       'spec',
                       'update',
                       'verify',
                       'why',
                       'info',
                       '',
                       'invalid');

function GetCommandFromString(const value : string) : TDPMCommand;

implementation

uses
  System.TypInfo;

function GetCommandFromString(const value : string) : TDPMCommand;
var
  i : integer;
begin
  if value = '' then
    exit(TDPMCommand.Help);

  i := GetEnumValue(TypeInfo(TDPMCommand),value);
  if i <> -1 then
    result := TDPMCommand(i)
  else
    result := TDPMCommand.Invalid;
end;

end.
