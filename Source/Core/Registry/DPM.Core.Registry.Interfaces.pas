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

unit DPM.Core.Registry.Interfaces;

interface

uses
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Spec.Interfaces;

type
  //A package registry is a folder-per-id collection of <id>.dspec.yaml files,
  //backed either by a plain local folder or by a cloned git repo mirror. The
  //catalog resolves/refreshes the working copy and parses the dspecs into specs.
  IRegistryCatalog = interface
  ['{4E2A1C73-9F6B-4D0A-8C5E-7B1A2D3F4E5A}']
    //Ensures the local working copy is present and (for git-URL registries)
    //refreshed per the TTL policy. forceRefresh bypasses the TTL. Folder
    //registries are always live so this is effectively a presence check.
    function EnsureUpdated(const cancellationToken : ICancellationToken; const forceRefresh : boolean) : boolean;

    //Returns the parsed registry dspec for id (case-insensitive), or nil.
    function GetPackageSpec(const cancellationToken : ICancellationToken; const id : string) : IPackageSpec;

    //All package ids present in the registry.
    function GetPackageIds(const cancellationToken : ICancellationToken) : IList<string>;
  end;

implementation

end.
