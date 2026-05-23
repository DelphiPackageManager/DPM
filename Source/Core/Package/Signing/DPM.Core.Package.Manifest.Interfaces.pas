{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
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

unit DPM.Core.Package.Manifest.Interfaces;

interface

uses
  System.SysUtils,
  DPM.Core.Crypto.Algorithms;

type
  TManifestFileEntry = record
    Path : string;            // forward-slash, NFC, relative
    Size : Int64;
    Hash : TBytes;            // bytes of `algorithm` digest
  end;

  IPackageManifest = interface
    ['{B3D2A0AA-3D6C-4B7F-9C61-9C29F4DDB3E4}']
    function DpmPackageFormat : integer;
    function ManifestSchemaVersion : integer;
    function PackageId : string;
    function Version : string;
    function HashAlgorithm : THashAlgorithm;
    function Created : TDateTime;
    function Files : TArray<TManifestFileEntry>;
    function RawBytes : TBytes;   // exact signed bytes — never re-emitted
  end;

  IManifestService = interface
    ['{8F2A7C0E-3A1A-4F8C-93EC-3CDA1B5E2F1C}']
    function Generate(const packageRoot : string;
                      const packageId : string;
                      const version : string;
                      algorithm : THashAlgorithm) : IPackageManifest;

    // Generate a manifest from an already-built .dpkg archive — used by
    // the pack command after FArchiveWriter has finished writing entries.
    // Excludes dpm-manifest.json and signatures/** automatically.
    function GenerateFromArchive(const archivePath : string;
                                 const packageId : string;
                                 const version : string;
                                 algorithm : THashAlgorithm) : IPackageManifest;

    // Inject the manifest into an existing .dpkg (zmReadWrite append).
    procedure InjectIntoArchive(const archivePath : string;
                                const manifest : IPackageManifest);

    function Parse(const bytes : TBytes) : IPackageManifest;

    // Path safety (M-8). Used by both producer and verifier.
    function ValidatePath(const path : string; out reason : string) : boolean;
    function NormalizeToNfc(const value : string) : string;
  end;

  EManifest = class(Exception);
  EManifestPath = class(EManifest);
  EManifestParse = class(EManifest);

const
  cManifestFileName = 'dpm-manifest.json';
  cManifestMaxBytes = 8 * 1024 * 1024;    // V-2: bound parser input
  cManifestMaxDepth = 8;                  // V-2: bound nesting
  cCurrentDpmPackageFormat = 1;
  cCurrentManifestSchemaVersion = 1;

implementation

end.
