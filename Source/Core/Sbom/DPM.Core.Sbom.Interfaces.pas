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

unit DPM.Core.SBOM.Interfaces;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Options.Sbom,
  DPM.Core.SBOM.Types;

type
  ///<summary>Single output-format writer (CycloneDX, SPDX, etc).
  /// Container registers all implementations under this interface and the generator
  /// resolves the matching writers for the requested format.</summary>
  ISbomWriter = interface
    ['{4DF5E7A9-3D7B-4F02-9C8B-5C7C7A3F44A4}']
    function GetFormat : TSBOMFormat;
    function GetFileExtension : string;
    procedure Write(const report : TSBOMReport; const fileName : string);
    property Format : TSBOMFormat read GetFormat;
    property FileExtension : string read GetFileExtension;
  end;

  ///<summary>Top-level entry point used by the CLI command and (later) the IDE plugin.
  /// Resolves a project (dproj or groupproj), enumerates platforms, builds one TSBOMReport
  /// per (project, platform) pair, runs the configured writers on each.</summary>
  ISbomGenerator = interface
    ['{F1A0C0F2-DC4C-4D6D-BE3F-5A87BD68F69A}']
    function Generate(const cancellationToken : ICancellationToken; const options : TSBOMOptions) : boolean;
  end;

  ///<summary>Inverse of ISbomWriter for the CycloneDX format - parses an existing
  /// CycloneDX 1.5 JSON document back into a TSBOMReport so dpm scan can consume
  /// SBOMs the user (or someone else) generated previously. Caller owns the returned
  /// report.</summary>
  ISBOMReader = interface
    ['{0B7C5F2E-8D14-4F3B-9E07-2A6C1D8B9F45}']
    function ReadFromFile(const fileName : string) : TSBOMReport;
    function ReadFromString(const json : string) : TSBOMReport;
  end;

implementation

end.
