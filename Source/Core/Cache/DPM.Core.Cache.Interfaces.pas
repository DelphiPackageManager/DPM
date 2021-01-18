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

unit DPM.Core.Cache.Interfaces;

interface

uses
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Package.Interfaces,
  DPM.Core.Spec.Interfaces;


type
  IPackageCache = interface
    ['{4285BB27-6E42-4B2A-9B81-B63505ABF934}']
    function GetLocation : string;
    procedure SetLocation(const value : string);
    function GetPackagesFolder : string;

    function Clean : boolean;

    // creates the folder where the package would reside and returns the path.
    function CreatePackagePath(const packageId : IPackageId) : string;

    function GetPackagePath(const packageId : IPackageId) : string;overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : string;overload;


    //checks if the package is present as a folder, if not there but the file is
    //then it will call InstallPackage to extract it.
    function EnsurePackage(const packageId : IPackageId) : boolean;

    function InstallPackageFromFile(const packageFileName : string; const saveFile : boolean) : boolean;

    //gets the package info with dependencies. Calls EnsurePackage.
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;

    //gets the full package metadata including search paths.
    function GetPackageMetadata(const packageId : IPackageId) : IPackageMetadata;

    //gets the deserialized dspec file for the package.
    function GetPackageSpec(const packageId : IPackageId) : IPackageSpec;

    property Location : string read GetLocation write SetLocation;
    property PackagesFolder : string read GetPackagesFolder;
  end;

implementation

end.

