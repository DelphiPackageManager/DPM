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

unit DPM.Core.Packaging.Archive;

interface

uses
  System.Zip,
  System.Classes,
  Spring.Container,
  Spring.Container.Common,
  VSoft.SemanticVersion;

type
  //wrapper for archive zip file (or extracted folder).
  IPackageArchive = interface
  ['{A5CE5203-6405-49D9-9F86-AB6D10580D1C}']
    function GetArchiveName : string;
    function GetArchivePath : string;
    function Open(const fileName : string) : boolean;
    procedure Close;
    function Exists : boolean;
    function IsArchive : boolean;
    function GetLastErrorString : string;

    property ArchiveName : string read GetArchiveName;
    property ArchivePath : string read GetArchivePath;
    property LastErrorString : string read GetLastErrorString;
  end;

  IPackageArchiveReader = interface(IPackageArchive)
  ['{28705A4E-7B7D-4C56-A48F-77D706D0AD26}']
    //Read the metaDataFile into a stream;
    function ReadMetaDataFile(const stream : TStream) : boolean;
    function ReadFileNames : TArray<string>;
    function ExtractFileTo(const fileName : string; const destFileName : string) : boolean;
    function ExtractTo(const path : string) : boolean;
  end;

  IPackageArchiveWriter = interface(IPackageArchive)
  ['{B1BA4ED1-E456-42DE-AA17-AA53480EE645}']
    procedure SetBasePath(const path : string);
    function WriteMetaDataFile(const stream: TStream): Boolean;
    function AddFile(const filePath: string): Boolean;overload;
    function AddFile(const fileName : string; const archiveFileName : string) : boolean;overload;
    function AddFiles(const files: System.TArray<System.string>): Boolean;
  end;



implementation


end.

