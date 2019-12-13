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

unit DPM.Core.Package.Interfaces;

interface

uses
  Spring.Collections,
  System.Generics.Defaults,

  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.TargetPlatform,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Install,
  DPM.Core.Options.Restore;

type
  //Only has info we can get from the filename!
  IPackageIdentity = interface
  ['{E9E49A25-3ECA-4380-BB75-AC9E29725BEE}']
    function GetId : string;
    function GetVersion : TPackageVersion;
    function GetSourceName : string;
    function GetCompilerVersion : TCompilerVersion;
    function GetPlatform : TDPMPlatform;
    function ToString : string;
    property Id : string read GetId;
    property Version : TPackageVersion read GetVersion;
    property SourceName : string read GetSourceName;
    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
    property Platform : TDPMPlatform read GetPlatform;
  end;

  IPackageDependency = interface
  ['{E3576B9F-2CD5-415F-81D7-9E01AA74C9DB}']
    function GetId : string;
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function ToString : string;

    property Id : string read GetId;
    property Version : TVersionRange read GetVersionRange write SetVersionRange;
  end;

  //identity plus dependencies. used when resolving.
  IPackageInfo = interface(IPackageIdentity)
  ['{5672DB4A-40BC-45E0-857C-39117D03C322}']
    function GetDependencies : IList<IPackageDependency>;

    property Dependencies : IList<IPackageDependency> read GetDependencies;
  end;

  IPackageSearchPath = interface
  ['{55B09D0C-01E4-4FF3-977F-98A1A57B62B1}']
    function GetPath : string;
    function GetSourceOnly : boolean;
    function GetBinariesOnly : boolean;

    property Path : string read GetPath;
    property BinariesOnly : boolean read GetBinariesOnly;
    property SourceOnly : boolean read GetSourceOnly;
  end;


  //full package metadata.
  IPackageMetadata = interface(IPackageInfo)
  ['{0C39A81D-63FF-4939-A74A-4BFE29724168}']
    function GetDescription   : string;
    function GetAuthors       : string;
    function GetProjectUrl    : string;
    function GetLicense       : string;
    function GetIcon          : string;
    function GetCopyright     : string;
    function GetTags          : string;
    function GetIsTrial       : boolean;
    function GetIsCommercial  : boolean;
    function GetSearchPaths   : IList<IPackageSearchPath>;

    property Description  : string  read GetDescription;
    property Authors      : string  read GetAuthors;
    property ProjectUrl   : string  read GetProjectUrl;
    property License      : string  read GetLicense;
    property Icon         : string  read GetIcon;
    property Copyright    : string  read GetCopyright;
    property Tags         : string  read GetTags;
    property IsTrial      : boolean read GetIsTrial;
    property IsCommercial : boolean read GetIsCommercial;
    property SearchPaths  : IList<IPackageSearchPath> read GetSearchPaths;
  end;

  //does the work of installing/restoring packages.
  IPackageInstaller = interface
  ['{554A0842-6C83-42BD-882C-B49FE4619DE0}']
    function Install(const options : TInstallOptions) : boolean;
    function Restore(const options : TRestoreOptions) : boolean;
    function Cache(const options : TCacheOptions) : boolean;
  end;

  //used to collect and detect package conflicts when working with multiple projects.
  IPackageInstallerContext = interface
  ['{8FD229A2-FE7B-4315-84B2-FF18B78C76DC}']
    procedure Reset;

  end;


  TPackageIdentityComparer = class(TInterfacedObject,IEqualityComparer<IPackageIdentity>)
  protected
    function Equals(const Left, Right: IPackageIdentity): Boolean;reintroduce;
    function GetHashCode(const Value: IPackageIdentity): Integer; reintroduce;
  end;

  TPackageInfoComparer = class(TInterfacedObject,IEqualityComparer<IPackageInfo>)
  protected
    function Equals(const Left, Right: IPackageInfo): Boolean;reintroduce;
    function GetHashCode(const Value: IPackageInfo): Integer; reintroduce;
  end;



implementation

uses
  System.SysUtils;

{ TPackageIdentityComparer }

function TPackageIdentityComparer.Equals(const Left, Right: IPackageIdentity): Boolean;
begin
  result := SameText(Left.ToString, right.ToString);
end;

function TPackageIdentityComparer.GetHashCode(const Value: IPackageIdentity): Integer;
var
  s : string;
begin
  s := Value.ToString;
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
end;

{ TPackageInfoComparer }

function TPackageInfoComparer.Equals(const Left, Right: IPackageInfo): Boolean;
begin
  result := SameText(Left.ToString, right.ToString);
end;

function TPackageInfoComparer.GetHashCode(const Value: IPackageInfo): Integer;
var
  s : string;
begin
  s := Value.ToString;
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
end;

end.
