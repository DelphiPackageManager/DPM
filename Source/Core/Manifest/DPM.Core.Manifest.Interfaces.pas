unit DPM.Core.Manifest.Interfaces;

interface

uses
  System.Classes,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Spec.Interfaces;

type
  // The package manifest is a processed spec with a single targetPlatform.
  IPackageManifest = interface
  ['{F3B0C13F-20A8-4DA6-9056-CF17021C12FC}']

    function GetMetaData : ISpecMetaData;
    function GetTargetPlatform : ISpecTargetPlatform;
    function GetIsValid : boolean;
    function GetFileName : string;
    function LoadFromYAML(const yamlObj : IYAMLMapping) : boolean;

    property MetaData : ISpecMetaData read GetMetaData;
    property TargetPlatform : ISpecTargetPlatform read GetTargetPlatform;
    property IsValid : boolean read GetIsValid;
    property FileName : string read GetFileName;
  end;

  IPackageManifestReader = interface
  ['{98B91CEB-ACF3-433C-8E5D-087AFD0A1EA8}']
    function ReadManifest(const fileName : string) : IPackageManifest;
    function ReadManifestString(const manifestString : string) : IPackageManifest;
  end;

implementation

end.
