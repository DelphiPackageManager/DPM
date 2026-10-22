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

unit DPM.Core.Diagnostics;

//The codes for every error and warning DPM reports - 'error DPME1101: ...', 'warning DPMW1004: ...'.
//docs\error-codes.md explains what each one means and how to fix it.

interface

uses
  System.SysUtils;

{$SCOPEDENUMS ON}
//The catalogues will grow past 256 entries, so store the code enums as 4 bytes. The size belongs to the
//type, so records holding a code pick it up wherever they are declared. Never declare a set of either
//enum - sets are limited to 256 elements.
{$MINENUMSIZE 4}

type
  //Every error DPM reports.
  // - the identifier carries the number, so a reported code can be grepped for.
  // - errors and warnings share one number space - a number belongs to one code only.
  // - the thousands digit is the subsystem, the hundreds digit an area within it :
  //     0 general/cli/options/config, 1 sources/repositories, 2 dependency resolution,
  //     3 install/restore/uninstall/cache, 4 project files, 5 compiling, 6 spec/pack/push,
  //     7 signing/trust/verify, 8 sbom/vulnerabilities, 9 ide/dspec creator/mcp.
  // - never renumber or reuse a released code - retire it (keep the entry, stop using it).
  // - no explicit ordinal values - an enum with them has no RTTI, which the tests rely on.
  // - keep the list in number order, and add the matching row to cErrorInfo and docs\error-codes.md.
  TErrorCode = (
    E0001_UnexpectedException,
    E0002_FailedWithoutDetail,
    E0003_InternalError,
    E0101_MissingArgument,
    E0102_InvalidArgument,
    E0201_ConfigurationLoadFailed,
    E0202_ConfigurationSaveFailed,
    E0203_ConfigurationNotFound,
    E1001_NoSourcesConfigured,
    E1002_InvalidSource,
    E1003_UnknownSource,
    E1101_PackageNotFound,
    E1102_PackageNotFoundSourcesUnavailable,
    E1201_PackageDownloadFailed,
    E1202_PackageHashMismatch,
    E1203_GitCommandFailed,
    E1209_PackagePushFailed,
    E2001_DependencyUnsatisfiable,
    E2002_TopLevelVersionConflict,
    E2003_ProjectGroupConflict,
    E2004_ResolverIterationLimit,
    E2005_SelectedVersionConflict,
    E2006_RestoreTopLevelIncompatible,
    E2007_PlanConflict,
    E2008_NoCompatibleCompanion,
    E2009_PlanIterationLimit,
    E3001_NothingToInstall,
    E3002_PackageNotInstalled,
    E3003_NoPackagesToInstall,
    E3004_PackageFileNotFound,
    E3005_InvalidPackageFile,
    E3006_PackageDeclaresNoPlatforms,
    E3007_LockHashMismatch,
    E3008_CacheWriteFailed,
    E3009_PackageSpecUnavailable,
    E3010_PackageExtractFailed,
    E4001_ProjectNotFound,
    E4002_NoProjectsFound,
    E4003_ProjectLoadFailed,
    E4004_InvalidPackageReference,
    E4005_ProjectStructureInvalid,
    E4006_ProjectSaveFailed,
    E5001_CompilerNotInstalled,
    E5002_CompilerLaunchFailed,
    E5003_PackageCompileFailed,
    E5004_BuildProjectUnreadable,
    E5005_BuildProjectRunsCommands,
    E5006_PrebuiltDesignPackageInvalid,
    E5007_PrebuiltDesignPackageMissing,
    E5008_PackageProjectGenerationFailed,
    E6001_SpecNotFound,
    E6002_SpecUnreadable,
    E6003_SpecElementMissing,
    E6004_SpecValueInvalid,
    E6005_SpecNeedsNewerDPM,
    E6101_TemplateNotFound,
    E6102_EnvironmentVariableNotAllowed,
    E6103_VariableReplacementFailed,
    E6105_PackFileNotFound,
    E6106_PackageWriteFailed,
    E6107_PackageInvalid,
    E6108_ManifestWriteFailed,
    E6109_GitPackageNotPackable,
    E6201_PushRejected,
    E6202_PushNotSupported,
    E6203_PushSourceDisabled,
    E7001_PackageVerificationFailed,
    E7002_PackageBlocked,
    E7003_AuthorDowngradeBlocked,
    E7004_RepositorySignatureBlocked,
    E7101_SigningProviderUnavailable,
    E7102_SigningFailed,
    E7103_SignatureInvalid,
    E7104_CertificateInvalid,
    E7105_TimestampFailed,
    E7106_CryptoFailed,
    E7107_ManifestInvalid,
    E7108_TrustStateFailed
  );

  //Every warning DPM reports - the same rules as TErrorCode.
  TWarningCode = (
    W0004_OperationCancelled,
    W1004_SourceUnavailable,
    W1204_PackageHashUnknown,
    W1205_HashFileWriteFailed,
    W1206_MultiplePackageFilesMatched,
    W1207_PackageEntryUnreadable,
    W1208_RegistryEntryInvalid,
    W2010_ProjectGroupMismatch,
    W2011_TopLevelVersionNewer,
    W2012_CompanionsRequired,
    W3011_PlatformNotSupportedByPackages,
    W3012_PackageHasNoSearchPaths,
    W3013_PackageSpecUnreadable,
    W3014_DesignPackageUnloadFailed,
    W3015_CacheReceiptWriteFailed,
    W3016_CopyLocalTargetsWriteFailed,
    W4008_ProjectPlatformsNotTargeted,
    W4009_ProjectVersionAmbiguous,
    W4010_ProjectVersionMismatch,
    W4011_ProjectNotFoundInList,
    W4012_UnsupportedProjectPlatform,
    W4013_InvalidDPMCompiler,
    W5009_BuildRecordSaveFailed,
    W5010_DesignHostCompileFailed,
    W5011_BuildTargetPatchFailed,
    W5012_PathOutsidePackage,
    W5013_BuildEventIgnored,
    W5014_CopyFileFailed,
    W5015_DesignProjectHasNoPlatforms,
    W5016_DesignProjectUnreadable,
    W5017_PackageDefinitionHasNoPlatforms,
    W5018_BuildConfigurationAdded,
    W6006_PlatformNotValidForCompiler,
    W6104_PatternMatchedNoFiles,
    W6110_SourceVariableUnresolved,
    W6301_PrepareTargetExists,
    W6302_SpecHasNoPlatforms,
    W7005_RepositorySignatureChanged,
    W7006_AuthorSignatureChanged,
    W7007_AuthorRebuildAccepted,
    W7008_PublisherNotTrusted,
    W7009_SignatureSelfCheckFailed,
    W7010_PackageUnsigned
  );

  TDiagnosticInfo = record
    Number : integer;
    Title : string;
  end;

const
  cErrorCodePrefix = 'DPME';
  cWarningCodePrefix = 'DPMW';

  cErrorInfo : array[TErrorCode] of TDiagnosticInfo = (
    (Number : 1; Title : 'An unexpected exception occurred'),
    (Number : 2; Title : 'The operation failed without reporting why'),
    (Number : 3; Title : 'Internal error'),
    (Number : 101; Title : 'A required argument is missing'),
    (Number : 102; Title : 'An argument is not valid'),
    (Number : 201; Title : 'Unable to load the configuration file'),
    (Number : 202; Title : 'Unable to save the configuration file'),
    (Number : 203; Title : 'The configuration file does not exist'),
    (Number : 1001; Title : 'No package sources are configured'),
    (Number : 1002; Title : 'A package source is not valid'),
    (Number : 1003; Title : 'The package source is not configured'),
    (Number : 1101; Title : 'Package not found'),
    (Number : 1102; Title : 'Package not found - a package source could not be used'),
    (Number : 1201; Title : 'Unable to download the package'),
    (Number : 1202; Title : 'The package does not match its recorded hash'),
    (Number : 1203; Title : 'A git command failed'),
    (Number : 1209; Title : 'Unable to push the package'),
    (Number : 2001; Title : 'No available version of a dependency satisfies the required range'),
    (Number : 2002; Title : 'A top level package version does not satisfy a dependency'),
    (Number : 2003; Title : 'A version used by another project in the group does not satisfy a dependency'),
    (Number : 2004; Title : 'Dependency resolution did not finish'),
    (Number : 2005; Title : 'A selected package version does not satisfy a dependency'),
    (Number : 2006; Title : 'A top level package version is not compatible with a package that uses it'),
    (Number : 2007; Title : 'The packages to install cannot be resolved together'),
    (Number : 2008; Title : 'No compatible version of a package that has to move with the install'),
    (Number : 2009; Title : 'The install plan did not settle'),
    (Number : 3001; Title : 'None of the projects need the package(s)'),
    (Number : 3002; Title : 'The package is not installed in any of the projects'),
    (Number : 3003; Title : 'No packages were given to install'),
    (Number : 3004; Title : 'The package file does not exist'),
    (Number : 3005; Title : 'The file is not a valid package file'),
    (Number : 3006; Title : 'The package does not declare any supported platforms'),
    (Number : 3007; Title : 'A cached package does not match the hash the project recorded for it'),
    (Number : 3008; Title : 'Unable to write to the package cache'),
    (Number : 3009; Title : 'Unable to read a package spec from the cache'),
    (Number : 3010; Title : 'Unable to extract the package into the cache'),
    (Number : 4001; Title : 'The project file does not exist'),
    (Number : 4002; Title : 'No project files were found'),
    (Number : 4003; Title : 'Unable to load the project file'),
    (Number : 4004; Title : 'The project contains an invalid package reference'),
    (Number : 4005; Title : 'The project file is missing something DPM needs'),
    (Number : 4006; Title : 'Unable to save the project file'),
    (Number : 5001; Title : 'The Delphi compiler is not installed'),
    (Number : 5002; Title : 'Unable to run msbuild'),
    (Number : 5003; Title : 'A package failed to compile'),
    (Number : 5004; Title : 'A package project could not be inspected before building it'),
    (Number : 5005; Title : 'A package project would run commands during the build'),
    (Number : 5006; Title : 'A prebuilt design package entry is not valid'),
    (Number : 5007; Title : 'A prebuilt design package is missing from the package'),
    (Number : 5008; Title : 'Unable to generate a package project from its package definition'),
    (Number : 6001; Title : 'The spec file does not exist'),
    (Number : 6002; Title : 'The spec file could not be read'),
    (Number : 6003; Title : 'The spec is missing something it needs'),
    (Number : 6004; Title : 'A value in the spec is not valid'),
    (Number : 6005; Title : 'The spec needs a newer version of DPM'),
    (Number : 6101; Title : 'The spec has no template with that name'),
    (Number : 6102; Title : 'A template sets an environment variable DPM does not allow'),
    (Number : 6103; Title : 'Unable to replace the variables in the spec'),
    (Number : 6105; Title : 'A file the spec refers to does not exist'),
    (Number : 6106; Title : 'Unable to write the package file'),
    (Number : 6107; Title : 'The package written is not valid'),
    (Number : 6108; Title : 'Unable to write the package manifest'),
    (Number : 6109; Title : 'Git registry packages cannot be packed'),
    (Number : 6201; Title : 'The server rejected the package'),
    (Number : 6202; Title : 'The source does not accept pushed packages'),
    (Number : 6203; Title : 'The source to push to is disabled'),
    (Number : 7001; Title : 'The package failed verification'),
    (Number : 7002; Title : 'The package is blocked'),
    (Number : 7003; Title : 'The package author signature changed and the install was blocked'),
    (Number : 7004; Title : 'The package no longer carries its repository signature and the install was blocked'),
    (Number : 7101; Title : 'Unable to set up the signing provider'),
    (Number : 7102; Title : 'Unable to sign the package'),
    (Number : 7103; Title : 'A signature could not be built or read'),
    (Number : 7104; Title : 'A certificate could not be used'),
    (Number : 7105; Title : 'Unable to get a timestamp for the signature'),
    (Number : 7106; Title : 'A cryptographic operation failed'),
    (Number : 7107; Title : 'The package manifest is not valid'),
    (Number : 7108; Title : 'Unable to read or write the trust state')
  );

  cWarningInfo : array[TWarningCode] of TDiagnosticInfo = (
    (Number : 4; Title : 'The operation was cancelled'),
    (Number : 1004; Title : 'A package source could not be used'),
    (Number : 1204; Title : 'No hash is recorded for the package, so the download was not verified'),
    (Number : 1205; Title : 'Unable to write the package hash file'),
    (Number : 1206; Title : 'More than one package file matched in a folder source'),
    (Number : 1207; Title : 'A package from a source could not be read'),
    (Number : 1208; Title : 'An entry in a package registry is not valid'),
    (Number : 2010; Title : 'Projects in the group use different versions of a package'),
    (Number : 2011; Title : 'A top level package is newer than the range a package that uses it declares'),
    (Number : 2012; Title : 'Other installed packages have to move with this install'),
    (Number : 3011; Title : 'Some packages do not support one of the project platforms'),
    (Number : 3012; Title : 'A package spec has no target platform or template for the compiler'),
    (Number : 3013; Title : 'The package spec could not be read'),
    (Number : 3014; Title : 'Unable to unload design packages'),
    (Number : 3015; Title : 'Unable to write the package verification receipt'),
    (Number : 3016; Title : 'Unable to write the copy-local targets file'),
    (Number : 4008; Title : 'The project does not target the requested platforms'),
    (Number : 4009; Title : 'The project version does not identify a single compiler version'),
    (Number : 4010; Title : 'The project version does not match the compiler version'),
    (Number : 4011; Title : 'A listed project file does not exist'),
    (Number : 4012; Title : 'The project targets a platform DPM does not support'),
    (Number : 4013; Title : 'The project DPMCompiler value is not a known compiler version'),
    (Number : 5009; Title : 'Unable to save the package build record'),
    (Number : 5010; Title : 'Unable to compile design-time packages for the IDE platform'),
    (Number : 5011; Title : 'Unable to add the build configuration to a package project'),
    (Number : 5012; Title : 'A package path resolves outside the package folder'),
    (Number : 5013; Title : 'A package project has a build event that DPM does not run'),
    (Number : 5014; Title : 'Unable to copy a package file'),
    (Number : 5015; Title : 'A design project declares no platforms DPM supports'),
    (Number : 5016; Title : 'A design project could not be inspected'),
    (Number : 5017; Title : 'A package definition has no platforms'),
    (Number : 5018; Title : 'A package project had no configuration for the platform being built'),
    (Number : 6006; Title : 'A platform in the spec is not valid for its compiler'),
    (Number : 6104; Title : 'A file pattern in the spec matched no files'),
    (Number : 6110; Title : 'A source pattern in the spec uses a variable that is not defined'),
    (Number : 6301; Title : 'A file prepare would write already exists'),
    (Number : 6302; Title : 'The spec declares no platforms'),
    (Number : 7005; Title : 'The package repository signature changed'),
    (Number : 7006; Title : 'The package author signature changed'),
    (Number : 7007; Title : 'A rebuild by the package author, without a repository signature, was allowed'),
    (Number : 7008; Title : 'The package signer is not a trusted publisher'),
    (Number : 7009; Title : 'A signature self-check failed'),
    (Number : 7010; Title : 'The package is not signed')
  );

//'DPME0001'
function ErrorCodeToString(const code : TErrorCode) : string;

//'DPMW0004'
function WarningCodeToString(const code : TWarningCode) : string;

//'error DPME0001: message' - MSBuild's canonical format, which CI log parsers recognise.
function FormatError(const code : TErrorCode; const message : string) : string;

//'warning DPMW0004: message'
function FormatWarning(const code : TWarningCode; const message : string) : string;

//'error DPME0001: message' / 'warning DPMW0004: message', or 'error: message' when code is empty -
//errors and warnings that have not been given a code yet.
function FormatDiagnostic(const isError : boolean; const code : string; const message : string) : string;

type
  ///<summary> Raise this rather than logging an error and then raising - whoever catches it reports it
  ///  once, with its code (LogException in DPM.Core.Logging). </summary>
  EDPMError = class(Exception)
  private
    FCode : TErrorCode;
  public
    //these hide Exception.Create/CreateFmt on purpose - an EDPMError always has a code.
    constructor Create(const code : TErrorCode; const msg : string);
    constructor CreateFmt(const code : TErrorCode; const msg : string; const args : array of const);
    property Code : TErrorCode read FCode;
  end;

  ///<summary> Base for an area's own exception class, which supplies the code - so raising one stays
  ///  `raise ECryptoX509.Create(msg)`. </summary>
  EDPMCodedError = class(EDPMError)
  protected
    class function DefaultCode : TErrorCode; virtual; abstract;
  public
    constructor Create(const msg : string);
    constructor CreateFmt(const msg : string; const args : array of const);
  end;

implementation

function ErrorCodeToString(const code : TErrorCode) : string;
begin
  result := cErrorCodePrefix + Format('%.4d', [cErrorInfo[code].Number]);
end;

function WarningCodeToString(const code : TWarningCode) : string;
begin
  result := cWarningCodePrefix + Format('%.4d', [cWarningInfo[code].Number]);
end;

function FormatDiagnostic(const isError : boolean; const code : string; const message : string) : string;
begin
  if isError then
    result := 'error'
  else
    result := 'warning';
  if code <> '' then
    result := result + ' ' + code;
  result := result + ': ' + message;
end;

function FormatError(const code : TErrorCode; const message : string) : string;
begin
  result := FormatDiagnostic(true, ErrorCodeToString(code), message);
end;

function FormatWarning(const code : TWarningCode; const message : string) : string;
begin
  result := FormatDiagnostic(false, WarningCodeToString(code), message);
end;

{ EDPMError }

constructor EDPMError.Create(const code : TErrorCode; const msg : string);
begin
  inherited Create(msg);
  FCode := code;
end;

constructor EDPMError.CreateFmt(const code : TErrorCode; const msg : string; const args : array of const);
begin
  inherited CreateFmt(msg, args);
  FCode := code;
end;

{ EDPMCodedError }

constructor EDPMCodedError.Create(const msg : string);
begin
  inherited Create(DefaultCode, msg);
end;

constructor EDPMCodedError.CreateFmt(const msg : string; const args : array of const);
begin
  inherited CreateFmt(DefaultCode, msg, args);
end;

end.
