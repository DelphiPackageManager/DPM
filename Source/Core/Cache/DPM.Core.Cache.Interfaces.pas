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

unit DPM.Core.Cache.Interfaces;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
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
    function CreatePackagePath(const packageId : IPackageIdentity) : string;

    function GetPackagePath(const packageId : IPackageIdentity) : string;overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string;overload;

    /// <summary>
    ///  Folder where the raw .dpkg file for the given package id lives -
    ///  {cache}/{compiler}/{id}. Sibling of the per-version extraction folders
    ///  returned by GetPackagePath. Does not create the folder.
    /// </summary>
    function GetPackageFileFolder(const packageId : IPackageIdentity) : string;

    /// <summary>
    ///  checks if the package is present as a folder, if not there but the file is
    ///  then it will call InstallPackage to extract it.
    /// </summary>
    function EnsurePackage(const packageId : IPackageIdentity) : boolean;

    // skipTrustRatchets bypasses the TOFU author-downgrade and repository
    // assurance ratchets (signature verification itself still runs). Used by the
    // DSpecCreator test workflow where a freshly built local .dpkg legitimately
    // lacks the repository signature its published counterpart carries.
    function InstallPackageFromFile(const packageFileName : string; const skipTrustRatchets : boolean = false) : boolean;

    // Scoped equivalent of the skipTrustRatchets parameter above: while set, every
    // ratchet evaluation in the cache (InstallPackageFromFile AND the cache-hit
    // re-check in EnsurePackage) is skipped. The installer turns this on for the
    // duration of a test cache build so the package's own re-validation during the
    // build step doesn't re-trigger the ratchet. The cache is a singleton, so the
    // caller must reset it (the installer does so in a finally). Off by default.
    procedure SetSkipTrustRatchets(const value : boolean);

    //gets the package info with dependencies. Calls EnsurePackage.
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;

    //gets the full package metadata including search paths.
    function GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;

    //gets the deserialized dspec file for the package.
    function GetPackageSpec(const packageId : IPackageIdentity) : IPackageSpec;

    //gets the platforms supported by a package from its dspec.
    function GetPackagePlatforms(const packageId : IPackageIdentity) : TDPMPlatforms;

    /// <summary>
    ///  Returns IPackageInfo for any cached versions of the given (id, compilerVersion)
    ///  that satisfy versionRange. Used by the dependency resolver to avoid hitting
    ///  the repository for packages already on disk. Returns an empty list when no
    ///  cached version satisfies the range. Sorted descending by version. Prereleases
    ///  are excluded unless preRelease is true.
    /// </summary>
    function GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                      const id : string;
                                                      const compilerVersion : TCompilerVersion;
                                                      const versionRange : TVersionRange;
                                                      const preRelease : boolean) : IList<IPackageInfo>;

    /// <summary>
    ///  Returns the package's icon directly from the extracted cache folder
    ///  (icon.svg / icon.png), avoiding a network call. Used by the IDE for
    ///  installed packages. Returns false (and icon = nil) when the package
    ///  is not in the cache or has no embedded icon.
    /// </summary>
    function TryGetPackageIcon(const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;

    /// <summary>
    ///  Resolves the SHA-256 hash of the package's .dpkg file. Self-heals
    ///  missing sidecars: if the .sha256 file is absent but the .dpkg is in
    ///  the cache, computes the hash and persists the sidecar so subsequent
    ///  calls and any other tooling (SBOM, audits, signature verification)
    ///  can read it without recomputing. Returns empty if even the .dpkg
    ///  cannot be located.
    ///  The algorithm is always 'sha256' (see cPackageHashAlgorithm).
    /// </summary>
    function GetPackageHash(const packageId : IPackageIdentity) : string;

    /// <summary>
    /// Re-hash every cached package against its manifest and re-run signature
    /// verification (V-34). Used by `dpm cache verify` and the IDE DPM menu.
    /// Logs per-package progress and honours cancellationToken between packages
    /// so a long-running verify can be interrupted. Returns the number of
    /// packages that failed verification.
    /// </summary>
    function FullReVerify(const cancellationToken : ICancellationToken) : integer;

    /// <summary>
    ///  Returns the cached package versions matching the filter, for `dpm cache
    ///  remove`. A blank version matches every cached version of the id; passing
    ///  UnknownVersion as compilerVersion matches every compiler folder. Unions
    ///  extracted version folders and raw .dpkg filenames so a downloaded-but-
    ///  never-extracted package is still found. Empty list when nothing matches.
    /// </summary>
    function GetCachedPackagesMatching(const id : string; const compilerVersion : TCompilerVersion; const version : string) : IList<IPackageIdentity>;

    /// <summary>
    ///  Removes a single cached id-compiler-version: deletes its extracted
    ///  folder and the matching .dpkg + .sha256 sidecar so the next install
    ///  re-downloads, and drops the in-memory memo entry. Also drops the
    ///  per-user TOFU trust state (author + repository ratchets) for the id -
    ///  trust state is keyed by id only, so removing the cached package resets
    ///  it, otherwise repacking/re-testing a differently signed build of the
    ///  same id would trip the ratchet and fail. Returns true if anything was
    ///  deleted.
    /// </summary>
    function RemovePackage(const packageId : IPackageIdentity) : boolean;

    property Location : string read GetLocation write SetLocation;
    property PackagesFolder : string read GetPackagesFolder;
  end;

implementation

end.

