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

unit DPM.Core.Constants;

interface

const
  cDPMClientVersion =  '1.0.0';
  cLockFileVersion = '0.0.1';
  cPackageFileExt = '.dpkg';
  cPackageSpecExt = '.dspec.yaml';
  cPackageHashAlgorithm = 'sha256';
  cPackageHashAlgorithmExt = '.' + cPackageHashAlgorithm;

  //fixed filename for the package dspec inside the .dpkg so we can easily extract it
  cPackageDspecFile = 'package' + cPackageSpecExt;

  //marks a git registry package that was cloned + built in place in the cache. Such
  //packages are unsigned source (no .dpkg / manifest / receipt) by design, so the
  //cache identifies them by this marker and skips the signing verification gate.
  //Also holds the git commit the clone was built from (for HEAD tracking of untagged repos).
  cGitPackageMarkerFile = '.dpm-git-commit';
  cIconFileSVG = 'icon.svg';
  cIconFilePNG = 'icon.png';

  cDPMConfigFileName = 'dpm.config.yaml';

  cDefaultDPMFolder = '%APPDATA%\.dpm';
  cDefaultConfigFile = cDefaultDPMFolder + '\' + cDPMConfigFileName;
  cDefaultPackageCache = cDefaultDPMFolder + '\package_cache';
  cDPMPackageCacheEnviromentVar = 'DPMPACKAGECACHE';

  //local mirror folder for git-URL package registries (folder registries are read in place)
  cDefaultRegistriesFolder = cDefaultDPMFolder + '\registries';

  //default minutes between auto-pulls of a git-URL registry mirror. 0 = always pull,
  //negative = never auto-pull (explicit `dpm sources refresh` only).
  cDefaultRegistryRefreshMinutes = 60;

  cTargetPlatformAttribute = 'targetPlatform';
  cUnset = '--unset--';

  cMaxPackageIdLength = 100;

  cPackageFileRegex = '^((?:\w+)(?:\.\w+)+)\-([^\-]+)\-([^\-]+)\-(.*)$';
  cPackageIdRegex = '^((?:\w+)(?:\.\w+)+)$';

  //template regexs
  //compiler version
  cPackageFileRegexCV = '^((?:\w+)(?:\.\w+)+)\-(%s)\-([^\-]+)\-(.*)$';
  //compiler version and platform
  cPackageFileRegexCVP = '^((?:\w+)(?:\.\w+)+)\-(%s)\-(%s)\-(.*)$';

  //compiler version and platform and package version
  cPackageFileRegexCVPV = '^((?:\w+)(?:\.\w+)+)\-(%s)\-(%s)\-(%s)$';

  //compiler version and package version - don't think this would ever be used?
  cPackageFileRegexCVV = '^((?:\w+)(?:\.\w+)+)\-(%s)\-(%s)\-(%s)$';

  //platform
  cPackageFileRegexP = '^((?:\w+)(?:\.\w+)+)\-([^\-]+)\-(%s)\-(.*)$';

  //platform and version
  cPackageFileRegexPV = '^((?:\w+)(?:\.\w+)+)\-(%s)\-(%s)\-(.*)$';

  //version
  cPackageFileRegexV = '^((?:\w+)(?:\.\w+)+)\-([^\-]+)\-([^\-]+)\-(%s)$';

  cRootNode = 'root-node';
  cUserAgentHeader = 'User-Agent';
  cDPMUserAgent  = 'DPM ' + cDPMClientVersion;
  cClientVersionHeader = 'Client-Version';

  cDefaultSourceName = 'DPM';
  cDefaultSourceUrl = 'https://delphi.dev/api/v2/index.json';

function GetIconArchiveFileName(const sourcePath : string) : string;

implementation

uses
  System.SysUtils;

function GetIconArchiveFileName(const sourcePath : string) : string;
begin
  if SameText(ExtractFileExt(sourcePath), '.svg') then
    result := cIconFileSVG
  else
    result := cIconFilePNG;
end;

end.

