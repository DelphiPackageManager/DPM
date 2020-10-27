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

unit DPM.Core.Constants;

interface

const
  cDPMClientVersion = '1.0.0-alpha1';
  cLockFileVersion = '0.0.1';
  cLockFileExt = '.dpmlock';
  cPackageFileExt = '.dpkg';
  cPackageSpecExt = '.dspec';
  //fixed filename for the package dspec so we can easily extract it
  cPackageMetaFile = 'package' + cPackageSpecExt;
  cIconFileSVG = 'icon.svg';
  cIconFilePNG = 'icon.png';

  cDPMConfigFileName = 'dpm.config';

  cDefaultDPMFolder = '%APPDATA%\.dpm';
  cDefaultConfigFile = cDefaultDPMFolder + '\' + cDPMConfigFileName;
  cDefaultPackageCache = cDefaultDPMFolder + '\packages';
  cDPMPackageCacheEnviromentVar = 'DPMPACKAGECACHE';

  cTargetPlatformAttribute = 'targetPlatform';
  cUnset = '--unset--';

  cMaxPackageIdLength = 100;

  cPackageFileRegex = '^(\w+\.\w+)\-([^\-]+)\-([^\-]+)\-(.*)$';
  cPackageIdRegex = '^(\w+\.\w+)$';

  //template regexs
  //compiler version
  cPackageFileRegexCV = '^(\w+\.\w+)\-(%s)\-([^\-]+)\-(.*)$';
  //compiler version and platform
  cPackageFileRegexCVP = '^(\w+\.\w+)\-(%s)\-(%s)\-(.*)$';

  //compiler version and platform and package version
  cPackageFileRegexCVPV = '^(\w+\.\w+)\-(%s)\-(%s)\-(%s)$';

  //compiler version and package version - don't think this would ever be used?
  cPackageFileRegexCVV = '^(\w+\.\w+)\-(%s)\-(%s)\-(%s)$';

  //platform
  cPackageFileRegexP = '^(\w+\.\w+)\-([^\-]+)\-(%s)\-(.*)$';

  //platform and version
  cPackageFileRegexPV = '^(\w+\.\w+)\-(%s)\-(%s)\-(.*)$';

  //version
  cPackageFileRegexV = '^(\w+\.\w+)\-([^\-]+)\-([^\-]+)\-(%s)$';

implementation

end.

