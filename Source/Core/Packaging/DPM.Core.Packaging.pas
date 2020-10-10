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

unit DPM.Core.Packaging;

interface

uses
  System.SysUtils,
  System.Classes,
  Spring.Container.Common,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Options.Pack,
  DPM.Core.Types,
  DPM.Core.TargetPlatform,
  DPM.Core.Packaging.Archive;

type
  {$M+}
  IPackageWriter = interface
  ['{10944B58-5766-4B73-9C7F-C8488151E42B}']
    function WritePackageFromSpec(const cancellationToken : ICancellationToken; const options : TPackOptions) : boolean;
  end;

implementation

end.
