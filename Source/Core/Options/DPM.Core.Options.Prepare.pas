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

unit DPM.Core.Options.Prepare;

interface

uses
  DPM.Core.Options.Base;

type
  TPrepareOptions = class(TOptionsBase)
  private
    FSpecFile : string;
    FForce : boolean;
    FDryRun : boolean;
    class var
      FDefault : TPrepareOptions;
  public
    class constructor CreateDefault;
    class property Default : TPrepareOptions read FDefault;
    constructor Create; override;

    //Optional - if empty, the prepare command discovers a *.dspec in the current directory.
    property SpecFile : string read FSpecFile write FSpecFile;
    //When true, propagated dpk/dproj files overwrite existing targets in other version folders.
    //Default is to skip existing targets.
    property Force : boolean read FForce write FForce;
    //When true, no files are created or modified - the command logs what it would do.
    //Useful for previewing destructive operations before committing to them.
    property DryRun : boolean read FDryRun write FDryRun;
  end;

implementation

{ TPrepareOptions }

constructor TPrepareOptions.Create;
begin
  inherited;
  FForce := false;
  FDryRun := false;
end;

class constructor TPrepareOptions.CreateDefault;
begin
  FDefault := TPrepareOptions.Create;
end;

end.
