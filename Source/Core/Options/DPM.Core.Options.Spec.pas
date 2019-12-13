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

unit DPM.Core.Options.Spec;

interface


uses
  DPM.Core.Options.Base;

type
  TSpecOptions = class(TOptionsBase)
  private
    FPackageId : string;
    FFromProject : string;
    FOverwrite : boolean;
    FNoFlatten : boolean;
    class var
      FDefault : TSpecOptions;
  public
    class constructor CreateDefault;
    class property Default : TSpecOptions read FDefault;
    property PackageId : string read FPackageId write FPackageId;
    property FromProject : string read FFromProject write FFromProject;
    property Overwrite : boolean read FOverwrite write FOverwrite;
    property NoFlatten : boolean read FNoFlatten write FNoFlatten;
  end;


implementation

{ TSpecOptions }

class constructor TSpecOptions.CreateDefault;
begin
  FDefault := TSpecOptions.Create;
end;

end.
