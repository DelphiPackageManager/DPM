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

unit DPM.Core.Options.SearchCmd;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Search;

type
  ///<summary>
  ///  Options for the 'search' command.
  ///</summary>
  ///<remarks>
  ///  TSearchOptions (DPM.Core.Options.Search) is the shared base used by list, install and the
  ///  dependency resolver. This adds only the Default singleton, the command's own defaults,
  ///  and the compiler requirement - hence the SearchCmd unit name, to keep the two apart.
  ///</remarks>
  TSearchCmdOptions = class(TSearchOptions)
  private
    class var
      FDefault : TSearchCmdOptions;
  public
    class constructor CreateDefault;
    class destructor DestroyDefault;
    class property Default : TSearchCmdOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : boolean; override;
  end;

implementation

uses
  DPM.Core.Diagnostics;

{ TSearchCmdOptions }

constructor TSearchCmdOptions.Create;
begin
  inherited;
  //TSearchOptions leaves Commercial and Trial false, which silently drops every commercial and
  //trial package from the results with no indication anything was filtered. TListOptions makes
  //the same correction for the same reason.
  Prerelease := false;
  Commercial := true;
  Trial := true;
  Skip := 0;
  Take := 200;
end;

class constructor TSearchCmdOptions.CreateDefault;
begin
  FDefault := TSearchCmdOptions.Create;
end;

//the singleton goes with the unit - it was never freed, so every run reported it as a leak.
class destructor TSearchCmdOptions.DestroyDefault;
begin
  FDefault.Free;
end;

function TSearchCmdOptions.Validate(const logger : ILogger) : boolean;
begin
  result := inherited Validate(logger);

  //The feed endpoint always sends a compiler parameter, so an unset one would be sent as
  //'unknownversion' and quietly return nothing useful. Packages are published per compiler
  //version, so there is no sensible default to guess here - ask.
  if CompilerVersion = TCompilerVersion.UnknownVersion then
  begin
    logger.Error(TErrorCode.E0101_MissingArgument, 'The --compiler option is required, eg --compiler=12.0 - DPM packages are published separately for each Delphi compiler version.');
    result := false;
  end;

  FValidated := true;
  FIsValid := result;
end;

end.
