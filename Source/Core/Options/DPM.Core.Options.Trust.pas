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

unit DPM.Core.Options.Trust;

interface

uses
  DPM.Core.Logging,
  DPM.Core.Options.Base;

type
  TTrustSubCommand = (tsList, tsAdd, tsRemove, tsShow);
  TTrustKind = (tkPublisher, tkRepository);

  TTrustOptions = class(TOptionsBase)
  private
    FCommand : TTrustSubCommand;
    FKind    : TTrustKind;
    FName    : string;
    FUrl     : string;
    FSpki    : string;
    class var FDefault : TTrustOptions;
  public
    class constructor CreateDefault;
    class property Default : TTrustOptions read FDefault;
    function Validate(const logger : ILogger) : boolean; override;

    property Command : TTrustSubCommand read FCommand write FCommand;
    property Kind : TTrustKind read FKind write FKind;
    property Name : string read FName write FName;
    property Url : string read FUrl write FUrl;
    property Spki : string read FSpki write FSpki;
  end;

implementation

class constructor TTrustOptions.CreateDefault;
begin
  FDefault := TTrustOptions.Create;
  FDefault.FCommand := tsList;
  FDefault.FKind := tkPublisher;
end;

function TTrustOptions.Validate(const logger : ILogger) : boolean;
begin
  result := true;
  case FCommand of
    tsAdd :
      begin
        if FSpki = '' then
        begin
          logger.Error('trust add requires --spki sha256:...');
          result := false;
        end;
        if FKind = tkRepository then
        begin
          if FUrl = '' then
          begin
            logger.Error('trust add repository requires --url <repository url>');
            result := false;
          end;
        end
        else if FName = '' then
        begin
          logger.Error('trust add publisher requires --name <display name>');
          result := false;
        end;
      end;
    tsRemove :
      begin
        if FSpki = '' then
        begin
          logger.Error('trust remove requires --spki sha256:... to identify the entry to remove.');
          result := false;
        end;
      end;
  end;
end;

end.
