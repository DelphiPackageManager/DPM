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

unit DPM.Console.Logger;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Console.Writer;

type
  TDPMConsoleLogger = class(TBaseLogger)
  private
    FConsole : IConsoleWriter;
  protected
    procedure WriteMessage(const level : TLogLevel; const text : string; const important : boolean); override;
    procedure DoNewLine; override;
  public
    constructor Create(const console : IConsoleWriter);
  end;

implementation

{ TDPMConsoleLogger }

constructor TDPMConsoleLogger.Create(const console: IConsoleWriter);
begin
  inherited Create;
  FConsole := console;
  Verbosity := TVerbosity.Debug;
end;

procedure TDPMConsoleLogger.DoNewLine;
begin
  FConsole.WriteLine(' ');
end;

procedure TDPMConsoleLogger.WriteMessage(const level : TLogLevel; const text : string; const important : boolean);
begin
  case level of
    TLogLevel.Debug : FConsole.SetColour(ccGrey);
    TLogLevel.Verbose,
    TLogLevel.Information :
    begin
      if important then
        FConsole.SetColour(ccBrightWhite)
      else
        FConsole.SetColour(ccWhite);
    end;
    TLogLevel.Success :
    begin
      if important then
        FConsole.SetColour(ccBrightGreen)
      else
        FConsole.SetColour(ccDarkGreen);
    end;
    TLogLevel.Warning :
    begin
      if important then
        FConsole.SetColour(ccBrightYellow)
      else
        FConsole.SetColour(ccDarkYellow);
    end;
    TLogLevel.Error : FConsole.SetColour(ccBrightRed);
  end;
  FConsole.WriteLine(text);
  FConsole.SetColour(ccDefault);
end;

end.
