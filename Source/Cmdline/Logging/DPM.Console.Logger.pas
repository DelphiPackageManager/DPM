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

unit DPM.Console.Logger;

interface

uses
  DPM.Core.Logging,
  DPM.Console.Writer;

type
  TDPMConsoleLogger = class(TInterfacedObject, ILogger)
  private
    FConsole : IConsoleWriter;
    FLogLevel : TLogLevel;
  protected
    procedure Debug(const data: string);
    procedure Error(const data: string);
    procedure Information(const data: string; const important : boolean = false);
    procedure Verbose(const data: string);
    procedure Warning(const data: string);

  public
    constructor Create(const console : IConsoleWriter);
  end;

implementation

{ TDPMConsoleLogger }

constructor TDPMConsoleLogger.Create(const console: IConsoleWriter);
begin
  FConsole := console;
  FLogLevel := TLogLevel.Debug; //TODO : make this configurable.
end;

procedure TDPMConsoleLogger.Debug(const data: string);
begin
  if FLogLevel > TLogLevel.Debug then
    exit;
  FConsole.SetColour(ccWhite);
  FConsole.Write(data);
end;

procedure TDPMConsoleLogger.Error(const data: string);
begin
  FConsole.SetColour(ccBrightRed);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

procedure TDPMConsoleLogger.Information(const data: string; const important : boolean);
begin
  if FLogLevel > TLogLevel.Information then
    exit;

  if important then
    FConsole.SetColour(ccBrightWhite)
  else
    FConsole.SetColour(ccDefault);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

procedure TDPMConsoleLogger.Verbose(const data: string);
begin
  if FLogLevel > TLogLevel.Verbose then
    exit;

  FConsole.SetColour(ccWhite);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

procedure TDPMConsoleLogger.Warning(const data: string);
begin
  if FLogLevel > TLogLevel.Warning then
    exit;

  FConsole.SetColour(ccBrightYellow);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

end.
