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
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Console.Writer;

type
  TDPMConsoleLogger = class(TInterfacedObject, ILogger)
  private
    FConsole : IConsoleWriter;
    FVerbosity : TVerbosity;
  protected
    procedure Debug(const data: string);
    procedure Error(const data: string);
    procedure Information(const data: string; const important : boolean = false);
    procedure Success(const data: string; const important : boolean = false);
    procedure Verbose(const data: string; const important : boolean = false);
    procedure Warning(const data: string; const important : boolean = false);
    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);
    procedure Clear;
    procedure NewLine;
  public
    constructor Create(const console : IConsoleWriter);
  end;

implementation

{ TDPMConsoleLogger }

procedure TDPMConsoleLogger.Clear;
begin
  //no-op
end;

constructor TDPMConsoleLogger.Create(const console: IConsoleWriter);
begin
  FConsole := console;
  FVerbosity := TVerbosity.Debug;
end;

procedure TDPMConsoleLogger.Debug(const data: string);
begin
  if FVerbosity < TVerbosity.Debug then
    exit;

  FConsole.SetColour(ccGrey);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

procedure TDPMConsoleLogger.Error(const data: string);
begin
  //always log errors

  FConsole.SetColour(ccBrightRed);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

function TDPMConsoleLogger.GetVerbosity: TVerbosity;
begin
  result := FVerbosity;
end;

procedure TDPMConsoleLogger.Information(const data: string; const important : boolean);
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;

  if important then
    FConsole.SetColour(ccBrightWhite)
  else
    FConsole.SetColour(ccWhite);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

procedure TDPMConsoleLogger.NewLine;
begin
  FConsole.WriteLine(' ');
end;

procedure TDPMConsoleLogger.SetVerbosity(const value: TVerbosity);
begin
  FVerbosity := value;
end;

procedure TDPMConsoleLogger.Success(const data: string; const important: boolean);
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;

  if important then
    FConsole.SetColour(ccBrightGreen)
  else
    FConsole.SetColour(ccDarkGreen);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

procedure TDPMConsoleLogger.Verbose(const data: string; const important : boolean);
begin
  if (FVerbosity < TVerbosity.Detailed) then
    exit;

  if important then
    FConsole.SetColour(ccBrightWhite)
  else
    FConsole.SetColour(ccWhite);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

procedure TDPMConsoleLogger.Warning(const data: string; const important : boolean);
begin
  //always log warnings
  if important then
    FConsole.SetColour(ccBrightYellow)
  else
    FConsole.SetColour(ccDarkYellow);
  FConsole.Write(data);
  FConsole.SetColour(ccDefault);
end;

end.
