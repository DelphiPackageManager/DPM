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

unit DPM.IDE.AddInOptions;

interface

uses
  ToolsApi,
  Vcl.Forms,
  DPM.IDE.AddInOptionsFrame,
  Spring.Container,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces;

type
  TDPMAddinOptions = class(TInterfacedObject, INTAAddInOptions)
  private
    FConfigManager : IConfigurationManager;
    FLogger : ILogger;
    FFrame  : TDPMOptionsFrame;
  protected
    procedure DialogClosed(Accepted: Boolean);
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    function GetHelpContext: Integer;
    function IncludeInIDEInsight: Boolean;
    function ValidateContents: Boolean;
  public
    constructor Create(const container : TContainer);
  end;

implementation


{ TDPMAddinOptions }

constructor TDPMAddinOptions.Create(const container: TContainer);
begin
  FConfigManager := container.Resolve<IConfigurationManager>;
  FLogger := container.Resolve<ILogger>;
end;

procedure TDPMAddinOptions.DialogClosed(Accepted: Boolean);
begin
  if Accepted then
    FFrame.SaveSettings;
end;

procedure TDPMAddinOptions.FrameCreated(AFrame: TCustomFrame);
begin
  FFrame := TDPMOptionsFrame(AFrame);
  FFrame.SetConfigManager(FConfigManager);
  FFrame.SetLogger(FLogger);
  FFrame.LoadSettings;
end;

function TDPMAddinOptions.GetArea: string;
begin
  result := '';
end;

function TDPMAddinOptions.GetCaption: string;
begin
  result := 'DPM Package Manager';
end;

function TDPMAddinOptions.GetFrameClass: TCustomFrameClass;
begin
  result := TDPMOptionsFrame;
end;

function TDPMAddinOptions.GetHelpContext: Integer;
begin
  result := -1;
end;

function TDPMAddinOptions.IncludeInIDEInsight: Boolean;
begin
  result := true;
end;

function TDPMAddinOptions.ValidateContents: Boolean;
begin
  result := FFrame.Validate;
end;

end.
