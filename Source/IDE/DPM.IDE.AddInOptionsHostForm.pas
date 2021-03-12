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

unit DPM.IDE.AddInOptionsHostForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  DPM.IDE.AddInOptionsFrame,
  DPM.IDE.Options,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TDPMOptionsHostForm = class(TForm)
    DPMOptionsFrame : TDPMOptionsFrame;
    Panel1 : TPanel;
    btnCancel : TButton;
    btnOK : TButton;
    procedure btnOKClick(Sender : TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent; const configManager : IConfigurationManager; const logger : ILogger; const ideOptions : IDPMIDEOptions; const configFile : string); reintroduce;
  end;

var
  DPMOptionsHostForm : TDPMOptionsHostForm;

implementation

uses
  ToolsApi;

{$R *.dfm}

{$I DPMIDE.inc}

{ TDPMOptionsHostForm }

procedure TDPMOptionsHostForm.btnOKClick(Sender : TObject);
begin
  if DPMOptionsFrame.Validate then
  begin
    DPMOptionsFrame.SaveSettings;
    Self.ModalResult := mrOK;
  end;
end;

constructor TDPMOptionsHostForm.Create(AOwner : TComponent; const configManager : IConfigurationManager; const logger : ILogger;  const ideOptions : IDPMIDEOptions; const configFile : string);
begin
  inherited Create(AOwner);
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont, seClient, seBorder];
  {$ENDIF}
  {$IFDEF THEMESERVICES}
  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(Self);
  {$ENDIF}

  DPMOptionsFrame.Configure(configManager, ideOptions, logger, configFile);
  Self.Caption := 'DPM Options [' + configFile + ']';
  DPMOptionsFrame.LoadSettings;
end;

{$IFDEF THEMESERVICES}
initialization
  (BorlandIDEServices as IOTAIDEThemingServices250).RegisterFormClass(TDPMOptionsHostForm);
  {$ENDIF}

end.

