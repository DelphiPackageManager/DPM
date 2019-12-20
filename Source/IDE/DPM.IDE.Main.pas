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

unit DPM.IDE.Main;

interface

Uses
  ToolsAPI,
  WinApi.Windows,
  System.SysUtils,
  Vcl.Dialogs,
  DPM.IDE.Wizard;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;


Exports
  InitWizard name ToolsAPI.WizardEntryPoint;

implementation

uses
  Vcl.Graphics,
  DPM.IDE.Constants;

var
  SplashImage: TBitmap;

function CreateWizard(const BorlandIDEServices: IBorlandIDEServices) : IOTAWizard;
begin
  try
    result := TDPMWizard.Create;
    SplashImage := Vcl.Graphics.TBitmap.Create;
    SplashImage.LoadFromResourceName(HInstance, 'DPMIDELOGO');
    SplashScreenServices.AddPluginBitmap(sWizardTitle ,SplashImage.Handle);

    (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(sWizardTitle,  sWizardTitle  , SplashImage.Handle);

  except
    on E: Exception do
    begin
      MessageDlg('Failed to load wizard splash image', mtError, [mbOK], 0);
      OutputDebugString('Failed to load splash image');
      result := nil;
    end;
  end;

end;


function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;  //FI:O804
var
  wizard : IOTAWizard;
begin
  try
    wizard := CreateWizard(BorlandIDEServices);
    if wizard <> nil then
    begin
      RegisterProc(wizard);
      Result := True;
    end
    else
      Result := False;

  except
    on E: Exception do
    begin
      MessageDlg('Failed to load wizard. internal failure:' + E.ClassName + ':'
        + E.Message, mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;



end.
