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

unit DPM.IDE.AboutForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TDPMAboutForm = class(TForm)
    Image1 : TImage;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    githubLinkLabel : TLinkLabel;
    Label4 : TLabel;
    procedure githubLinkLabelLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DPMAboutForm : TDPMAboutForm;

implementation

uses
  Winapi.ShellAPI;

{$R *.dfm}

procedure TDPMAboutForm.githubLinkLabelLinkClick(Sender : TObject; const Link : string; LinkType : TSysLinkType);
begin
  ShellExecute(0, 'Open', PChar(Link), nil, nil, SW_SHOWNORMAL);
end;

end.

