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

unit DPM.IDE.TrustPromptForm;

// IDE-4: Modal dialog shown when DPM.Trust.State detects a previously-signed
// package now arriving unsigned (or signed by a different key) and the
// active authorDowngradePolicy is `prompt`. The user picks: trust this
// build, block this build, or block this package permanently.

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TTrustPromptResult = (tprTrustOnce, tprBlockOnce, tprBlockAlways);

  // Programmatically constructed (no .dfm) — keeps the form working across
  // every supported Delphi IDE version without needing a designer round-trip.
  TTrustPromptForm = class(TForm)
  private
    FResult : TTrustPromptResult;
    FPackage : TLabel;
    FPrev    : TLabel;
    FNew     : TLabel;
    procedure BuildLayout;
    procedure OnTrust(Sender : TObject);
    procedure OnBlock(Sender : TObject);
    procedure OnAlways(Sender : TObject);
  public
    constructor CreateNew(AOwner : TComponent; Dummy : integer = 0); reintroduce;
    class function Prompt(const packageId, version : string;
                          const prevSpki : string;
                          newSigned : boolean;
                          const newSpki : string;
                          out decision : TTrustPromptResult) : boolean;
  end;

implementation

constructor TTrustPromptForm.CreateNew(AOwner : TComponent; Dummy : integer);
begin
  inherited CreateNew(AOwner);
  Position := poScreenCenter;
  BorderStyle := bsDialog;
  Caption := 'DPM — author signing changed';
  ClientWidth := 540;
  ClientHeight := 220;
  BuildLayout;
end;

procedure TTrustPromptForm.BuildLayout;

  function MakeLabel(top : integer; const caption : string; bold : boolean = false) : TLabel;
  begin
    result := TLabel.Create(Self);
    result.Parent := Self;
    result.Left := 16;
    result.Top := top;
    result.Width := ClientWidth - 32;
    result.AutoSize := false;
    result.WordWrap := true;
    result.Height := 36;
    result.Caption := caption;
    if bold then
      result.Font.Style := result.Font.Style + [fsBold];
  end;

  function MakeButton(left : integer; const caption : string;
                      handler : TNotifyEvent) : TButton;
  begin
    result := TButton.Create(Self);
    result.Parent := Self;
    result.Left := left;
    result.Top := ClientHeight - 40;
    result.Width := 160;
    result.Height := 28;
    result.Caption := caption;
    result.OnClick := handler;
  end;

begin
  MakeLabel(10, 'DPM has detected an author-signing change for this package.', True);
  FPackage := MakeLabel(40, '');
  FPrev := MakeLabel(80, '');
  FNew := MakeLabel(120, '');

  MakeButton(16, 'Trust this build', OnTrust);
  MakeButton(184, 'Block this build', OnBlock);
  MakeButton(352, 'Always block', OnAlways);
end;

procedure TTrustPromptForm.OnTrust(Sender : TObject);
begin
  FResult := tprTrustOnce;
  ModalResult := mrOK;
end;

procedure TTrustPromptForm.OnBlock(Sender : TObject);
begin
  FResult := tprBlockOnce;
  ModalResult := mrCancel;
end;

procedure TTrustPromptForm.OnAlways(Sender : TObject);
begin
  FResult := tprBlockAlways;
  ModalResult := mrAbort;
end;

class function TTrustPromptForm.Prompt(const packageId, version : string;
                                       const prevSpki : string;
                                       newSigned : boolean;
                                       const newSpki : string;
                                       out decision : TTrustPromptResult) : boolean;
var
  form : TTrustPromptForm;
begin
  form := TTrustPromptForm.CreateNew(nil);
  try
    form.FPackage.Caption := Format('Package: %s %s', [packageId, version]);
    form.FPrev.Caption    := 'Previously signed by: sha256:' + prevSpki;
    if newSigned then
      form.FNew.Caption := 'This build is signed by a different key: sha256:' + newSpki
    else
      form.FNew.Caption := 'This build is UNSIGNED.';
    form.ShowModal;
    decision := form.FResult;
    result := decision = tprTrustOnce;
  finally
    form.Free;
  end;
end;

end.
