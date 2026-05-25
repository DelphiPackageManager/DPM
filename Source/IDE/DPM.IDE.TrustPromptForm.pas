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

// IDE-4: Modal dialog shown when a trust-state ratchet (author downgrade or
// repository assurance / V-24) would otherwise hard-fail the install. The
// form itself is text-agnostic: callers supply the heading, summary,
// consequence and technical-details strings. The user picks override
// (proceed and let the caller clear the relevant trust entry), cancel
// this install, or permanently block the package id.

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TTrustPromptResult = (tprOverride, tprCancel, tprBlockAlways);

  // Programmatically constructed (no .dfm) — keeps the form working across
  // every supported Delphi IDE version without needing a designer round-trip.
  TTrustPromptForm = class(TForm)
  private
    FResult        : TTrustPromptResult;
    FHeading       : TLabel;
    FSummary       : TLabel;
    FConsequence   : TLabel;
    FTechHeader    : TLabel;
    FTechPackage   : TLabel;
    FTechPrev      : TLabel;
    FTechNew       : TLabel;
    procedure BuildLayout;
    procedure OnOverride(Sender : TObject);
    procedure OnCancelInstall(Sender : TObject);
    procedure OnAlways(Sender : TObject);
  public
    constructor CreateNew(AOwner : TComponent; Dummy : integer = 0); reintroduce;
    class function Prompt(const heading, summary, consequence : string;
                          const techPackage, techPrev, techNew : string;
                          out decision : TTrustPromptResult) : boolean;
  end;

implementation

const
  cFormWidth   = 600;
  cFormHeight  = 340;
  cMargin      = 16;
  cButtonTop   = cFormHeight - 44;
  cButtonWidth = 180;
  cButtonHeight = 28;

constructor TTrustPromptForm.CreateNew(AOwner : TComponent; Dummy : integer);
begin
  inherited CreateNew(AOwner);
  Position := poScreenCenter;
  BorderStyle := bsDialog;
  Caption := 'DPM — package signing changed';
  ClientWidth := cFormWidth;
  ClientHeight := cFormHeight;
  // Default to Cancel so closing the dialog via [X] or Esc does the safe
  // thing — the install is aborted, the trust state is untouched.
  FResult := tprCancel;
  BuildLayout;
end;

procedure TTrustPromptForm.BuildLayout;

  function MakeLabel(top, height : integer; const caption : string;
                     bold : boolean = false; muted : boolean = false) : TLabel;
  begin
    result := TLabel.Create(Self);
    result.Parent := Self;
    result.Left := cMargin;
    result.Top := top;
    result.Width := ClientWidth - (cMargin * 2);
    result.AutoSize := false;
    result.WordWrap := true;
    result.Height := height;
    result.Caption := caption;
    if bold then
      result.Font.Style := result.Font.Style + [fsBold];
    if muted then
    begin
      result.Font.Size := result.Font.Size - 1;
      result.Font.Color := clGrayText;
    end;
  end;

  function MakeButton(left : integer; const caption : string;
                      handler : TNotifyEvent) : TButton;
  begin
    result := TButton.Create(Self);
    result.Parent := Self;
    result.Left := left;
    result.Top := cButtonTop;
    result.Width := cButtonWidth;
    result.Height := cButtonHeight;
    result.Caption := caption;
    result.OnClick := handler;
  end;

var
  buttonGap : integer;
begin
  FHeading     := MakeLabel(10, 22, '', true);
  FSummary     := MakeLabel(38, 56, '');
  FConsequence := MakeLabel(98, 40, '');

  FTechHeader  := MakeLabel(146, 16, 'Technical details', false, true);
  FTechPackage := MakeLabel(164, 16, '', false, true);
  FTechPrev    := MakeLabel(182, 16, '', false, true);
  FTechNew     := MakeLabel(200, 16, '', false, true);

  buttonGap := (ClientWidth - (3 * cButtonWidth)) div 4;
  MakeButton(buttonGap,                                  'Override and install', OnOverride);
  MakeButton(buttonGap * 2 + cButtonWidth,               'Cancel install',       OnCancelInstall);
  MakeButton(buttonGap * 3 + cButtonWidth * 2,           'Always block',         OnAlways);
end;

procedure TTrustPromptForm.OnOverride(Sender : TObject);
begin
  FResult := tprOverride;
  ModalResult := mrOK;
end;

procedure TTrustPromptForm.OnCancelInstall(Sender : TObject);
begin
  FResult := tprCancel;
  ModalResult := mrCancel;
end;

procedure TTrustPromptForm.OnAlways(Sender : TObject);
begin
  FResult := tprBlockAlways;
  ModalResult := mrAbort;
end;

class function TTrustPromptForm.Prompt(const heading, summary, consequence : string;
                                       const techPackage, techPrev, techNew : string;
                                       out decision : TTrustPromptResult) : boolean;
var
  form : TTrustPromptForm;
begin
  form := TTrustPromptForm.CreateNew(nil);
  try
    form.FHeading.Caption     := heading;
    form.FSummary.Caption     := summary;
    form.FConsequence.Caption := consequence;
    form.FTechPackage.Caption := techPackage;
    form.FTechPrev.Caption    := techPrev;
    form.FTechNew.Caption     := techNew;

    form.ShowModal;
    decision := form.FResult;
    result := decision = tprOverride;
  finally
    form.Free;
  end;
end;

end.
