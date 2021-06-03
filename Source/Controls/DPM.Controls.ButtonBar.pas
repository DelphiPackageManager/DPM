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

unit DPM.Controls.ButtonBar;

interface

uses
  System.Classes,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Themes,
  DPM.Controls.GroupButton,
  DPM.IDE.Types;

type
  TDPMTabChangedEvent = procedure(const tab : TDPMCurrentTab) of object;

  TDPMButtonBar = class(TPanel)
  private
    FButtonHeight    : integer;
    FInstalledButton : TDPMGroupButton;
    FUpdatesButton   : TDPMGroupButton;
    FSearchButton    : TDPMGroupButton;
    FConflictsButton : TDPMGroupButton;

    FIDEStyleServices : TCustomStyleServices;
    FOnTabChanged : TDPMTabChangedEvent;
  protected
    procedure SetShowConflictsTab(const Value: boolean);
    function GetShowConflictsButton: boolean;

    procedure DoTabChanged(Sender : TObject);
    procedure LayoutButtons;
    procedure ChangeScale(M: Integer; D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend}); override;

  public
    constructor Create(AOwner : TComponent);override;
    procedure ThemeChanged;
    property ShowConflictsTab : boolean read GetShowConflictsButton write SetShowConflictsTab;
    property OnTabChanged : TDPMTabChangedEvent read FOnTabChanged write FOnTabChanged;
  end;

implementation

uses
  WinApi.Windows,
  Vcl.Graphics,
  Vcl.Forms;

{ TDPMButtonBar }

procedure TDPMButtonBar.ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend});
begin
  FButtonHeight := Muldiv(FButtonHeight, M, D);
  LayoutButtons;
  inherited;
end;

constructor TDPMButtonBar.Create(AOwner: TComponent);
begin
  inherited;
  Align := alTop;
  Height := 34;
  BorderStyle := bsNone;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FButtonHeight := 24;

  FInstalledButton := TDPMGroupButton.Create(Self);
  FUpdatesButton := TDPMGroupButton.Create(Self);
  FSearchButton := TDPMGroupButton.Create(Self);
  FConflictsButton := TDPMGroupButton.Create(Self);

  FInstalledButton.OnClick := DoTabChanged;
  FUpdatesButton.OnClick := DoTabChanged;
  FSearchButton.OnClick := DoTabChanged;
  FConflictsButton.OnClick := DoTabChanged;

  LayoutButtons;
end;

procedure TDPMButtonBar.DoTabChanged(Sender: TObject);
var
  tab : TDPMCurrentTab;
begin
  if Assigned(FOnTabChanged) then
  begin
    tab := TDPMCurrentTab(TDPMGroupButton(Sender).Tag);
    FOnTabChanged(tab);
  end;
end;

function TDPMButtonBar.GetShowConflictsButton: boolean;
begin
  result := FConflictsButton.Visible;
end;

procedure TDPMButtonBar.LayoutButtons;
begin
  FSearchButton.Top := 10;
  FSearchButton.Left := 20;
  FSearchButton.Height := FButtonHeight;
  FSearchButton.Caption := 'Search';
  FSearchButton.Tag := Ord(TDPMCurrentTab.Search);
  FSearchButton.Parent := Self;

  FInstalledButton.Top := 10;
  FInstalledButton.Left := FSearchButton.Left + FSearchButton.Width + 20;
  FInstalledButton.Height := FButtonHeight;
  FInstalledButton.Caption := 'Installed';
  FInstalledButton.Tag := Ord(TDPMCurrentTab.Installed);
  FInstalledButton.Active := true;
  FInstalledButton.Parent := Self;

  FUpdatesButton.Top := 10;
  FUpdatesButton.Left := FInstalledButton.Left + FInstalledButton.Width + 20;
  FUpdatesButton.Height := FButtonHeight;
  FUpdatesButton.Caption := 'Updates';
  FUpdatesButton.Tag := Ord(TDPMCurrentTab.Updates);
  FUpdatesButton.Parent := Self;

  FConflictsButton.Top := 10;
  FConflictsButton.Left := FUpdatesButton.Left + FUpdatesButton.Width + 20;
  FConflictsButton.Height := FButtonHeight;
  FConflictsButton.Caption := 'Conflicts';
  FConflictsButton.Tag := Ord(TDPMCurrentTab.Conflicts);
  FConflictsButton.Visible := false;
  FConflictsButton.Parent := Self;

end;

procedure TDPMButtonBar.SetShowConflictsTab(const Value: boolean);
begin
  FConflictsButton.Visible := Value;
end;

procedure TDPMButtonBar.ThemeChanged;
{$IF CompilerVersion >=32.0}
var
  ideThemeSvc : IOTAIDEThemingServices;
  {$IFEND}
begin
  {$IF CompilerVersion >=32.0}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  ideThemeSvc.ApplyTheme(Self);
  FIDEStyleServices := ideThemeSvc.StyleServices;
  ideThemeSvc.ApplyTheme(Self);
  {$ELSE}
  FIDEStyleServices := Vcl.Themes.StyleServices;
  {$IFEND}

  Self.Color := FIDEStyleServices.GetSystemColor(clBtnFace);
  Self.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);

  FSearchButton.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  FInstalledButton.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  FUpdatesButton.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  FConflictsButton.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);

end;

end.
