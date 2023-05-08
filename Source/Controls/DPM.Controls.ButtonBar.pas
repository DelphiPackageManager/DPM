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

//TODO : use inc file when we move this under \IDE
{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  {$DEFINE STYLEELEMENTS}
{$IFEND}

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

    FOnTabChanged : TDPMTabChangedEvent;
  protected
    procedure SetShowConflictsTab(const Value: boolean);
    function GetShowConflictsButton: boolean;


    procedure DoTabChanged(Sender : TObject);
    procedure LayoutButtons;
    procedure ChangeScale(M: Integer; D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend}); override;

  public
    constructor Create(AOwner : TComponent);override;
    procedure ThemeChanged(const ideStyleServices : TCustomStyleServices);
    property ShowConflictsTab : boolean read GetShowConflictsButton write SetShowConflictsTab;
    {$IF CompilerVersion >= 24.0}
      {$LEGACYIFEND ON}
    property StyleElements;
    {$IFEND}
    procedure SetCurrentTab(const tab : TDPMCurrentTab);


    property OnTabChanged : TDPMTabChangedEvent read FOnTabChanged write FOnTabChanged;


  end;

implementation

uses
  ToolsAPI,
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
  //not published in older versions, so get removed when we edit in older versions.
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont, seClient, seBorder];
  {$ENDIF}

  Align := alTop;
  Height := 34;
  BorderStyle := bsNone;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FButtonHeight := 24;

//  ParentBackground := false;
//  ParentColor := false;


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

procedure TDPMButtonBar.SetCurrentTab(const tab: TDPMCurrentTab);
begin
  case tab of
    TDPMCurrentTab.Search   : FSearchButton.Active := true;
    TDPMCurrentTab.Installed: FInstalledButton.Active := true ;
    TDPMCurrentTab.Updates  : FUpdatesButton.Active := true;
    TDPMCurrentTab.Conflicts: FConflictsButton.Active := true;
  end;
end;

procedure TDPMButtonBar.SetShowConflictsTab(const Value: boolean);
begin
  FConflictsButton.Visible := Value;
end;

procedure TDPMButtonBar.ThemeChanged(const ideStyleServices : TCustomStyleServices);
begin
  Self.Color := ideStyleServices.GetSystemColor(clBtnFace);
  Self.Font.Color := ideStyleServices.GetSystemColor(clWindowText);
//
  FSearchButton.Font.Color := ideStyleServices.GetSystemColor(clWindowText);
  FInstalledButton.Font.Color := ideStyleServices.GetSystemColor(clWindowText);
  FUpdatesButton.Font.Color := ideStyleServices.GetSystemColor(clWindowText);
  FConflictsButton.Font.Color := ideStyleServices.GetSystemColor(clWindowText);

end;

end.
