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

unit DPM.IDE.SBOMOptionsForm;

{ The Generate SBOM dialog. It asks for the product information the SBOM's root needs (supplier,
  supplier contact, licence, SBOM creator, version) - prefilled from the project's <name>.sbom.yaml
  sidecar and saved back to it when the user asks - and for the output formats and folder.

  Built in code (no .dfm) like TTrustPromptForm, so it works across every supported IDE version
  without a designer round trip. The layout is measured from the font rather than fixed pixels, so
  it follows the IDE's font and DPI. }

interface

uses
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Winapi.Messages,
  DPM.Core.Options.SBOM,
  DPM.Core.SBOM.ProductInfo;

type
  ///<summary>What the dialog starts with and, after Generate, what the user chose.</summary>
  TSBOMDialogValues = record
    ///<summary>The sidecar's values going in, the dialog's (trimmed) values coming out.</summary>
    Info : TSBOMProductInfo;
    ///<summary>The project's version info layer - shown as what a blank field means, never edited.</summary>
    Fallback : TSBOMProductInfo;
    Formats : TSBOMFormats;
    OutputDir : string;
    SaveToSidecar : boolean;
  end;

  ///<summary>Word-wrapped text painted in a colour of our own. The IDE theme takes a TLabel's font
  /// colour away (see DPM.IDE.Types), so muted text is painted rather than set on a label.</summary>
  TSBOMNoteText = class(TGraphicControl)
  private
    FMuted : boolean;
    procedure SetMuted(const value : boolean);
    procedure CMTextChanged(var message : TMessage); message CM_TEXTCHANGED;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    property Caption;
    property Muted : boolean read FMuted write SetMuted;
  end;

  TSBOMOptionsForm = class(TForm)
  private
    FTargetPath : string;
    FSidecarName : string;
    FIsGroup : boolean;
    FFallback : TSBOMProductInfo;
    FMeasure : TBitmap;
    FSupplierEdit : TEdit;
    FSupplierUrlEdit : TEdit;
    FLicenseCombo : TComboBox;
    FCreatorEdit : TEdit;
    FVersionEdit : TEdit;
    FMissingText : TSBOMNoteText;
    FFormatChecks : array[TSBOMFormat] of TCheckBox;
    FOutputEdit : TEdit;
    FSaveCheck : TCheckBox;
    procedure BuildLayout;
    procedure LoadValues(const values : TSBOMDialogValues);
    function MeasureText(const text : string; const width : integer) : integer;
    function CurrentInfo : TSBOMProductInfo;
    function CurrentFormats : TSBOMFormats;
    function MissingInfoText(const gaps : TSBOMProductInfoGaps) : string;
    function SupplierNote : string;
    function VersionNote : string;
    function Refuse(const control : TWinControl; const reason : string) : boolean;
    function Validate : boolean;
    procedure UpdateMissing(Sender : TObject);
    procedure BrowseClick(Sender : TObject);
    procedure GenerateClick(Sender : TObject);
  public
    constructor CreateDialog(const targetPath : string; const values : TSBOMDialogValues);
    destructor Destroy; override;
    ///<summary>Shows the dialog for targetPath (a .dproj or .groupproj). False when the user cancels,
    /// with values untouched; true after Generate, with values holding what they chose.</summary>
    class function Execute(const targetPath : string; var values : TSBOMDialogValues) : boolean;
  end;

implementation

{$I DPMIDE.inc}

//TFileOpenDialog is Windows only, as is the IDE - see DPM.IDE.AddInOptionsFrame
{$WARN SYMBOL_PLATFORM OFF}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Math,
  System.UITypes,
  Vcl.Dialogs,
  Vcl.Themes,
  {$IFDEF THEMESERVICES}
  ToolsAPI,
  {$ENDIF}
  DPM.Core.Utils.Spdx,
  DPM.IDE.ToolsAPI,
  DPM.IDE.MessageForm;

const
  cFormatCaptions : array[TSBOMFormat] of string = ('CycloneDX JSON', 'SPDX JSON', 'HTML report', 'Markdown report');

function IDEStyleServices : TCustomStyleServices;
begin
  {$IFDEF THEMESERVICES}
  result := (BorlandIDEServices as IOTAIDEThemingServices).StyleServices;
  {$ELSE}
  result := StyleServices;
  {$ENDIF}
end;

//percent of foreground over background - a muted text colour that keeps its contrast in light and
//dark themes alike, where a fixed grey would not
function BlendColor(const foreground, background : TColor; const percent : integer) : TColor;
var
  fore : longint;
  back : longint;
begin
  fore := ColorToRGB(foreground);
  back := ColorToRGB(background);
  result := RGB((GetRValue(fore) * percent + GetRValue(back) * (100 - percent)) div 100,
                (GetGValue(fore) * percent + GetGValue(back) * (100 - percent)) div 100,
                (GetBValue(fore) * percent + GetBValue(back) * (100 - percent)) div 100);
end;

//'a', 'a and b', 'a, b and c'
function JoinWithAnd(const items : TArray<string>) : string;
var
  i : integer;
begin
  result := '';
  for i := 0 to Length(items) - 1 do
  begin
    if i > 0 then
    begin
      if i = Length(items) - 1 then
        result := result + ' and '
      else
        result := result + ', ';
    end;
    result := result + items[i];
  end;
end;

{ TSBOMNoteText }

constructor TSBOMNoteText.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  //transparent - the parent paints the themed background behind the text
  ControlStyle := ControlStyle - [csOpaque];
end;

procedure TSBOMNoteText.CMTextChanged(var message : TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSBOMNoteText.SetMuted(const value : boolean);
begin
  if FMuted <> value then
  begin
    FMuted := value;
    Invalidate;
  end;
end;

procedure TSBOMNoteText.Paint;
var
  services : TCustomStyleServices;
  textColor : TColor;
  r : TRect;
  text : string;
begin
  services := IDEStyleServices;
  textColor := services.GetSystemColor(clBtnText);
  if FMuted then
    textColor := BlendColor(textColor, services.GetSystemColor(clBtnFace), 60);
  Canvas.Font := Font;
  Canvas.Font.Color := textColor;
  Canvas.Brush.Style := bsClear;
  r := ClientRect;
  text := Caption;
  DrawText(Canvas.Handle, PChar(text), Length(text), r, DT_LEFT or DT_TOP or DT_WORDBREAK or DT_NOPREFIX);
end;

{ TSBOMOptionsForm }

constructor TSBOMOptionsForm.CreateDialog(const targetPath : string; const values : TSBOMDialogValues);
begin
  inherited CreateNew(nil);
  FTargetPath := targetPath;
  FSidecarName := ExtractFileName(SBOMSidecarFileName(targetPath));
  FIsGroup := SameText(ExtractFileExt(targetPath), '.groupproj');
  FFallback := values.Fallback;

  BorderStyle := bsDialog;
  Position := poMainFormCenter;
  Caption := 'Generate SBOM - ' + ExtractFileName(targetPath);
  //the IDE's own UI font, already sized for the monitor the IDE is on
  if Application.MainForm <> nil then
    Font.Assign(Application.MainForm.Font);

  //qualified - Winapi.Windows has a TBitmap record too
  FMeasure := Vcl.Graphics.TBitmap.Create;
  FMeasure.Canvas.Font.Assign(Font);
  BuildLayout;
  LoadValues(values);
  UpdateMissing(nil);

  TToolsApiUtils.RegisterFormClassForTheming(TSBOMOptionsForm, Self);
end;

destructor TSBOMOptionsForm.Destroy;
begin
  FMeasure.Free;
  inherited;
end;

function TSBOMOptionsForm.MeasureText(const text : string; const width : integer) : integer;
var
  r : TRect;
begin
  r := Rect(0, 0, width, 0);
  DrawText(FMeasure.Canvas.Handle, PChar(text), Length(text), r, DT_LEFT or DT_TOP or DT_WORDBREAK or DT_NOPREFIX or DT_CALCRECT);
  result := r.Bottom - r.Top;
end;

function TSBOMOptionsForm.SupplierNote : string;
begin
  result := 'The company or person that supplies the product.';
  if FFallback.Supplier <> '' then
    result := result + ' Leave blank to use CompanyName from the version info: ' + FFallback.Supplier + '.';
end;

function TSBOMOptionsForm.VersionNote : string;
begin
  if FIsGroup then
    result := 'The version of the group as a whole - each project''s version comes from its own version info.'
  else if FFallback.Version <> '' then
    result := 'Leave blank to use the project version info: ' + FFallback.Version + '. A version saved here does not ' +
              'change with each release.'
  else
    result := 'The project has no version info - enter the version here, or include version info in the project.';
end;

procedure TSBOMOptionsForm.BuildLayout;
var
  lh : integer;
  cw : integer;
  margin : integer;
  gap : integer;
  small : integer;
  pad : integer;
  editH : integer;
  checkH : integer;
  buttonH : integer;
  buttonW : integer;
  labelW : integer;
  fieldW : integer;
  groupW : integer;
  y : integer;
  rowY : integer;
  missingH : integer;
  productGroup : TGroupBox;
  outputGroup : TGroupBox;
  sbomFormat : TSBOMFormat;
  formatIndex : integer;
  browseButton : TButton;
  generateButton : TButton;
  cancelButton : TButton;
  formatsLabel : TLabel;

  function Scale(const value : integer) : integer;
  begin
    //the sizes below are for a 13 pixel line - 8pt Tahoma / 9pt Segoe UI at 96 DPI
    result := MulDiv(value, lh, 13);
  end;

  function TextW(const text : string) : integer;
  begin
    result := FMeasure.Canvas.TextWidth(StringReplace(text, '&', '', [rfReplaceAll]));
  end;

  procedure AddRow(const parent : TWinControl; const caption : string; const control : TWinControl; const note : string);
  var
    lbl : TLabel;
    noteText : TSBOMNoteText;
    noteH : integer;
  begin
    control.Parent := parent;
    control.SetBounds(pad + labelW, rowY, fieldW, editH);
    lbl := TLabel.Create(Self);
    lbl.Parent := parent;
    lbl.Caption := caption;
    lbl.FocusControl := control;
    lbl.SetBounds(pad, rowY + (editH - lh) div 2, labelW - gap, lh);
    rowY := rowY + editH + small;
    noteText := TSBOMNoteText.Create(Self);
    noteText.Parent := parent;
    noteText.Muted := true;
    noteText.Caption := note;
    noteH := MeasureText(note, fieldW);
    noteText.SetBounds(pad + labelW, rowY, fieldW, noteH);
    rowY := rowY + noteH + gap;
  end;

  function NewEdit : TEdit;
  begin
    result := TEdit.Create(Self);
    result.OnChange := UpdateMissing;
  end;

begin
  lh := FMeasure.Canvas.TextHeight('Wg');
  cw := Max(1, FMeasure.Canvas.TextWidth('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ') div 52);
  margin := Scale(12);
  gap := Scale(8);
  small := Max(2, Scale(3));
  pad := Scale(10);
  editH := lh + Scale(8);
  checkH := lh + Scale(4);
  buttonH := editH + Scale(4);
  labelW := Max(Max(TextW('Supplier contact'), TextW('SBOM creator')), TextW('Formats')) + gap;
  fieldW := cw * 56;
  groupW := pad + labelW + fieldW + pad;

  //Product information - created in visual order, which is the tab order
  productGroup := TGroupBox.Create(Self);
  productGroup.Parent := Self;
  productGroup.Caption := 'Product information';
  rowY := lh + gap;

  FSupplierEdit := NewEdit;
  AddRow(productGroup, '&Supplier', FSupplierEdit, SupplierNote);

  FSupplierUrlEdit := NewEdit;
  AddRow(productGroup, 'Supplier &contact', FSupplierUrlEdit, 'A URL or email address where the supplier can be reached.');

  FLicenseCombo := TComboBox.Create(Self);
  FLicenseCombo.Style := csDropDown;
  FLicenseCombo.DropDownCount := 16;
  FLicenseCombo.OnChange := UpdateMissing;
  AddRow(productGroup, '&Licence', FLicenseCombo, 'An SPDX id or expression, such as MIT or Apache-2.0, or a licence name.');
  TSpdxLicenses.GetLicenseIds(FLicenseCombo.Items);

  FCreatorEdit := NewEdit;
  AddRow(productGroup, 'SBOM c&reator', FCreatorEdit, 'Your email address or URL - who produced this SBOM.');

  FVersionEdit := NewEdit;
  AddRow(productGroup, '&Version', FVersionEdit, VersionNote);

  productGroup.SetBounds(margin, margin, groupW, rowY);
  y := margin + rowY + gap;

  //what is still missing - sized for the longest it can get, so the dialog doesn't jump as the user types
  FMissingText := TSBOMNoteText.Create(Self);
  FMissingText.Parent := Self;
  missingH := Max(MeasureText(MissingInfoText([TSBOMProductInfoGap.NoSupplier, TSBOMProductInfoGap.NoSbomCreator,
                                                          TSBOMProductInfoGap.NoVersion, TSBOMProductInfoGap.NoLicense]), groupW),
                  MeasureText(MissingInfoText([TSBOMProductInfoGap.NoSupplier, TSBOMProductInfoGap.NoSbomCreator,
                                                          TSBOMProductInfoGap.DefaultVersion, TSBOMProductInfoGap.NoLicense]), groupW));
  FMissingText.SetBounds(margin, y, groupW, missingH);
  y := y + missingH + gap;

  //Output
  outputGroup := TGroupBox.Create(Self);
  outputGroup.Parent := Self;
  outputGroup.Caption := 'Output';
  rowY := lh + gap;

  formatIndex := 0;
  for sbomFormat := Low(TSBOMFormat) to High(TSBOMFormat) do
  begin
    FFormatChecks[sbomFormat] := TCheckBox.Create(Self);
    FFormatChecks[sbomFormat].Parent := outputGroup;
    FFormatChecks[sbomFormat].Caption := cFormatCaptions[sbomFormat];
    //two columns of two
    FFormatChecks[sbomFormat].SetBounds(pad + labelW + (formatIndex mod 2) * (fieldW div 2),
                                    rowY + (formatIndex div 2) * (checkH + small),
                                    fieldW div 2 - gap, checkH);
    Inc(formatIndex);
  end;
  formatsLabel := TLabel.Create(Self);
  formatsLabel.Parent := outputGroup;
  formatsLabel.Caption := 'Formats';
  formatsLabel.SetBounds(pad, rowY + (checkH - lh) div 2, labelW - gap, lh);
  rowY := rowY + 2 * checkH + small + gap;

  FOutputEdit := TEdit.Create(Self);
  AddRow(outputGroup, '&Folder', FOutputEdit, 'Where the SBOM files are written.');
  FOutputEdit.Width := fieldW - editH - small;
  browseButton := TButton.Create(Self);
  browseButton.Parent := outputGroup;
  browseButton.Caption := '...';
  browseButton.Hint := 'Choose the folder';
  browseButton.ShowHint := true;
  browseButton.SetBounds(FOutputEdit.Left + FOutputEdit.Width + small, FOutputEdit.Top, editH, editH);
  browseButton.OnClick := BrowseClick;

  outputGroup.SetBounds(margin, y, groupW, rowY);
  y := y + rowY + gap;

  FSaveCheck := TCheckBox.Create(Self);
  FSaveCheck.Parent := Self;
  FSaveCheck.Caption := 'Save the product information to ' + FSidecarName;
  FSaveCheck.SetBounds(margin, y, groupW, checkH);
  y := y + checkH + gap;

  buttonW := Max(Scale(75), Max(TextW('Generate'), TextW('Cancel')) + 4 * cw);
  generateButton := TButton.Create(Self);
  generateButton.Parent := Self;
  generateButton.Caption := 'Generate';
  generateButton.Default := true;
  generateButton.OnClick := GenerateClick;
  generateButton.SetBounds(margin + groupW - 2 * buttonW - gap, y, buttonW, buttonH);

  cancelButton := TButton.Create(Self);
  cancelButton.Parent := Self;
  cancelButton.Caption := 'Cancel';
  cancelButton.Cancel := true;
  cancelButton.ModalResult := mrCancel;
  cancelButton.SetBounds(margin + groupW - buttonW, y, buttonW, buttonH);

  ClientWidth := margin + groupW + margin;
  ClientHeight := y + buttonH + margin;
  ActiveControl := FSupplierEdit;
end;

procedure TSBOMOptionsForm.LoadValues(const values : TSBOMDialogValues);
var
  sbomFormat : TSBOMFormat;
begin
  FSupplierEdit.Text := values.Info.Supplier;
  FSupplierUrlEdit.Text := values.Info.SupplierUrl;
  FLicenseCombo.Text := values.Info.License;
  FCreatorEdit.Text := values.Info.SbomCreator;
  FVersionEdit.Text := values.Info.Version;
  //what a blank field falls back to, in the field itself
  FSupplierEdit.TextHint := FFallback.Supplier;
  FVersionEdit.TextHint := FFallback.Version;

  for sbomFormat := Low(TSBOMFormat) to High(TSBOMFormat) do
    FFormatChecks[sbomFormat].Checked := sbomFormat in values.Formats;
  FOutputEdit.Text := values.OutputDir;
  FSaveCheck.Checked := values.SaveToSidecar;
end;

function TSBOMOptionsForm.CurrentInfo : TSBOMProductInfo;
begin
  result.Supplier := Trim(FSupplierEdit.Text);
  result.SupplierUrl := Trim(FSupplierUrlEdit.Text);
  result.License := Trim(FLicenseCombo.Text);
  result.SbomCreator := Trim(FCreatorEdit.Text);
  result.Version := Trim(FVersionEdit.Text);
end;

function TSBOMOptionsForm.CurrentFormats : TSBOMFormats;
var
  sbomFormat : TSBOMFormat;
begin
  result := [];
  for sbomFormat := Low(TSBOMFormat) to High(TSBOMFormat) do
    if FFormatChecks[sbomFormat].Checked then
      Include(result, sbomFormat);
end;

//The dialog's wording of GetProductInfoGaps - the CLI's warnings name sidecar keys and switches,
//which mean nothing here.
function TSBOMOptionsForm.MissingInfoText(const gaps : TSBOMProductInfoGaps) : string;
var
  missing : TArray<string>;

  procedure Add(const text : string);
  begin
    SetLength(missing, Length(missing) + 1);
    missing[Length(missing) - 1] := text;
  end;

begin
  missing := nil;
  if TSBOMProductInfoGap.NoSupplier in gaps then
    Add('a supplier');
  if TSBOMProductInfoGap.NoSupplierContact in gaps then
    Add('a contact for the supplier');
  if TSBOMProductInfoGap.NoSbomCreator in gaps then
    Add('who created the SBOM');
  if TSBOMProductInfoGap.NoVersion in gaps then
    Add('a version');
  if TSBOMProductInfoGap.NoLicense in gaps then
    Add('a licence');

  if gaps = [] then
  begin
    result := 'Nothing is missing - the SBOM has all the product information the CRA and BSI TR-03183-2 ask for.';
    exit;
  end;

  result := '';
  if Length(missing) > 0 then
    result := 'Still missing: ' + JoinWithAnd(missing) + '. The CRA and BSI TR-03183-2 ask for these - the SBOM is ' +
              'generated either way, with a warning for each.';
  if TSBOMProductInfoGap.DefaultVersion in gaps then
  begin
    if result <> '' then
      result := result + ' ';
    result := result + 'The version 1.0.0.0 is the default for a new Delphi project - if it is not the real release, ' +
              'enter the version or update the project version info.';
  end;
end;

procedure TSBOMOptionsForm.UpdateMissing(Sender : TObject);
var
  info : TSBOMProductInfo;
  gaps : TSBOMProductInfoGaps;
  versionIsExplicit : boolean;
begin
  if FMissingText = nil then
    exit;
  //what the generator will end up with - the dialog's values over the version info
  info := CurrentInfo;
  info := info.Merge(FFallback);
  //a group's version can only come from here, so it is explicit whatever it is
  versionIsExplicit := FIsGroup or (Trim(FVersionEdit.Text) <> '');
  gaps := GetProductInfoGaps(info, versionIsExplicit);
  FMissingText.Caption := MissingInfoText(gaps);
  FMissingText.Muted := gaps = [];
end;

procedure TSBOMOptionsForm.BrowseClick(Sender : TObject);
var
  dialog : TFileOpenDialog;
begin
  dialog := TFileOpenDialog.Create(Self);
  try
    dialog.Title := 'Choose the folder to write the SBOM to';
    dialog.Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem];
    if DirectoryExists(Trim(FOutputEdit.Text)) then
      dialog.DefaultFolder := Trim(FOutputEdit.Text)
    else
      dialog.DefaultFolder := ExtractFileDir(FTargetPath);
    if dialog.Execute(Handle) then
      FOutputEdit.Text := dialog.FileName;
  finally
    dialog.Free;
  end;
end;

function TSBOMOptionsForm.Refuse(const control : TWinControl; const reason : string) : boolean;
begin
  MessageDlg(reason, mtWarning, [mbOK], 0);
  if control.CanFocus then
    control.SetFocus;
  result := false;
end;

function TSBOMOptionsForm.Validate : boolean;
var
  info : TSBOMProductInfo;
begin
  info := CurrentInfo;
  //BSI TR-03183-2 5.2.1 - the generator refuses anything else (DPME8101), so say so while it can be fixed
  if (info.SbomCreator <> '') and not (IsEmailAddress(info.SbomCreator) or IsHttpUrl(info.SbomCreator)) then
    exit(Refuse(FCreatorEdit, 'The SBOM creator must be an email address or an http(s) URL - [' + info.SbomCreator + '] is neither.'));
  if (info.SupplierUrl <> '') and not (IsEmailAddress(info.SupplierUrl) or IsHttpUrl(info.SupplierUrl)) then
    exit(Refuse(FSupplierUrlEdit, 'The supplier contact must be an email address or an http(s) URL - [' + info.SupplierUrl + '] is neither.'));
  if CurrentFormats = [] then
    exit(Refuse(FFormatChecks[TSBOMFormat.CycloneDX], 'Choose at least one format to generate.'));
  if Trim(FOutputEdit.Text) = '' then
    exit(Refuse(FOutputEdit, 'Choose the folder to write the SBOM to.'));
  result := true;
end;

procedure TSBOMOptionsForm.GenerateClick(Sender : TObject);
begin
  if Validate then
    ModalResult := mrOk;
end;

class function TSBOMOptionsForm.Execute(const targetPath : string; var values : TSBOMDialogValues) : boolean;
var
  form : TSBOMOptionsForm;
begin
  result := false;
  form := TSBOMOptionsForm.CreateDialog(targetPath, values);
  try
    //owned by the IDE for this one modal session - see DPM.IDE.TrustPromptForm
    if Application.MainForm <> nil then
    begin
      form.PopupMode := pmExplicit;
      form.PopupParent := Application.MainForm;
    end;
    //the log window is topmost while it is up - it must not cover the dialog
    TDPMMessageForm.BeginModalPrompt;
    try
      if form.ShowModal <> mrOk then
        exit;
    finally
      TDPMMessageForm.EndModalPrompt;
    end;
    values.Info := form.CurrentInfo;
    values.Formats := form.CurrentFormats;
    values.OutputDir := Trim(form.FOutputEdit.Text);
    values.SaveToSidecar := form.FSaveCheck.Checked;
    result := true;
  finally
    form.Free;
  end;
end;

end.
