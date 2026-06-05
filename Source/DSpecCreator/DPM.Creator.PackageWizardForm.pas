unit DPM.Creator.PackageWizardForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Console.Command.Spec.Discovery,
  DPM.Console.Command.Spec.Scaffold;

type
  TPackageWizardForm = class(TForm)
    PageControl : TPageControl;
    tsRoot : TTabSheet;
    tsFolders : TTabSheet;
    tsMetadata : TTabSheet;
    tsMultiPackage : TTabSheet;
    tsReview : TTabSheet;
    pnlButtons : TPanel;
    btnBack : TButton;
    btnNext : TButton;
    btnFinish : TButton;
    btnCancel : TButton;
    lblRootIntro : TLabel;
    lblRoot : TLabel;
    edtRootFolder : TEdit;
    btnBrowseRoot : TButton;
    lblSourceFolder : TLabel;
    edtSourceFolder : TEdit;
    btnBrowseSource : TButton;
    chkHasPackages : TCheckBox;
    lblPackagesFolder : TLabel;
    edtPackagesFolder : TEdit;
    btnBrowsePackages : TButton;
    lblId : TLabel;
    edtId : TEdit;
    lblDescription : TLabel;
    edtDescription : TEdit;
    lblAuthor : TLabel;
    edtAuthor : TEdit;
    lblVersion : TLabel;
    edtVersion : TEdit;
    lblLicense : TLabel;
    edtLicense : TEdit;
    rgMultiMode : TRadioGroup;
    lblWhich : TLabel;
    cboWhichPackage : TComboBox;
    lblReview : TLabel;
    mmoReview : TMemo;
    procedure FormCreate(Sender : TObject);
    procedure btnBrowseRootClick(Sender : TObject);
    procedure btnBrowseSourceClick(Sender : TObject);
    procedure btnBrowsePackagesClick(Sender : TObject);
    procedure chkHasPackagesClick(Sender : TObject);
    procedure rgMultiModeClick(Sender : TObject);
    procedure btnBackClick(Sender : TObject);
    procedure btnNextClick(Sender : TObject);
    procedure btnFinishClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
  private
    FLogger : ILogger;
    FConfigManager : IConfigurationManager;
    FConfig : IConfiguration;
    FCtx : TScaffoldContext;
    FLogicalPackages : TLogicalPackages;
    FDefaultId : string;
    FResultFile : string;
    function IsMultiPackage : boolean;
    function NextVisiblePage(const fromIdx : integer) : integer;
    function PrevVisiblePage(const fromIdx : integer) : integer;
    procedure UpdateButtons;
    procedure RunScan;
    procedure BuildReviewText;
    function LeaveRoot(out msg : string) : boolean;
    function LeaveFolders(out msg : string) : boolean;
    function LeaveMetadata(out msg : string) : boolean;
    function LeaveMultiPackage(out msg : string) : boolean;
    function DoFinish : boolean;
  public
    constructor Create(AOwner : TComponent; const logger : ILogger;
      const configManager : IConfigurationManager); reintroduce;
    property ResultFile : string read FResultFile;
  end;

implementation

{$R *.dfm}

uses
  System.UITypes,
  System.IOUtils,
  System.StrUtils,
  Vcl.FileCtrl,
  DPM.Core.Types,
  DPM.Core.Constants,
  DPM.Core.Utils.Config,
  DPM.Console.Command.Spec.Writer;

const
  cPageRoot = 0;
  cPageFolders = 1;
  cPageMetadata = 2;
  cPageMultiPackage = 3;
  cPageReview = 4;

constructor TPackageWizardForm.Create(AOwner : TComponent; const logger : ILogger;
  const configManager : IConfigurationManager);
begin
  inherited Create(AOwner);
  FLogger := logger;
  FConfigManager := configManager;
  FResultFile := '';
end;

procedure TPackageWizardForm.FormCreate(Sender : TObject);
var
  i : integer;
begin
  //hide the tabs - navigation is driven entirely by the Back/Next buttons.
  for i := 0 to PageControl.PageCount - 1 do
    PageControl.Pages[i].TabVisible := false;
  PageControl.ActivePageIndex := cPageRoot;
  edtRootFolder.Text := ExcludeTrailingPathDelimiter(GetCurrentDir);
  UpdateButtons;
end;

function TPackageWizardForm.IsMultiPackage : boolean;
begin
  result := Length(FLogicalPackages) > 1;
end;

function TPackageWizardForm.NextVisiblePage(const fromIdx : integer) : integer;
begin
  result := fromIdx + 1;
  if (result = cPageMultiPackage) and (not IsMultiPackage) then
    result := cPageReview;
  if result > cPageReview then
    result := cPageReview;
end;

function TPackageWizardForm.PrevVisiblePage(const fromIdx : integer) : integer;
begin
  result := fromIdx - 1;
  if (result = cPageMultiPackage) and (not IsMultiPackage) then
    result := cPageMetadata;
  if result < cPageRoot then
    result := cPageRoot;
end;

procedure TPackageWizardForm.UpdateButtons;
var
  isLast : boolean;
begin
  isLast := PageControl.ActivePageIndex = cPageReview;
  btnBack.Enabled := PageControl.ActivePageIndex > cPageRoot;
  btnNext.Visible := not isLast;
  btnFinish.Visible := isLast;
  btnNext.Default := not isLast;
  btnFinish.Default := isLast;
end;

procedure TPackageWizardForm.btnBrowseRootClick(Sender : TObject);
var
  dir : string;
begin
  dir := edtRootFolder.Text;
  if SelectDirectory('Select the project root folder', '', dir) then
    edtRootFolder.Text := dir;
end;

procedure TPackageWizardForm.btnBrowseSourceClick(Sender : TObject);
var
  dir : string;
begin
  dir := edtSourceFolder.Text;
  if SelectDirectory('Select the source folder', '', dir) then
    edtSourceFolder.Text := dir;
end;

procedure TPackageWizardForm.btnBrowsePackagesClick(Sender : TObject);
var
  dir : string;
begin
  dir := edtPackagesFolder.Text;
  if SelectDirectory('Select the packages folder', '', dir) then
    edtPackagesFolder.Text := dir;
end;

procedure TPackageWizardForm.chkHasPackagesClick(Sender : TObject);
begin
  lblPackagesFolder.Enabled := chkHasPackages.Checked;
  edtPackagesFolder.Enabled := chkHasPackages.Checked;
  btnBrowsePackages.Enabled := chkHasPackages.Checked;
end;

procedure TPackageWizardForm.rgMultiModeClick(Sender : TObject);
begin
  //index 1 = "pick a single package"
  cboWhichPackage.Enabled := rgMultiMode.ItemIndex = 1;
  lblWhich.Enabled := rgMultiMode.ItemIndex = 1;
end;

function TPackageWizardForm.LeaveRoot(out msg : string) : boolean;
var
  existingSpec : TArray<string>;
  srcFolder : string;
  pkgFolder : string;
begin
  result := false;
  msg := '';
  FCtx.RootDir := ExcludeTrailingPathDelimiter(Trim(edtRootFolder.Text));
  if (FCtx.RootDir = '') or not TDirectory.Exists(FCtx.RootDir) then
  begin
    msg := 'Please choose an existing folder.';
    exit;
  end;

  //overwrite guard - mirrors step 1 of the spec command.
  existingSpec := TDirectory.GetFiles(FCtx.RootDir, '*' + cPackageSpecExt);
  if Length(existingSpec) > 0 then
  begin
    if MessageDlg('A ' + cPackageSpecExt + ' already exists in this folder. Overwrite?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      exit; //blocked, but no error message
  end;

  //pre-fill the folders page from discovery (only when the fields are empty so
  //the user's edits survive navigating back and forth).
  if Trim(edtSourceFolder.Text) = '' then
    if FindSourceFolder(FCtx.RootDir, srcFolder) then
      edtSourceFolder.Text := srcFolder;
  if (not chkHasPackages.Checked) and (Trim(edtPackagesFolder.Text) = '') then
    if FindPackagesFolder(FCtx.RootDir, pkgFolder) then
    begin
      chkHasPackages.Checked := true;
      edtPackagesFolder.Text := pkgFolder;
    end;

  result := true;
end;

function TPackageWizardForm.LeaveFolders(out msg : string) : boolean;
var
  src : string;
  pkg : string;
begin
  result := false;
  msg := '';
  src := Trim(edtSourceFolder.Text);
  if src = '' then
  begin
    msg := 'A source folder is required.';
    exit;
  end;
  if not TPath.IsPathRooted(src) then
    src := TPath.Combine(FCtx.RootDir, src);
  if not TDirectory.Exists(src) then
  begin
    msg := 'The source folder does not exist.';
    exit;
  end;
  FCtx.SourceFolder := src;
  FCtx.SourceRel := StringReplace(MakeRelative(FCtx.RootDir, src), './', '', []);

  if chkHasPackages.Checked then
  begin
    pkg := Trim(edtPackagesFolder.Text);
    if pkg = '' then
    begin
      msg := 'A packages folder is required (or untick the checkbox).';
      exit;
    end;
    if not TPath.IsPathRooted(pkg) then
      pkg := TPath.Combine(FCtx.RootDir, pkg);
    if not TDirectory.Exists(pkg) then
    begin
      msg := 'The packages folder does not exist.';
      exit;
    end;
    FCtx.PackagesFolder := pkg;
    FCtx.HasPackages := true;
    FCtx.PackagesRel := StringReplace(MakeRelative(FCtx.RootDir, pkg), './', '', []);
  end
  else
  begin
    FCtx.PackagesFolder := '';
    FCtx.HasPackages := false;
    FCtx.PackagesRel := '';
  end;

  RunScan;
  result := true;
end;

procedure TPackageWizardForm.RunScan;
var
  i : integer;
  primaryIdx : integer;
  gitUrl : string;
  projectUrl : string;
  configFile : string;
begin
  //LoadConfig('') fails (an empty path never resolves to the default config), so
  //ensure the default config exists and load it by its real path.
  FConfigManager.EnsureDefaultConfig;
  configFile := TConfigUtils.GetDefaultConfigFileName;
  FConfig := FConfigManager.LoadConfig(configFile);
  FCtx.Config := FConfig;

  //scan packages for compiler folders
  if FCtx.HasPackages then
  begin
    FCtx.CompilerFolders := ScanPackagesFolder(FCtx.PackagesFolder, FLogger, FConfig);
    if Length(FCtx.CompilerFolders) = 0 then
    begin
      FLogger.Warning('No recognisable compiler folders found under ' + FCtx.PackagesFolder +
        ' - skipping build/design.');
      FCtx.HasPackages := false;
    end;
  end;

  //derive defaults from git
  gitUrl := '';
  projectUrl := '';
  FDefaultId := '';
  if DetectGitRemoteUrl(FCtx.RootDir, gitUrl) then
  begin
    projectUrl := NormaliseGitUrl(gitUrl);
    FDefaultId := DerivePackageIdFromUrl(projectUrl);
  end;
  FCtx.ProjectUrl := projectUrl;

  //detect logical packages from the primary compiler folder (if any)
  SetLength(FLogicalPackages, 0);
  if FCtx.HasPackages then
  begin
    primaryIdx := ChoosePrimaryFolder(FCtx.CompilerFolders);
    FLogicalPackages := GroupDProjsByStem(FCtx.CompilerFolders[primaryIdx].DProjFiles);
  end;
  FCtx.LogicalPackages := FLogicalPackages;
  SetLength(FCtx.PackageIds, Length(FLogicalPackages));
  for i := 0 to High(FLogicalPackages) do
    FCtx.PackageIds[i] := FLogicalPackages[i].Stem;

  //pre-fill metadata defaults (only when empty)
  if Trim(edtId.Text) = '' then
    edtId.Text := FDefaultId;
  if (Trim(edtAuthor.Text) = '') and (FConfig <> nil) then
    edtAuthor.Text := FConfig.Author;
  if Trim(edtVersion.Text) = '' then
    edtVersion.Text := '0.1.0';
  if Trim(edtLicense.Text) = '' then
    edtLicense.Text := 'Apache-2.0';

  //in single-package mode the id is editable; in multi-package mode each package
  //is scaffolded under its stem, so hide/disable the id field.
  edtId.Enabled := not IsMultiPackage;
  lblId.Enabled := not IsMultiPackage;

  //populate the multi-package choices
  rgMultiMode.Items.Clear;
  rgMultiMode.Items.Add(Format('Scaffold one %s per package (%d files)',
    [cPackageSpecExt, Length(FLogicalPackages)]));
  rgMultiMode.Items.Add('Scaffold a single ' + cPackageSpecExt + ' (pick which package)');
  rgMultiMode.ItemIndex := 0;
  cboWhichPackage.Items.Clear;
  for i := 0 to High(FLogicalPackages) do
    cboWhichPackage.Items.Add(FLogicalPackages[i].Stem);
  if cboWhichPackage.Items.Count > 0 then
    cboWhichPackage.ItemIndex := 0;
  rgMultiModeClick(nil);
end;

function TPackageWizardForm.LeaveMetadata(out msg : string) : boolean;
var
  author : string;
  ver : string;
  lic : string;
begin
  result := false;
  msg := '';
  if Trim(edtDescription.Text) = '' then
  begin
    msg := 'A description is required.';
    exit;
  end;
  if (not IsMultiPackage) and (Trim(edtId.Text) = '') then
  begin
    msg := 'A package id is required.';
    exit;
  end;

  FCtx.Description := Trim(edtDescription.Text);
  author := Trim(edtAuthor.Text);
  FCtx.Author := author;
  if author <> '' then
    FCtx.Copyright := author + ' and contributors'
  else
    FCtx.Copyright := '';

  ver := Trim(edtVersion.Text);
  if ver = '' then
    ver := '0.1.0';
  FCtx.Version := ver;

  lic := Trim(edtLicense.Text);
  if lic = '' then
    lic := 'Apache-2.0';
  FCtx.License := lic;

  //persist the author back to config so subsequent specs default to it (mirrors
  //step 8 of the spec command).
  if (author <> '') and (FConfig <> nil) and (not SameText(author, FConfig.Author)) then
  begin
    FConfig.Author := author;
    if not FConfigManager.SaveConfig(FConfig, FConfig.FileName) then
      FLogger.Warning('Could not save author to ' + FConfig.FileName);
  end;

  result := true;
end;

function TPackageWizardForm.LeaveMultiPackage(out msg : string) : boolean;
begin
  result := false;
  msg := '';
  if (rgMultiMode.ItemIndex = 1) and (cboWhichPackage.ItemIndex < 0) then
  begin
    msg := 'Please select a package.';
    exit;
  end;
  result := true;
end;

procedure TPackageWizardForm.BuildReviewText;
var
  i : integer;
begin
  mmoReview.Lines.BeginUpdate;
  try
    mmoReview.Lines.Clear;
    mmoReview.Lines.Add('Root folder : ' + FCtx.RootDir);
    mmoReview.Lines.Add('Source      : ' + FCtx.SourceRel);
    if FCtx.HasPackages then
      mmoReview.Lines.Add('Packages    : ' + FCtx.PackagesRel)
    else
      mmoReview.Lines.Add('Packages    : (none)');
    mmoReview.Lines.Add('');

    if not IsMultiPackage then
      mmoReview.Lines.Add('Will write : ' + Trim(edtId.Text) + cPackageSpecExt)
    else if rgMultiMode.ItemIndex = 1 then
      mmoReview.Lines.Add('Will write : ' + FLogicalPackages[cboWhichPackage.ItemIndex].Stem + cPackageSpecExt)
    else
    begin
      mmoReview.Lines.Add(Format('Will write %d files:', [Length(FLogicalPackages)]));
      for i := 0 to High(FLogicalPackages) do
        mmoReview.Lines.Add('  ' + FLogicalPackages[i].Stem + cPackageSpecExt);
    end;
    mmoReview.Lines.Add('');
    mmoReview.Lines.Add('Review the generated file(s) and fill in any remaining values.');
  finally
    mmoReview.Lines.EndUpdate;
  end;
end;

procedure TPackageWizardForm.btnBackClick(Sender : TObject);
begin
  PageControl.ActivePageIndex := PrevVisiblePage(PageControl.ActivePageIndex);
  UpdateButtons;
end;

procedure TPackageWizardForm.btnNextClick(Sender : TObject);
var
  msg : string;
  ok : boolean;
  nextPage : integer;
begin
  msg := '';
  case PageControl.ActivePageIndex of
    cPageRoot : ok := LeaveRoot(msg);
    cPageFolders : ok := LeaveFolders(msg);
    cPageMetadata : ok := LeaveMetadata(msg);
    cPageMultiPackage : ok := LeaveMultiPackage(msg);
  else
    ok := true;
  end;

  if not ok then
  begin
    if msg <> '' then
      MessageDlg(msg, mtWarning, [mbOK], 0);
    exit;
  end;

  nextPage := NextVisiblePage(PageControl.ActivePageIndex);
  PageControl.ActivePageIndex := nextPage;
  if nextPage = cPageReview then
    BuildReviewText;
  UpdateButtons;
end;

function TPackageWizardForm.DoFinish : boolean;
var
  scaffold : TSpecScaffold;
  idx : integer;
  effectiveId : string;
  i : integer;
  writtenPath : string;
  firstPath : string;
begin
  result := false;
  firstPath := '';

  if not IsMultiPackage then
  begin
    if Length(FLogicalPackages) = 1 then
    begin
      FCtx.PackageIds[0] := Trim(edtId.Text);
      scaffold := BuildPackageScaffold(FLogicalPackages[0], FCtx, Trim(edtId.Text), FLogger);
    end
    else
      scaffold := BuildPackageScaffold(Default(TLogicalPackage), FCtx, Trim(edtId.Text), FLogger);
    if not WriteScaffoldFile(scaffold, FCtx.RootDir, FLogger, writtenPath) then
    begin
      MessageDlg('Failed to write ' + writtenPath, mtError, [mbOK], 0);
      exit;
    end;
    FResultFile := writtenPath;
    result := true;
    exit;
  end;

  if rgMultiMode.ItemIndex = 1 then
  begin
    idx := cboWhichPackage.ItemIndex;
    effectiveId := FLogicalPackages[idx].Stem;
    FCtx.PackageIds[idx] := effectiveId;
    scaffold := BuildPackageScaffold(FLogicalPackages[idx], FCtx, effectiveId, FLogger);
    if not WriteScaffoldFile(scaffold, FCtx.RootDir, FLogger, writtenPath) then
    begin
      MessageDlg('Failed to write ' + writtenPath, mtError, [mbOK], 0);
      exit;
    end;
    FResultFile := writtenPath;
    result := true;
    exit;
  end;

  //scaffold all - the first written file is the one loaded into the editor.
  for i := 0 to High(FLogicalPackages) do
  begin
    effectiveId := FLogicalPackages[i].Stem;
    scaffold := BuildPackageScaffold(FLogicalPackages[i], FCtx, effectiveId, FLogger);
    if not WriteScaffoldFile(scaffold, FCtx.RootDir, FLogger, writtenPath) then
    begin
      MessageDlg('Failed to write ' + writtenPath, mtError, [mbOK], 0);
      exit;
    end;
    if firstPath = '' then
      firstPath := writtenPath;
  end;
  FResultFile := firstPath;
  result := true;
end;

procedure TPackageWizardForm.btnFinishClick(Sender : TObject);
begin
  if DoFinish then
    ModalResult := mrOk;
end;

procedure TPackageWizardForm.btnCancelClick(Sender : TObject);
begin
  ModalResult := mrCancel;
end;

end.
