unit DPM.Creator.MainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.CheckLst,
  Vcl.Menus,
  Vcl.WinXPanels,
  Vcl.ExtCtrls,
  Vcl.Grids,
  Vcl.ExtDlgs,
  Vcl.Imaging.pngimage,
  Vcl.ValEdit,
  Vcl.ActnList,
  Vcl.ToolWin,
  Vcl.ActnMan,
  Vcl.ActnCtrls,
  System.Actions,
  System.RegularExpressions,
  Spring.Collections,
  DosCommand,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Creator.TemplateTreeNode,
  DPM.Creator.Dspec.FileHandler,
  DPM.Creator.MRUService,
  VSoft.Controls.Menus.MRU;

type
  TDSpecCreatorForm = class(TForm, IMRUSource)
    PageControl : TPageControl;
    tsInfo : TTabSheet;
    edtId : TEdit;
    lblId : TLabel;
    lblVersion : TLabel;
    edtVersion : TEdit;
    mmoDescription : TMemo;
    lblDescription : TLabel;
    lblProjectURL : TLabel;
    edtProjectURL : TEdit;
    lblRepositoryURL : TLabel;
    edtRepositoryURL : TEdit;
    lblLicense : TLabel;
    cboLicense : TComboBox;
    tsPlatforms : TTabSheet;
    lblTags : TLabel;
    edtTags : TEdit;
    clbCompilers : TCheckListBox;
    tsTemplates : TTabSheet;
    MainMenu : TMainMenu;
    mnuFile : TMenuItem;
    miNew : TMenuItem;
    miOpen : TMenuItem;
    miSave : TMenuItem;
    mnuSaveAs : TMenuItem;
    miExit : TMenuItem;
    N1 : TMenuItem;
    cboTemplate : TComboBox;
    clbPlatforms : TCheckListBox;
    lblTemplate : TLabel;
    OpenDialog : TOpenDialog;
    SaveDialog : TSaveDialog;
    CardPanel : TCardPanel;
    crdSource : TCard;
    crdSearchPathItem : TCard;
    lblSrc : TLabel;
    edtFileEntrySource : TEdit;
    chkFileEntryFlatten : TCheckBox;
    lblDest : TLabel;
    edtFileEntryDest : TEdit;
    lbFileEntryExclude : TListBox;
    btnAddExclude : TButton;
    btnDeleteExclude : TButton;
    crdBuild : TCard;
    crdRuntimeOrDesignBpl : TCard;
    lblSearchPaths : TLabel;
    lblRuntime : TLabel;
    lblBuild : TLabel;
    lblBuildId : TLabel;
    edtBuildId : TEdit;
    lblProject : TLabel;
    edtProject : TEdit;
    lblRuntimeSrc : TLabel;
    edtBPLEntrySrc : TEdit;
    lblRuntimeBuildId : TLabel;
    edtBPLEntryBuildId : TEdit;
    chkCopyLocal : TCheckBox;
    PopupMenu : TPopupMenu;
    BalloonHint1 : TBalloonHint;
    tsGenerate : TTabSheet;
    lblCompilers : TLabel;
    lblPlatform : TLabel;
    edtSearchPath : TEdit;
    miOptions : TMenuItem;
    lblAuthor : TLabel;
    edtAuthor : TEdit;
    crdDependency : TCard;
    Label1 : TLabel;
    lblDependencyId : TLabel;
    edtDependencyId : TEdit;
    edtDependencyVersion : TEdit;
    ImageList1 : TImageList;
    GridPanel1 : TGridPanel;
    Panel1 : TPanel;
    btnBuildPackages : TButton;
    Memo1 : TMemo;
    edtPackageOutputPath : TEdit;
    Label2 : TLabel;
    edtConfiguration : TEdit;
    lblConfiguration : TLabel;
    chkBuildForDesign : TCheckBox;
    chkDesignOnly : TCheckBox;
    crdTemplate : TCard;
    edtTemplateName : TEdit;
    lblTemplateName : TLabel;
    tsLogging : TTabSheet;
    Memo2 : TMemo;
    chkInstall : TCheckBox;
    VariablesList : TValueListEditor;
    ActionList1 : TActionList;
    actDeleteTemplate : TAction;
    actDuplicateTemplate : TAction;
    lblSPDX : TLabel;
    Label4 : TLabel;
    lblPackageId : TLabel;
    Label5 : TLabel;
    actAddBuildItem : TAction;
    actFileOpen : TAction;
    actFileSave : TAction;
    actFileSaveAs : TAction;
    actFileNew : TAction;
    actDeleteBuildItem : TAction;
    actAddRuntimeItem : TAction;
    actDeleteRuntimeItem : TAction;
    actAddDesignItem : TAction;
    actDeleteDesignItem : TAction;
    actAddSourceItem : TAction;
    actAddFileItem : TAction;
    actDeleteSourceItem : TAction;
    actDeleteFileItem : TAction;
    actAddLibItem : TAction;
    actDeleteLibItem : TAction;
    actAddSearchPath : TAction;
    actDeleteSearchPath : TAction;
    actAddDependency : TAction;
    actDeleteDependency : TAction;
    OpenPictureDialog1 : TOpenPictureDialog;
    pnlIcon : TPanel;
    ImgIcon : TImage;
    crdBuildHeading : TCard;
    lblBuildHeading : TLabel;
    lblBuildDescription : TLabel;
    crdSearchPathHeading : TCard;
    lblSearchPathsDescription : TLabel;
    lblSearchPathsHeading : TLabel;
    crdSourceHeading : TCard;
    lblSourceItemsHeading : TLabel;
    lblSourceItemsDescription : TLabel;
    crdDependenciesHeading : TCard;
    Label6 : TLabel;
    Label7 : TLabel;
    crdLibEntriesHeading : TCard;
    lblLibFilesHeading : TLabel;
    lblLibFilesDescription : TLabel;
    crdFileEntriesHeading : TCard;
    lblFileEntriesHeading : TLabel;
    lblFileEntriesDescription : TLabel;
    crdDesignHeading : TCard;
    lblDesignHeading : TLabel;
    lblDesignDescription : TLabel;
    crdRuntimeHeading : TCard;
    lblRuntimeHeading : TLabel;
    lblRuntimeDescription : TLabel;
    lblSourceItemHeader : TLabel;
    Label8 : TLabel;
    Label9 : TLabel;
    Label10 : TLabel;
    Label11 : TLabel;
    Help1 : TMenuItem;
    CreatingPackages1 : TMenuItem;
    N2 : TMenuItem;
    About1 : TMenuItem;
    actFileExit : TAction;
    mnuFileOpenSep : TMenuItem;
    pmCompilers : TPopupMenu;
    actCompilersSelectAll : TAction;
    actCompilersDeselectAll : TAction;
    SelectAll1 : TMenuItem;
    DeselectAll1 : TMenuItem;
    pmPlatforms : TPopupMenu;
    actPlatformsSelectAll : TAction;
    actPlatformsDeselectAll : TAction;
    SelectAll2 : TMenuItem;
    DeselectAll2 : TMenuItem;
    Label12 : TLabel;
    edtCopyright : TEdit;
    Panel2 : TPanel;
    lblTemplateView : TLabel;
    tvTemplates : TTreeView;
    Label3 : TLabel;
    btnAddTemplate : TButton;
    btnDeleteTemplate : TButton;
    btnDuplicateTemplate : TButton;
    Splitter1: TSplitter;
    procedure FormDestroy(Sender : TObject);
    procedure btnAddExcludeClick(Sender : TObject);
    procedure btnAddTemplateClick(Sender : TObject);
    procedure btnBuildPackagesClick(Sender : TObject);
    procedure btnDeleteExcludeClick(Sender : TObject);
    procedure cboLicenseChange(Sender : TObject);
    procedure cboTemplateChange(Sender : TObject);
    procedure chkBuildForDesignClick(Sender : TObject);
    procedure chkCopyLocalClick(Sender : TObject);
    procedure chkDesignInstallClick(Sender : TObject);
    procedure chkDesignOnlyClick(Sender : TObject);
    procedure clbCompilersClick(Sender : TObject);
    procedure clbCompilersClickCheck(Sender : TObject);
    procedure clbPlatformsClickCheck(Sender : TObject);
    procedure DosCommandNewLine(ASender : TObject; const ANewLine : string; AOutputType : TOutputType);
    procedure DosCommandTerminated(Sender : TObject);
    procedure edtAuthorChange(Sender : TObject);
    procedure edtBuildIdChange(Sender : TObject);
    procedure edtConfigurationChange(Sender : TObject);
    procedure edtDependencyIdChange(Sender : TObject);
    procedure edtDependencyVersionChange(Sender : TObject);
    procedure edtDesignBuildIdChange(Sender : TObject);
    procedure edtDesignSrcChange(Sender : TObject);
    procedure edtFileEntrySourceChange(Sender : TObject);
    procedure edtFileEntryDestChange(Sender : TObject);
    procedure edtIdChange(Sender : TObject);
    procedure edtProjectChange(Sender : TObject);
    procedure edtProjectURLChange(Sender : TObject);
    procedure edtRepositoryURLChange(Sender : TObject);
    procedure edtBPLEntrySrcChange(Sender : TObject);
    procedure edtSearchPathChange(Sender : TObject);
    procedure edtTagsChange(Sender : TObject);
    procedure edtTemplateNameChange(Sender : TObject);
    procedure edtVersionChange(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : Boolean);
    procedure mmoDescriptionChange(Sender : TObject);
    procedure miOptionsClick(Sender : TObject);
    procedure tvTemplatesChange(Sender : TObject; Node : TTreeNode);
    procedure tvTemplatesCollapsing(Sender : TObject; Node : TTreeNode; var AllowCollapse : Boolean);
    procedure tvTemplatesContextPopup(Sender : TObject; MousePos : TPoint; var Handled : Boolean);
    procedure tvTemplatesCreateNodeClass(Sender : TCustomTreeView; var NodeClass : TTreeNodeClass);
    procedure tvTemplatesEdited(Sender : TObject; Node : TTreeNode; var S : string);
    procedure tvTemplatesEditing(Sender : TObject; Node : TTreeNode; var AllowEdit : Boolean);
    procedure VariablesListStringsChange(Sender : TObject);
    procedure actDuplicateTemplateExecute(Sender : TObject);
    procedure ActionList1Update(Action : TBasicAction; var Handled : Boolean);
    procedure actDeleteTemplateExecute(Sender : TObject);
    procedure lblSPDXClick(Sender : TObject);
    procedure UriLabelClick(Sender : TObject);
    procedure UriLabelMouseEnter(Sender : TObject);
    procedure UriLabelMouseLeave(Sender : TObject);
    procedure actAddBuildItemExecute(Sender : TObject);
    procedure actDeleteBuildItemExecute(Sender : TObject);
    procedure actAddDependencyExecute(Sender : TObject);
    procedure actDeleteDependencyExecute(Sender : TObject);
    procedure actAddDesignItemExecute(Sender : TObject);
    procedure actDeleteDesignItemExecute(Sender : TObject);
    procedure actAddFileItemExecute(Sender : TObject);
    procedure actDeleteFileItemExecute(Sender : TObject);
    procedure actAddLibItemExecute(Sender : TObject);
    procedure actDeleteLibItemExecute(Sender : TObject);
    procedure actFileOpenExecute(Sender : TObject);
    procedure actFileSaveExecute(Sender : TObject);
    procedure actFileSaveAsExecute(Sender : TObject);
    procedure actFileNewExecute(Sender : TObject);
    procedure actAddRuntimeItemExecute(Sender : TObject);
    procedure actDeleteRuntimeItemExecute(Sender : TObject);
    procedure actAddSearchPathExecute(Sender : TObject);
    procedure actDeleteSearchPathExecute(Sender : TObject);
    procedure actAddSourceItemExecute(Sender : TObject);
    procedure actDeleteSourceItemExecute(Sender : TObject);
    procedure ImgIconClick(Sender : TObject);
    procedure CreatingPackages1Click(Sender : TObject);
    procedure About1Click(Sender : TObject);
    procedure actFileExitExecute(Sender : TObject);
    procedure FormClose(Sender : TObject; var Action : TCloseAction);
    procedure edtVersionExit(Sender : TObject);
    procedure edtPackageOutputPathExit(Sender : TObject);
    procedure actCompilersDeselectAllExecute(Sender : TObject);
    procedure actCompilersSelectAllExecute(Sender : TObject);
    procedure actPlatformsSelectAllExecute(Sender : TObject);
    procedure actPlatformsDeselectAllExecute(Sender : TObject);
    procedure edtCopyrightChange(Sender : TObject);
    procedure edtBPLEntryBuildIdChange(Sender: TObject);
  private
    { Private declarations }
    FtmpFilename : string;
    FOpenFile : TDSpecFile;
    FDosCommand : TDosCommand;
    FTemplate : ISpecTemplate;
    FLogger : ILogger;
    FInVariableUpdate : Boolean;
    FSPDXList : TStringList;
    FMRUMenu : TMRUMenu;
  protected
    // IMRUSource
    procedure MRUAdd(const filename : string);
    function MRURemove(const filename : string) : Boolean;
    procedure MRULoad(const list : TStrings);
    procedure MRUSave(const list : TStrings);
    function MRUCount : integer;
    // IMRUSource
    procedure MRUListClick(Sender : TObject; const filename : string);

    procedure UpdateFormCaption(const value : string);

    procedure OpenProject(const filename : string);

    procedure EnableControls(value : Boolean);
    procedure DeleteSelectedEntry;

    function FindTemplateNode(const template : ISpecTemplate) : TTemplateTreeNode;
    function FindHeadingNode(const templateNode : TTemplateTreeNode; nodeType : TNodeType) : TTemplateTreeNode;

    function LoadTemplate(const template : ISpecTemplate) : TTemplateTreeNode;
    procedure LoadTemplates;
    procedure EnableDisablePlatform(compilerVersion : TCompilerVersion);
    function ReplaceVars(const inputStr : String; compiler : TCompilerVersion) : string;
    function AddRootTemplateNode(template : ISpecTemplate) : TTemplateTreeNode;

    function LoadSourceNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const sourceEntry : ISpecFileEntry) : TTemplateTreeNode;
    procedure LoadSourceNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecFileEntry>);

    function LoadLibNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const libEntry : ISpecFileEntry) : TTemplateTreeNode;
    procedure LoadLibNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecFileEntry>);

    function LoadFileNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileEntry : ISpecFileEntry) : TTemplateTreeNode;
    procedure LoadFileNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecFileEntry>);

    function LoadSearchPathNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const searchPath : ISpecSearchPath) : TTemplateTreeNode;
    procedure LoadSearchPathNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);

    function LoadBuildNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const buildEntry : ISpecBuildEntry) : TTemplateTreeNode;
    procedure LoadBuildNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);

    function LoadDesigntimeNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const item : ISpecBPLEntry) : TTemplateTreeNode;
    procedure LoadDesigntimeNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecBPLEntry>);

    function LoadRuntimeNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const item : ISpecBPLEntry) : TTemplateTreeNode;
    procedure LoadRuntimeNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecBPLEntry>);

    function LoadDependency(const parentNode : TTemplateTreeNode; template : ISpecTemplate; const dependency : ISpecDependency) : TTemplateTreeNode;
    procedure LoadDependencies(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);

    procedure UriClick(const uri : string);

    procedure LoadSPDXList;
    procedure LoadDspecStructure;
    procedure SaveDspecStructure(const filename : string);

    function SelectedPlatform : ISpecTargetPlatform;

  public
    { Public declarations }
  end;

var
  DSpecCreatorForm : TDSpecCreatorForm;

implementation

{$R *.dfm}

uses
  System.UITypes,
  System.IOUtils,
  System.IniFiles,
  Winapi.ShellAPI,
  DPM.Core.dependency.Version,
  DPM.Creator.TemplateForm,
  DPM.Creator.FileForm,
  DPM.Creator.BuildForm,
  DPM.Creator.RuntimeForm,
  DPM.Creator.SearchPathForm,
  DPM.Creator.OptionsForm,
  DPM.Creator.DependencyForm,
  DPM.Creator.Logger,
  DPM.Creator.Dspec.Replacer,
  DPM.IDE.AboutForm;

const
  cToolName = 'DPM dspec Creator';
  cNewTemplate = 'Create New Template...';

procedure TDSpecCreatorForm.btnDeleteExcludeClick(Sender : TObject);
var
  exclude : string;
  itemToDelete : integer;
  entry : ISpecFileEntry;
begin
  if lbFileEntryExclude.ItemIndex < 0 then
    Exit;

  if Assigned(tvTemplates.Selected) then
  begin
    entry := (tvTemplates.Selected as TTemplateTreeNode).fileEntry;
    if not Assigned(entry) then
      Exit;

    exclude := lbFileEntryExclude.Items[lbFileEntryExclude.ItemIndex];
    lbFileEntryExclude.DeleteSelected;
    itemToDelete := entry.exclude.IndexOf(exclude);
    entry.exclude.Delete(itemToDelete);
  end;
end;

procedure TDSpecCreatorForm.btnAddExcludeClick(Sender : TObject);
var
  src : string;
  entry : ISpecFileEntry;
begin
  src := InputBox('Add Exclude', 'Exclude to Add', '');

  if Assigned(tvTemplates.Selected) then
  begin
    entry := (tvTemplates.Selected as TTemplateTreeNode).fileEntry;
    if not Assigned(entry) then
      Exit;

    entry.exclude.Add(src);
    lbFileEntryExclude.Items.Add(src);
  end;
end;

procedure TDSpecCreatorForm.btnAddTemplateClick(Sender : TObject);
var
  templateName : string;
  TemplateForm : TTemplateForm;
  newTemplate : ISpecTemplate;
  templateNode : TTemplateTreeNode;
begin
  TemplateForm := TTemplateForm.Create(nil);
  try
    if not FOpenFile.DoesTemplateExist('default') then
      TemplateForm.edtTemplate.Text := 'default';
    if TemplateForm.ShowModal = mrCancel then
      Exit;
    templateName := TemplateForm.edtTemplate.Text;
    if templateName.IsEmpty then
      Exit;
  finally
    FreeAndNil(TemplateForm);
  end;
  newTemplate := FOpenFile.Spec.newTemplate(templateName);
  templateNode := LoadTemplate(newTemplate);
  tvTemplates.Selected := templateNode;
end;

procedure TDSpecCreatorForm.btnBuildPackagesClick(Sender : TObject);
var
  guid : TGUID;
begin
  guid := TGUID.NewGuid;
  FtmpFilename := FOpenFile.WorkingDir;
  FtmpFilename := TPath.Combine(FtmpFilename, guid.ToString);
  FtmpFilename := ChangeFileExt(FtmpFilename, '.dspec');
  TFile.WriteAllText(FtmpFilename, FOpenFile.AsString);
  if DirectoryExists(edtPackageOutputPath.Text) then
    FDosCommand.CommandLine := 'dpm pack "' + FtmpFilename + '" -o=' + edtPackageOutputPath.Text;
  FDosCommand.Execute;
end;

procedure TDSpecCreatorForm.DeleteSelectedEntry;
var
  selectedNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
begin
  selectedNode := tvTemplates.Selected as TTemplateTreeNode;
  if selectedNode <> nil then
  begin
    selectedNode.DeleteEntry;;
    parentNode := selectedNode.Parent as TTemplateTreeNode;
    tvTemplates.Items.Delete(selectedNode);
    tvTemplates.Selected := parentNode;
  end;
end;

procedure TDSpecCreatorForm.DosCommandNewLine(ASender : TObject; const ANewLine : string; AOutputType : TOutputType);
begin
  if AOutputType = otEntireLine then
    Memo1.Lines.Add(ANewLine);
end;

procedure TDSpecCreatorForm.DosCommandTerminated(Sender : TObject);
begin
  TFile.Delete(FtmpFilename);
  FtmpFilename := '';
end;

procedure TDSpecCreatorForm.cboLicenseChange(Sender : TObject);
begin
  FOpenFile.Spec.metadata.license := cboLicense.Text;
  var  i := FSPDXList.IndexOfName(cboLicense.Text);
  if i <> -1 then
  begin
    var data := FSPDXList.ValueFromIndex[i];
    var Items := data.Split([',']);
    if Length(Items) <> 2 then // just in case the txt file is messed up.
    begin
      lblSPDX.Visible := false;
      Exit;
    end;

    lblSPDX.Caption := Items[0];
    lblSPDX.Hint := Items[1];
    lblSPDX.Visible := true;
  end
  else
  begin
    lblSPDX.Visible := false;

  end;
end;

procedure TDSpecCreatorForm.cboTemplateChange(Sender : TObject);
var
  templateName : string;
  vPlatform : ISpecTargetPlatform;
begin
  templateName := cboTemplate.Items[cboTemplate.ItemIndex];
  if templateName = cNewTemplate then
  begin
    PageControl.ActivePage := tsTemplates;
    btnAddTemplateClick(Sender);
    cboTemplate.ItemIndex := -1;
    Exit;
  end;

  if clbCompilers.ItemIndex < 0 then
    raise Exception.Create('Please select a compiler before trying to set the template');
  vPlatform := FOpenFile.GetPlatform(clbCompilers.Items[clbCompilers.ItemIndex]);

  if not Assigned(vPlatform) then
  begin
    vPlatform := FOpenFile.AddCompiler(clbCompilers.Items[clbCompilers.ItemIndex]);
  end;
  vPlatform.TemplateName := templateName;
  cboTemplate.ItemIndex := cboTemplate.Items.IndexOf(templateName);
end;

procedure TDSpecCreatorForm.chkBuildForDesignClick(Sender : TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).build.buildForDesign := chkBuildForDesign.Checked;
  end;
end;

procedure TDSpecCreatorForm.chkCopyLocalClick(Sender : TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.copyLocal := chkCopyLocal.Checked;
  end;
end;

procedure TDSpecCreatorForm.chkDesignInstallClick(Sender : TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.Install := chkInstall.Checked;
  end;
end;

procedure TDSpecCreatorForm.chkDesignOnlyClick(Sender : TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).build.designOnly := chkDesignOnly.Checked;
  end;
end;

procedure TDSpecCreatorForm.clbCompilersClick(Sender : TObject);
var
  j : integer;
  vPlatform : ISpecTargetPlatform;
  compilerVersion : TCompilerVersion;
begin
  FInVariableUpdate := true;
  try
    VariablesList.Strings.Clear;
  finally
    FInVariableUpdate := false;
  end;
  if clbCompilers.ItemIndex < 0 then
  begin
    cboTemplate.ItemIndex := -1;
    Exit;
  end;

  vPlatform := FOpenFile.GetPlatform(clbCompilers.Items[clbCompilers.ItemIndex]);
  compilerVersion := StringToCompilerVersion(clbCompilers.Items[clbCompilers.ItemIndex]);

  lblPlatform.Caption := clbCompilers.Items[clbCompilers.ItemIndex] + ' - Platforms';
  lblTemplate.Caption := clbCompilers.Items[clbCompilers.ItemIndex] + ' - Template';
  EnableDisablePlatform(compilerVersion);

  // these default to disabled until a compiler version is selected.
  // if the compiler version is unchecked then the controls will be disabled.
  EnableControls(clbCompilers.Checked[clbCompilers.ItemIndex]);

  if clbCompilers.Checked[clbCompilers.ItemIndex] and not Assigned(vPlatform) then
  begin
    vPlatform := FOpenFile.AddCompiler(clbCompilers.Items[clbCompilers.ItemIndex]);
  end;

  if Assigned(vPlatform) then
  begin
    for j := 0 to clbPlatforms.Count - 1 do
    begin
      clbPlatforms.Checked[j] := false;
    end;
    cboTemplate.ItemIndex := cboTemplate.Items.IndexOf(vPlatform.TemplateName);
  end;

  if not Assigned(vPlatform) then
  begin
    cboTemplate.ItemIndex := -1;
    Exit;
  end;

  for var platform in vPlatform.Platforms do
  begin
    var platformName := DPMPlatformToString(platform);
    var i := clbPlatforms.Items.IndexOf(platformName);
    if i <> -1 then
      clbPlatforms.Checked[i] := true;
  end;

  FInVariableUpdate := true;
  try
    VariablesList.Strings.Clear;
    VariablesList.Strings.AddStrings(vPlatform.Variables);
  finally
    FInVariableUpdate := false;
  end;

  cboTemplate.Clear;
  LoadTemplates;

  cboTemplate.ItemIndex := cboTemplate.Items.IndexOf(vPlatform.TemplateName);
end;

procedure TDSpecCreatorForm.clbCompilersClickCheck(Sender : TObject);
var
  vPlatform : ISpecTargetPlatform;
  compiler : string;
begin
  if clbCompilers.ItemIndex < 0 then
    Exit;
  compiler := clbCompilers.Items[clbCompilers.ItemIndex];
  vPlatform := FOpenFile.GetPlatform(compiler);
  if clbCompilers.Checked[clbCompilers.ItemIndex] and not Assigned(vPlatform) then
  begin
    vPlatform := FOpenFile.AddCompiler(compiler);
  end
  else if Assigned(vPlatform) and (clbCompilers.Checked[clbCompilers.ItemIndex] = false) then
  begin
    FOpenFile.DeleteCompiler(compiler);
  end;

  EnableControls(clbCompilers.Checked[clbCompilers.ItemIndex])
end;

procedure TDSpecCreatorForm.clbPlatformsClickCheck(Sender : TObject);
var
  vPlatform : ISpecTargetPlatform;
  compiler : string;
  platformString : string;
  dpmPlatform : TDPMPlatform;
  i, j : integer;
  platformCount : integer;
  newPlatforms : TArray<TDPMPlatform>;
begin
  if clbCompilers.ItemIndex < 0 then
  begin
    raise Exception.Create('You must select a compiler before you can select platforms');
  end;
  // if clbPlatforms.ItemIndex < 0 then
  // Exit;

  compiler := clbCompilers.Items[clbCompilers.ItemIndex];
  vPlatform := FOpenFile.GetPlatform(compiler);
  if vPlatform = nil then
    Exit;

  platformCount := 0;
  for i := 0 to clbPlatforms.Count - 1 do
  begin
    if clbPlatforms.Checked[i] then
    begin
      platformString := clbPlatforms.Items[i];
      dpmPlatform := StringToDPMPlatform(platformString);
      if dpmPlatform = TDPMPlatform.UnknownPlatform then
        continue;
      Inc(platformCount);
    end;
  end;

  SetLength(newPlatforms, platformCount);
  j := 0;
  for i := 0 to clbPlatforms.Count - 1 do
  begin
    if not clbPlatforms.Checked[i] then
      continue;
    platformString := clbPlatforms.Items[i];

    dpmPlatform := StringToDPMPlatform(platformString);

    if dpmPlatform = TDPMPlatform.UnknownPlatform then
      continue;
    newPlatforms[j] := dpmPlatform;
    Inc(j);
  end;

  vPlatform.Platforms := newPlatforms;
end;

procedure TDSpecCreatorForm.CreatingPackages1Click(Sender : TObject);
begin
  UriClick('https://docs.delphi.dev/getting-started/creating-packages.html');
end;

procedure TDSpecCreatorForm.edtAuthorChange(Sender : TObject);
begin
  FOpenFile.Spec.metadata.authors := edtAuthor.Text;
end;

procedure TDSpecCreatorForm.edtBuildIdChange(Sender : TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).build.id := edtBuildId.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtBuildId.Text;
  end;
end;

procedure TDSpecCreatorForm.edtConfigurationChange(Sender : TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).build.Config := edtConfiguration.Text;
  end;
end;

procedure TDSpecCreatorForm.edtCopyrightChange(Sender : TObject);
begin
  FOpenFile.Spec.metadata.Copyright := edtCopyright.Text;
end;

procedure TDSpecCreatorForm.edtDependencyIdChange(Sender : TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).dependency.id := edtDependencyId.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtDependencyId.Text + ' - ' + edtDependencyVersion.Text;
  end;
end;

procedure TDSpecCreatorForm.edtDependencyVersionChange(Sender : TObject);
var
  ver : TVersionRange;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    if Length(edtDependencyVersion.Text) > 0 then
    begin
      if TVersionRange.TryParse(edtDependencyVersion.Text, ver) then
      begin
        (tvTemplates.Selected as TTemplateTreeNode).dependency.Version := ver;
        (tvTemplates.Selected as TTemplateTreeNode).Text := edtDependencyId.Text + ' - ' + edtDependencyVersion.Text;
      end;
    end;
  end;
end;

procedure TDSpecCreatorForm.edtDesignSrcChange(Sender : TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.Source := edtBPLEntrySrc.Text;

    str := 'Possible Expanded Paths:' + System.sLineBreak;

    for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if compiler = TCompilerVersion.UnknownVersion then
        continue;
      str := str + System.sLineBreak + ReplaceVars(edtBPLEntrySrc.Text, compiler);
    end;
    edtBPLEntrySrc.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.edtDesignBuildIdChange(Sender : TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.buildId := edtBPLEntryBuildId.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtBPLEntryBuildId.Text;
  end;
end;

procedure TDSpecCreatorForm.edtFileEntryDestChange(Sender : TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).fileEntry.Destination := edtFileEntryDest.Text;

    str := 'Possible Expanded Paths:' + System.sLineBreak;

    for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if compiler = TCompilerVersion.UnknownVersion then
        continue;
      str := str + System.sLineBreak + ReplaceVars(edtFileEntryDest.Text, compiler);
    end;
    edtFileEntryDest.Hint := str;
  end;
end;

function TDSpecCreatorForm.AddRootTemplateNode(template : ISpecTemplate) : TTemplateTreeNode;
begin
  cboTemplate.Items.Insert(cboTemplate.Items.Count - 1, template.name);
  result := tvTemplates.Items.Add(nil, template.name) as TTemplateTreeNode;
  result.NodeType := ntTemplateHeading;
  result.Template := template;
  result.ImageIndex := 5;
  result.SelectedIndex := 5;
  result.TemplateHeading := true;
end;

function TDSpecCreatorForm.LoadSourceNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const sourceEntry : ISpecFileEntry) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, sourceEntry.Source) as TTemplateTreeNode;
  result.fileEntry := sourceEntry;
  result.NodeType := ntSource;
  result.Template := template;
  result.ImageIndex := 2;
  result.SelectedIndex := 2;
  result.AddAction := actAddSourceItem;
  result.DeleteAction := actDeleteSourceItem;
end;

procedure TDSpecCreatorForm.LoadSourceNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecFileEntry>);
var
  nodeSource : TTemplateTreeNode;
  j : integer;
begin
  nodeSource := tvTemplates.Items.AddChild(parentNode, 'Source') as TTemplateTreeNode;
  nodeSource.Template := template;
  nodeSource.NodeType := ntSourceHeading;
  nodeSource.ImageIndex := 2;
  nodeSource.SelectedIndex := 2;

  nodeSource.AddAction := actAddSourceItem;
  nodeSource.DeleteAction := actDeleteSourceItem;

  for j := 0 to fileList.Count - 1 do
    LoadSourceNode(nodeSource, template, fileList[j]);
end;

function TDSpecCreatorForm.LoadLibNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const libEntry : ISpecFileEntry) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, libEntry.Source) as TTemplateTreeNode;
  result.fileEntry := libEntry;
  result.NodeType := ntLib;
  result.Template := template;
  result.ImageIndex := 2;
  result.SelectedIndex := 2;
  result.AddAction := actAddLibItem;
  result.DeleteAction := actDeleteLibItem;
end;

procedure TDSpecCreatorForm.LoadLibNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecFileEntry>);
var
  libsNode : TTemplateTreeNode;
  j : integer;
begin

  libsNode := tvTemplates.Items.AddChild(parentNode, 'Libs') as TTemplateTreeNode;
  libsNode.Template := template;
  libsNode.NodeType := ntLibHeading;
  libsNode.ImageIndex := 2;
  libsNode.SelectedIndex := 2;

  libsNode.AddAction := actAddLibItem;
  libsNode.DeleteAction := actDeleteLibItem;

  for j := 0 to fileList.Count - 1 do
    LoadLibNode(libsNode, template, fileList[j]);
end;

function TDSpecCreatorForm.LoadFileNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileEntry : ISpecFileEntry) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, fileEntry.Source) as TTemplateTreeNode;
  result.fileEntry := fileEntry;
  result.NodeType := ntFile;
  result.Template := template;
  result.ImageIndex := 2;
  result.SelectedIndex := 2;
  result.AddAction := actAddFileItem;
  result.DeleteAction := actDeleteFileItem;
end;

procedure TDSpecCreatorForm.LoadFileNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecFileEntry>);
var
  filesNode : TTemplateTreeNode;
  j : integer;
begin

  filesNode := tvTemplates.Items.AddChild(parentNode, 'Files') as TTemplateTreeNode;
  filesNode.Template := template;
  filesNode.NodeType := ntFileHeading;
  filesNode.ImageIndex := 2;
  filesNode.SelectedIndex := 2;

  filesNode.AddAction := actAddFileItem;
  filesNode.DeleteAction := actDeleteFileItem;

  for j := 0 to fileList.Count - 1 do
    LoadFileNode(filesNode, template, fileList[j]);
end;

function TDSpecCreatorForm.LoadSearchPathNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const searchPath : ISpecSearchPath) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, searchPath.Path) as TTemplateTreeNode;
  result.SearchPath := searchPath;
  result.Template := template;
  result.NodeType := ntSeachPath;
  result.ImageIndex := 3;
  result.SelectedIndex := 3;
end;

procedure TDSpecCreatorForm.LoadSearchPathNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);
var
  nodeSearchPath : TTemplateTreeNode;
  j : integer;
begin
  nodeSearchPath := tvTemplates.Items.AddChild(parentNode, 'SearchPaths') as TTemplateTreeNode;
  nodeSearchPath.Template := template;
  nodeSearchPath.ImageIndex := 3;
  nodeSearchPath.SelectedIndex := 3;
  nodeSearchPath.NodeType := ntSeachPathHeading;

  nodeSearchPath.AddAction := actAddSearchPath;
  nodeSearchPath.DeleteAction := actDeleteSearchPath;

  for j := 0 to template.SearchPaths.Count - 1 do
    LoadSearchPathNode(nodeSearchPath, template, template.SearchPaths[j]);
end;

function TDSpecCreatorForm.LoadBuildNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const buildEntry : ISpecBuildEntry) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, buildEntry.id) as TTemplateTreeNode;
  result.build := buildEntry;
  result.Template := template;
  result.ImageIndex := 0;
  result.SelectedIndex := 0;
  result.NodeType := ntBuild;
end;

procedure TDSpecCreatorForm.LoadBuildNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);
var
  nodeBuild : TTemplateTreeNode;
  j : integer;
begin
  nodeBuild := tvTemplates.Items.AddChild(parentNode, 'Build') as TTemplateTreeNode;
  nodeBuild.Template := template;
  nodeBuild.ImageIndex := 0;
  nodeBuild.SelectedIndex := 0;
  nodeBuild.NodeType := ntBuildHeading;

  nodeBuild.AddAction := actAddBuildItem;
  nodeBuild.DeleteAction := actDeleteBuildItem;

  for j := 0 to template.BuildEntries.Count - 1 do
  begin
    LoadBuildNode(nodeBuild, template, template.BuildEntries[j]);
  end;
end;

procedure TDSpecCreatorForm.About1Click(Sender : TObject);
var
  AboutForm : TDPMAboutForm;
begin
  AboutForm := TDPMAboutForm.Create(nil);
  try
    AboutForm.ShowModal;
  finally
    AboutForm.Free;
  end;
end;

procedure TDSpecCreatorForm.actAddBuildItemExecute(Sender : TObject);
var
  buildId : string;
  BuildForm : TBuildForm;
  build : ISpecBuildEntry;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  buildNode : TTemplateTreeNode;
begin
  BuildForm := TBuildForm.Create(nil);
  try
//    BuildForm.edtBuildId.Text := 'default';

    if BuildForm.ShowModal = mrCancel then
      Exit;
    buildId := BuildForm.edtBuildId.Text;
    if buildId.IsEmpty then
      Exit;
    build := FTemplate.NewBuildEntryById(buildId);
    build.project := BuildForm.edtProject.Text;
  finally
    FreeAndNil(BuildForm);
  end;

  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntBuildHeading);
  tvTemplates.Items.BeginUpdate;
  try
    buildNode := LoadBuildNode(parentNode, FTemplate, build);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := buildNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;
end;

procedure TDSpecCreatorForm.actAddDependencyExecute(Sender : TObject);
var
  dependancyId : string;
  DependencyForm : TDependencyForm;
  dependency : ISpecDependency;
  ver : TVersionRange;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  dependencyNode : TTemplateTreeNode;
begin
  DependencyForm := TDependencyForm.Create(nil);
  try
//    DependencyForm.edtDependencyId.Text := 'default';
//    DependencyForm.edtVersion.Text := '1.0.0';

    if DependencyForm.ShowModal = mrCancel then
      Exit;
    dependancyId := DependencyForm.edtDependencyId.Text;
    if dependancyId.IsEmpty then
      Exit;
    dependency := FTemplate.NewDependencyById(dependancyId);
    if Length(DependencyForm.edtVersion.Text) > 0 then
    begin
      if TVersionRange.TryParse(DependencyForm.edtVersion.Text, ver) then
      begin
        dependency.Version := ver;
      end;
    end;
  finally
    FreeAndNil(DependencyForm);
  end;
  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntDependencyHeading);
  tvTemplates.Items.BeginUpdate;
  try
    dependencyNode := LoadDependency(parentNode, FTemplate, dependency);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := dependencyNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;

end;

procedure TDSpecCreatorForm.actAddDesignItemExecute(Sender : TObject);
var
  designBuidId : string;
  DesignForm : TBplForm;
  design : ISpecBPLEntry;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  designNode : TTemplateTreeNode;
begin
  DesignForm := TBplForm.Create(nil);
  try
    //DesignForm.edtBuildId.Text := 'buildId';

    if DesignForm.ShowModal = mrCancel then
      Exit;
    designBuidId := DesignForm.edtBuildId.Text;
    if designBuidId.IsEmpty then
      Exit;
    design := FTemplate.NewDesignBplBySrc(DesignForm.edtSource.Text);
    design.buildId := DesignForm.edtBuildId.Text;
    design.Source := DesignForm.edtSource.Text;
    design.Install := DesignForm.chkInstall.Checked;
  finally
    FreeAndNil(DesignForm);
  end;

  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntDesignHeading);
  tvTemplates.Items.BeginUpdate;
  try
    designNode := LoadDesigntimeNode(parentNode, FTemplate, design);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := designNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;
end;

procedure TDSpecCreatorForm.actAddFileItemExecute(Sender : TObject);
var
  SourceSrc : string;
  FileForm : TSourceForm;
  fileEntry : ISpecFileEntry;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  fileNode : TTemplateTreeNode;
begin
  FileForm := TSourceForm.Create(nil);
  try
    FileForm.Caption := 'Add File entry';
//    FileForm.edtSource.Text := 'default';

    if FileForm.ShowModal = mrCancel then
      Exit;
    SourceSrc := FileForm.edtSource.Text;
    if SourceSrc.IsEmpty then
      Exit;
    fileEntry := FTemplate.NewFiles(SourceSrc);
    fileEntry.flatten := FileForm.chkFlatten.Checked;
    fileEntry.Destination := FileForm.edtDest.Text;
  finally
    FreeAndNil(FileForm);
  end;
  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntFileHeading);
  tvTemplates.Items.BeginUpdate;
  try
    fileNode := LoadFileNode(parentNode, FTemplate, fileEntry);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := fileNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;
end;

procedure TDSpecCreatorForm.actAddLibItemExecute(Sender : TObject);
var
  SourceSrc : string;
  LibForm : TSourceForm;
  libEntry : ISpecFileEntry;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  libNode : TTemplateTreeNode;
begin
  LibForm := TSourceForm.Create(nil);
  try
    LibForm.Caption := 'Add Lib Item';
//    LibForm.edtSource.Text := 'default';

    if LibForm.ShowModal = mrCancel then
      Exit;
    SourceSrc := LibForm.edtSource.Text;
    if SourceSrc.IsEmpty then
      Exit;
    libEntry := FTemplate.NewLib(SourceSrc);
    libEntry.flatten := LibForm.chkFlatten.Checked;
    libEntry.Destination := LibForm.edtDest.Text;
  finally
    FreeAndNil(LibForm);
  end;
  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntLibHeading);
  tvTemplates.Items.BeginUpdate;
  try
    libNode := LoadLibNode(parentNode, FTemplate, libEntry);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := libNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;
end;

procedure TDSpecCreatorForm.actAddRuntimeItemExecute(Sender : TObject);
var
  runtimeBuildId : string;
  RuntimeForm : TBplForm;
  runtime : ISpecBPLEntry;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  runtimeNode : TTemplateTreeNode;
begin
  RuntimeForm := TBplForm.Create(nil);
  try
//    RuntimeForm.edtBuildId.Text := 'default';

    if RuntimeForm.ShowModal = mrCancel then
      Exit;
    runtimeBuildId := RuntimeForm.edtBuildId.Text;
    if runtimeBuildId.IsEmpty then
      Exit;
    runtime := FTemplate.NewRuntimeBplBySrc(RuntimeForm.edtSource.Text);
    runtime.Source := RuntimeForm.edtSource.Text;
    runtime.buildId := runtimeBuildId;
    runtime.copyLocal := RuntimeForm.chkCopyLocal.Checked;
  finally
    FreeAndNil(RuntimeForm);
  end;
  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntRuntimeHeading);
  tvTemplates.Items.BeginUpdate;
  try
    runtimeNode := LoadRuntimeNode(parentNode, FTemplate, runtime);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := runtimeNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;
end;

procedure TDSpecCreatorForm.actAddSearchPathExecute(Sender : TObject);
var
  searchPathStr : string;
  SearchPathForm : TSearchPathForm;
  searchPath : ISpecSearchPath;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  searchNode : TTemplateTreeNode;
begin
  SearchPathForm := TSearchPathForm.Create(nil);
  try
//    SearchPathForm.edtSearchPath.Text := 'default';

    if SearchPathForm.ShowModal = mrCancel then
      Exit;
    searchPathStr := SearchPathForm.edtSearchPath.Text;
    if searchPathStr.IsEmpty then
      Exit;
    searchPath := FTemplate.NewSearchPath(searchPathStr);
  finally
    FreeAndNil(SearchPathForm);
  end;
  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntSeachPathHeading);
  tvTemplates.Items.BeginUpdate;
  try
    searchNode := LoadSearchPathNode(parentNode, FTemplate, searchPath);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := searchNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;
end;

procedure TDSpecCreatorForm.actAddSourceItemExecute(Sender : TObject);
var
  SourceSrc : string;
  SourceForm : TSourceForm;
  Source : ISpecFileEntry;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  sourceNode : TTemplateTreeNode;
begin
  SourceForm := TSourceForm.Create(nil);
  try
    SourceForm.Caption := 'Add Source Item';
//    SourceForm.edtSource.Text := 'default';

    if SourceForm.ShowModal = mrCancel then
      Exit;
    SourceSrc := SourceForm.edtSource.Text;
    if SourceSrc.IsEmpty then
      Exit;

    Source := FTemplate.NewSource(SourceSrc);
    Source.flatten := SourceForm.chkFlatten.Checked;
    Source.Destination := SourceForm.edtDest.Text;
  finally
    FreeAndNil(SourceForm);
  end;

  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntSourceHeading);
  tvTemplates.Items.BeginUpdate;
  try
    sourceNode := LoadSourceNode(parentNode, FTemplate, Source);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := sourceNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;

end;

procedure TDSpecCreatorForm.actCompilersDeselectAllExecute(Sender : TObject);
begin
  clbCompilers.CheckAll(TCheckBoxState.cbUnchecked);
end;

procedure TDSpecCreatorForm.actCompilersSelectAllExecute(Sender : TObject);
begin
  clbCompilers.CheckAll(TCheckBoxState.cbChecked);
  clbCompilersClick(clbCompilers);
end;

procedure TDSpecCreatorForm.actDeleteBuildItemExecute(Sender : TObject);
begin
  DeleteSelectedEntry;
end;

procedure TDSpecCreatorForm.actDeleteDependencyExecute(Sender : TObject);
begin
  DeleteSelectedEntry;
end;

procedure TDSpecCreatorForm.actDeleteDesignItemExecute(Sender : TObject);
begin
  DeleteSelectedEntry;
end;

procedure TDSpecCreatorForm.actDeleteFileItemExecute(Sender : TObject);
begin
  DeleteSelectedEntry;
end;

procedure TDSpecCreatorForm.actDeleteLibItemExecute(Sender : TObject);
begin
  DeleteSelectedEntry;
end;

procedure TDSpecCreatorForm.actDeleteRuntimeItemExecute(Sender : TObject);
begin
  DeleteSelectedEntry;
end;

procedure TDSpecCreatorForm.actDeleteSearchPathExecute(Sender : TObject);
begin
  DeleteSelectedEntry;
end;

procedure TDSpecCreatorForm.actDeleteSourceItemExecute(Sender : TObject);
begin
  DeleteSelectedEntry;
end;

procedure TDSpecCreatorForm.actDeleteTemplateExecute(Sender : TObject);
var
  templateName : string;
  selectedNode : TTemplateTreeNode;
  i : integer;
begin
  selectedNode := tvTemplates.Selected as TTemplateTreeNode;
  if not Assigned(selectedNode) then
    raise Exception.Create('Select Template to delete');
  templateName := selectedNode.Template.name;
  FOpenFile.DeleteTemplate(templateName);

  if selectedNode.NodeType = ntTemplateHeading then
    selectedNode.Delete
  else
  begin
    while ((selectedNode <> nil) and (selectedNode.NodeType <> ntTemplateHeading)) do
      selectedNode := selectedNode.Parent as TTemplateTreeNode;
    if selectedNode <> nil then
      selectedNode.Delete;
  end;

  i := cboTemplate.Items.IndexOf(templateName);
  if i <> -1 then
    cboTemplate.Items.Delete(i);
  if cboTemplate.Items.Count < 2 then
    CardPanel.Visible := false;
end;

procedure TDSpecCreatorForm.actDuplicateTemplateExecute(Sender : TObject);
var
  newTemplateName : string;
  sourceTemplate : ISpecTemplate;
  newTemplate : ISpecTemplate;
  templateNode : TTemplateTreeNode;
begin
  if not Assigned(tvTemplates.Selected) then
    Exit;

  sourceTemplate := (tvTemplates.Selected as TTemplateTreeNode).Template;
  newTemplateName := FOpenFile.GetNewTemplateName(sourceTemplate.name);

  newTemplate := sourceTemplate.Clone;
  newTemplate.name := newTemplateName;
  FOpenFile.Spec.Templates.Add(newTemplate);

  templateNode := LoadTemplate(newTemplate);
  tvTemplates.Selected := templateNode;

end;

procedure TDSpecCreatorForm.actFileExitExecute(Sender : TObject);
begin
  Close;
end;

procedure TDSpecCreatorForm.actFileNewExecute(Sender : TObject);
begin
  FreeAndNil(FOpenFile);
  FOpenFile := TDSpecFile.Create(FLogger);
  FOpenFile.Spec.newTemplate('default');
  UpdateFormCaption('');
  LoadDspecStructure;
end;

procedure TDSpecCreatorForm.actFileOpenExecute(Sender : TObject);
var
  dspecFilename : string;
begin
  if OpenDialog.Execute then
  begin
    dspecFilename := OpenDialog.filename;
    OpenProject(dspecFilename);
  end;
end;

procedure TDSpecCreatorForm.actFileSaveAsExecute(Sender : TObject);
begin
  if SaveDialog.Execute then
  begin
    SaveDspecStructure(SaveDialog.filename);
    MRUListService.Add(SaveDialog.filename);
  end;
end;

procedure TDSpecCreatorForm.actFileSaveExecute(Sender : TObject);
begin
  if FOpenFile.filename.IsEmpty then
  begin
    if SaveDialog.Execute then
    begin
      SaveDspecStructure(SaveDialog.filename);
    end;
  end
  else
  begin
    SaveDspecStructure(FOpenFile.filename);
  end;
end;

procedure TDSpecCreatorForm.ActionList1Update(Action : TBasicAction; var Handled : Boolean);
var
  selectedNode : TTemplateTreeNode;
  hasNode : Boolean;
begin
  selectedNode := tvTemplates.Selected as TTemplateTreeNode;
  hasNode := selectedNode <> nil;

  actDuplicateTemplate.Enabled := hasNode;
  actDeleteTemplate.Enabled := hasNode;

  actAddBuildItem.Enabled := hasNode and (selectedNode.IsBuildHeading or selectedNode.IsBuild);
  actDeleteBuildItem.Enabled := hasNode and selectedNode.IsBuild;

  actAddDependency.Enabled := hasNode and (selectedNode.IsDependencyHeading or selectedNode.IsDependency);
  actDeleteDependency.Enabled := hasNode and selectedNode.IsDependency;

  actAddDesignItem.Enabled := hasNode and (selectedNode.IsDesignHeading or selectedNode.IsDesign);
  actDeleteDesignItem.Enabled := hasNode and selectedNode.IsDesign;

  actAddFileItem.Enabled := hasNode and (selectedNode.IsFileEntryHeading or selectedNode.IsFileEntry);
  actDeleteFileItem.Enabled := hasNode and selectedNode.IsFileEntry;

  actAddLibItem.Enabled := hasNode and (selectedNode.IsLibEntryHeading or selectedNode.IsLibEntry);
  actDeleteLibItem.Enabled := hasNode and selectedNode.IsLibEntry;

  actAddRuntimeItem.Enabled := hasNode and (selectedNode.IsRuntimeHeading or selectedNode.IsRuntime);
  actDeleteRuntimeItem.Enabled := hasNode and selectedNode.IsRuntime;

  actAddDependency.Enabled := hasNode and (selectedNode.IsDependencyHeading or selectedNode.IsDependency);
  actDeleteDependency.Enabled := hasNode and selectedNode.IsDependency;

  actAddSearchPath.Enabled := hasNode and (selectedNode.IsSearchPathHeading or selectedNode.IsSearchPath);
  actDeleteSearchPath.Enabled := hasNode and selectedNode.IsSearchPath;

  actAddSourceItem.Enabled := hasNode and (selectedNode.IsSourceHeading or selectedNode.IsSearchPath);
  actDeleteSourceItem.Enabled := hasNode and selectedNode.IsSource;

end;

procedure TDSpecCreatorForm.actPlatformsDeselectAllExecute(Sender : TObject);
begin
  clbPlatforms.CheckAll(TCheckBoxState.cbUnchecked);
  clbPlatformsClickCheck(clbPlatforms);
end;

procedure TDSpecCreatorForm.actPlatformsSelectAllExecute(Sender : TObject);
var
  i : integer;
begin
  for i := 0 to clbPlatforms.Count - 1 do
  begin
    if clbPlatforms.ItemEnabled[i] then
      clbPlatforms.Checked[i] := true;
  end;
  clbPlatformsClickCheck(clbPlatforms);
end;

function TDSpecCreatorForm.LoadDesigntimeNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const item : ISpecBPLEntry) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, item.Source) as TTemplateTreeNode;
  result.bplEntry := item;
  result.NodeType := ntDesign;
  result.Template := template;
  result.ImageIndex := 6;
  result.SelectedIndex := 6;


end;

procedure TDSpecCreatorForm.LoadDesigntimeNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecBPLEntry>);
var
  designtimeNode : TTemplateTreeNode;
  j : integer;
begin
  designtimeNode := tvTemplates.Items.AddChild(parentNode, 'Design') as TTemplateTreeNode;
  designtimeNode.Template := template;
  designtimeNode.NodeType := ntDesignHeading;
  designtimeNode.ImageIndex := 6;
  designtimeNode.SelectedIndex := 6;

  designtimeNode.AddAction := actAddDesignItem;
  designtimeNode.DeleteAction := actDeleteDesignItem;

  for j := 0 to fileList.Count - 1 do
    LoadDesigntimeNode(designtimeNode, template, fileList[j]);
end;

function TDSpecCreatorForm.LoadRuntimeNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const item : ISpecBPLEntry) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, item.Source) as TTemplateTreeNode;
  result.bplEntry := item;
  result.NodeType := ntRuntime;
  result.Template := template;
  result.ImageIndex := 1;
  result.SelectedIndex := 1;
end;

procedure TDSpecCreatorForm.LoadRuntimeNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecBPLEntry>);
var
  runtimesNode : TTemplateTreeNode;
  j : integer;
begin
  runtimesNode := tvTemplates.Items.AddChild(parentNode, 'Runtime') as TTemplateTreeNode;
  runtimesNode.Template := template;
  runtimesNode.NodeType := ntRuntimeHeading;
  runtimesNode.ImageIndex := 1;
  runtimesNode.SelectedIndex := 1;

  runtimesNode.AddAction := actAddRuntimeItem;
  runtimesNode.DeleteAction := actDeleteRuntimeItem;

  for j := 0 to fileList.Count - 1 do
    LoadRuntimeNode(runtimesNode, template, fileList[j]);
end;

function TDSpecCreatorForm.LoadDependency(const parentNode : TTemplateTreeNode; template : ISpecTemplate; const dependency : ISpecDependency) : TTemplateTreeNode;
var
  sNode : string;
begin
  sNode := dependency.id + ' - ' + dependency.Version.ToString();
  result := tvTemplates.Items.AddChild(parentNode, sNode) as TTemplateTreeNode;
  result.dependency := dependency;
  result.Template := template;
  result.ImageIndex := 4;
  result.SelectedIndex := 4;
  result.NodeType := ntDependency;

end;

procedure TDSpecCreatorForm.LoadDependencies(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);
var
  nodeDependency : TTemplateTreeNode;
  j : integer;
begin
  nodeDependency := tvTemplates.Items.AddChild(parentNode, 'Dependencies') as TTemplateTreeNode;
  nodeDependency.Template := template;
  nodeDependency.ImageIndex := 4;
  nodeDependency.SelectedIndex := 4;
  nodeDependency.NodeType := ntDependencyHeading;

  nodeDependency.AddAction := actAddDependency;
  nodeDependency.DeleteAction := actDeleteDependency;

  for j := 0 to template.dependencies.Count - 1 do
    LoadDependency(nodeDependency, template, template.dependencies[j]);
end;

function TDSpecCreatorForm.LoadTemplate(const template : ISpecTemplate) : TTemplateTreeNode;
var
  Node : TTemplateTreeNode;
begin
  Node := AddRootTemplateNode(template);
  result := Node;

  LoadSourceNodes(Node, template, template.SourceFiles);
  LoadLibNodes(Node, template, template.LibFiles);
  LoadFileNodes(Node, template, template.Files);

  LoadSearchPathNodes(Node, template);

  LoadBuildNodes(Node, template);

  LoadRuntimeNodes(Node, template, template.RuntimeFiles);
  LoadDesigntimeNodes(Node, template, template.DesignFiles);

  LoadDependencies(Node, template);
  Node.Expand(true);

end;

procedure TDSpecCreatorForm.LoadTemplates;
var
  i : integer;
  template : ISpecTemplate;
  templateNode : TTemplateTreeNode;
begin
  templateNode := nil;
  tvTemplates.Items.BeginUpdate;
  try
    tvTemplates.Items.Clear;
    cboTemplate.Clear;
    cboTemplate.Items.Add(cNewTemplate);

    for i := 0 to FOpenFile.Spec.Templates.Count - 1 do
    begin
      template := FOpenFile.Spec.Templates[i];
      if i = 0 then
        templateNode := LoadTemplate(template)
      else
        LoadTemplate(template);
    end;
  finally
    tvTemplates.Items.EndUpdate;
  end;
  tvTemplates.Selected := templateNode;
end;

procedure TDSpecCreatorForm.edtIdChange(Sender : TObject);
begin
  FOpenFile.Spec.metadata.id := edtId.Text;
end;

procedure TDSpecCreatorForm.edtPackageOutputPathExit(Sender : TObject);
var
  iniFile : TIniFile;
begin
  // save path to ini file.
  iniFile := TIniFile.Create(MRUListService.GetIniFilePath);
  try
    iniFile.WriteString('Paths', 'PackageOutput', edtPackageOutputPath.Text);
  finally
    iniFile.Free;
  end;
end;

procedure TDSpecCreatorForm.edtProjectChange(Sender : TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).build.project := edtProject.Text;

    str := 'Possible Expanded Paths:' + System.sLineBreak;

    for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if compiler = TCompilerVersion.UnknownVersion then
        continue;
      str := str + System.sLineBreak + ReplaceVars(edtProject.Text, compiler);
    end;
    edtProject.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.edtProjectURLChange(Sender : TObject);
begin
  FOpenFile.Spec.metadata.projectUrl := edtProjectURL.Text;
end;

procedure TDSpecCreatorForm.edtRepositoryURLChange(Sender : TObject);
begin
  FOpenFile.Spec.metadata.repositoryUrl := edtRepositoryURL.Text;
end;

function TDSpecCreatorForm.ReplaceVars(const inputStr : String; compiler : TCompilerVersion) : string;
begin
  result := TClassReplacer.ReplaceVars(inputStr, compiler, FOpenFile.Spec);
end;

procedure TDSpecCreatorForm.edtBPLEntryBuildIdChange(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.BuildId := edtBPLEntryBuildId.Text;
end;

procedure TDSpecCreatorForm.edtBPLEntrySrcChange(Sender : TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.Source := edtBPLEntrySrc.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtBPLEntrySrc.Text;

    str := 'Possible Expanded Paths:' + System.sLineBreak;

    for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if compiler = TCompilerVersion.UnknownVersion then
        continue;
      str := str + System.sLineBreak + ReplaceVars(edtBPLEntrySrc.Text, compiler);
    end;
    edtBPLEntrySrc.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.edtSearchPathChange(Sender : TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).SearchPath.Path := edtSearchPath.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtSearchPath.Text
  end;
end;

procedure TDSpecCreatorForm.edtFileEntrySourceChange(Sender : TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).fileEntry.Source := edtFileEntrySource.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtFileEntrySource.Text;

    str := 'Possible Expanded Paths:' + System.sLineBreak;

    for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if compiler = TCompilerVersion.UnknownVersion then
        continue;
      str := str + System.sLineBreak + ReplaceVars(edtFileEntrySource.Text, compiler);
    end;
    edtFileEntrySource.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.edtTagsChange(Sender : TObject);
begin
  FOpenFile.Spec.metadata.tags := edtTags.Text;
end;

procedure TDSpecCreatorForm.edtTemplateNameChange(Sender : TObject);
var
  templateName : string;
  templateNode : TTemplateTreeNode;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    templateNode := (tvTemplates.Selected as TTemplateTreeNode);
    templateName := templateNode.Template.name;
    if SameText(templateName, edtTemplateName.Text) then
      Exit;

    templateNode.Text := edtTemplateName.Text;
    FOpenFile.Spec.RenameTemplate(templateName, edtTemplateName.Text);
  end;
end;

procedure TDSpecCreatorForm.edtVersionChange(Sender : TObject);
var
  Version : TPackageVersion;
begin
  if Length(edtVersion.Text) > 0 then
  begin
    if TPackageVersion.TryParse(edtVersion.Text, Version) then
      FOpenFile.Spec.metadata.Version := Version;
  end;
end;

procedure TDSpecCreatorForm.edtVersionExit(Sender : TObject);
begin
  if Length(edtVersion.Text) > 0 then
    FOpenFile.Spec.metadata.Version := TPackageVersion.Parse(edtVersion.Text);
end;

procedure TDSpecCreatorForm.FormCreate(Sender : TObject);
var
  idx : integer;
  iniFile : TIniFile;
begin
  FLogger := TDSpecLogger.Create(Memo2.Lines);
  FOpenFile := TDSpecFile.Create(FLogger);
  FOpenFile.Spec.newTemplate('default');
  FDosCommand := TDosCommand.Create(nil);
  FDosCommand.OnNewLine := DosCommandNewLine;
  FDosCommand.OnTerminated := DosCommandTerminated;
  FSPDXList := TStringList.Create;
  LoadSPDXList;
  LoadDspecStructure;
  FtmpFilename := '';
  PageControl.ActivePage := tsInfo;
  edtDependencyVersion.Text := '';
  UpdateFormCaption('');
  idx := mnuFile.IndexOf(mnuFileOpenSep);
  FMRUMenu := TMRUMenu.Create(Self);
  FMRUMenu.Caption := 'Open Recent Project';
  FMRUMenu.OnSelection := Self.MRUListClick;
  FMRUMenu.HidePathExtension := true;
  FMRUMenu.MaxItems := 10;
  mnuFile.Insert(idx, FMRUMenu);
  MRUListService.SetSource(Self);
  MRUListService.LoadMRU;
  FMRUMenu.Enabled := MRUListService.GetItemCount > 0;

  iniFile := TIniFile.Create(MRUListService.GetIniFilePath);
  try
    edtPackageOutputPath.Text := iniFile.ReadString('Paths', 'PackageOutput', '');

  finally
    iniFile.Free;
  end;

end;

procedure TDSpecCreatorForm.FormDestroy(Sender : TObject);
begin
  MRUListService.SetSource(nil);
  FSPDXList.Free;
  FreeAndNil(FDosCommand);
end;

procedure TDSpecCreatorForm.lblSPDXClick(Sender : TObject);
begin
  UriClick('https://spdx.org/licenses/');
end;

procedure TDSpecCreatorForm.LoadDspecStructure;
var
  i : integer;
  j : integer;
begin
  edtId.Text := FOpenFile.Spec.metadata.id;
  edtVersion.Text := FOpenFile.Spec.metadata.Version.ToString;
  mmoDescription.Text := FOpenFile.Spec.metadata.Description;
  edtProjectURL.Text := FOpenFile.Spec.metadata.projectUrl;
  edtRepositoryURL.Text := FOpenFile.Spec.metadata.repositoryUrl;
  edtAuthor.Text := FOpenFile.Spec.metadata.authors;
  edtCopyright.Text := FOpenFile.Spec.metadata.Copyright;
  cboLicense.Text := FOpenFile.Spec.metadata.license;
  edtTags.Text := FOpenFile.Spec.metadata.tags;
  if Length(FOpenFile.Spec.metadata.Icon) > 0 then
  begin
    ImgIcon.Picture.LoadFromFile(TPath.Combine(FOpenFile.WorkingDir, FOpenFile.Spec.metadata.Icon));
  end;

  cboTemplate.Text := '';

  CardPanel.Visible := false;
  for j := 0 to clbCompilers.Count - 1 do
  begin
    clbCompilers.Checked[j] := false;
  end;

  for i := 0 to FOpenFile.Spec.targetPlatforms.Count - 1 do
  begin
    j := clbCompilers.Items.IndexOf(CompilerToString(FOpenFile.Spec.targetPlatforms[i].compiler));
    if j >= 0 then
      clbCompilers.Checked[j] := j >= 0;
  end;

  LoadTemplates;
end;

procedure TDSpecCreatorForm.LoadSPDXList;
var
  Stream : TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, 'SPDX', RT_RCDATA);
  try
    FSPDXList.LoadFromStream(Stream);
    cboLicense.Items.Clear;
    for var i := 0 to FSPDXList.Count - 1 do
      cboLicense.Items.Add(FSPDXList.Names[i]);

  finally
    Stream.Free;
  end;

end;

procedure TDSpecCreatorForm.SaveDspecStructure(const filename : string);
begin
  FOpenFile.SaveToFile(filename);
  UpdateFormCaption(FOpenFile.filename);
end;

function TDSpecCreatorForm.SelectedPlatform : ISpecTargetPlatform;
begin
  result := nil;
  if clbPlatforms.ItemIndex < 0 then
    Exit;
  result := FOpenFile.GetPlatform(clbPlatforms.Items[clbPlatforms.ItemIndex]);
end;

procedure TDSpecCreatorForm.UpdateFormCaption(const value : string);
begin
  if value.IsEmpty then
    Caption := cToolName + ' - [Untitled]'
  else
    Caption := cToolName + ' - [' + value + ']';
end;

procedure TDSpecCreatorForm.UriClick(const uri : string);
begin
  ShellExecute(Application.Handle, 'open', PChar(uri), nil, nil, SW_SHOWNORMAL);
end;

procedure TDSpecCreatorForm.UriLabelClick(Sender : TObject);
var
  lbl : TLabel;
begin
  lbl := Sender as TLabel;
  if ((lbl <> nil) and lbl.Hint.StartsWith('https')) then
    UriClick(lbl.Hint);
end;

procedure TDSpecCreatorForm.UriLabelMouseEnter(Sender : TObject);
begin
  with (Sender As TLabel) do
  begin
    Font.Style := lblSPDX.Font.Style + [fsUnderline];
    // Font.Color := $00C57321;
    Enabled := true;
    Cursor := crHandPoint;
  end;

end;

procedure TDSpecCreatorForm.UriLabelMouseLeave(Sender : TObject);
begin
  with (Sender As TLabel) do
  begin
    Font.Style := lblSPDX.Font.Style - [fsUnderline];
    // Enabled := false;
    Cursor := crDefault;
  end;
end;

procedure TDSpecCreatorForm.EnableControls(value : Boolean);
begin
  lblPlatform.Enabled := value;
  clbPlatforms.Enabled := value;
  lblTemplate.Enabled := value;
  cboTemplate.Enabled := value;
  VariablesList.Enabled := value;
end;

procedure TDSpecCreatorForm.EnableDisablePlatform(compilerVersion : TCompilerVersion);
var
  DpmPlatforms : TDPMPlatforms;
  dpmPlatform : TDPMPlatform;
  platformString : string;
  i : integer;
begin
  DpmPlatforms := AllPlatforms(compilerVersion);

  for i := 0 to clbPlatforms.Count - 1 do
  begin
    platformString := clbPlatforms.Items[i];
    // if platformString.Equals('Linux') then
    // platformString := 'Linux64'
    // else if platformString.Equals('Andriod') then

    // else if platformString.Equals('IOS') then
    // platformString := 'iOS64';

    dpmPlatform := StringToDPMPlatform(platformString);
    clbPlatforms.ItemEnabled[i] := dpmPlatform in DpmPlatforms;
    clbPlatforms.Checked[i] := false;
  end;
end;

function TDSpecCreatorForm.FindHeadingNode(const templateNode : TTemplateTreeNode; nodeType : TNodeType) : TTemplateTreeNode;
var
  Node : TTemplateTreeNode;
begin
  result := nil;
  if templateNode = nil then
    raise Exception.Create('nil template node passed to FindHeadingNode');

  Node := templateNode.getFirstChild as TTemplateTreeNode;
  while (Node <> nil) and (result = nil) do
  begin
    if (Node.NodeType = nodeType) then
      result := Node;
    Node := Node.getNextSibling as TTemplateTreeNode;
  end;

end;

function TDSpecCreatorForm.FindTemplateNode(const template : ISpecTemplate) : TTemplateTreeNode;
var
  Node : TTemplateTreeNode;
begin
  result := nil;
  Node := tvTemplates.Items.GetFirstNode as TTemplateTreeNode;
  while (Node <> nil) and (result = nil) do
  begin
    if Node.Template = template then
      result := Node;
    Node := Node.getNextSibling as TTemplateTreeNode;
  end;

end;

procedure TDSpecCreatorForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
  MRUListService.SaveMRU;
end;

procedure TDSpecCreatorForm.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
var
  UserChoice : integer;
begin
  if FOpenFile.IsModified then
  begin
    UserChoice := MessageDlg('Do you want to save the changes?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case UserChoice of
      mrYes :
        begin
          // Call your save function here
          if not FOpenFile.filename.IsEmpty then
          begin
            SaveDspecStructure(FOpenFile.filename);
          end
          else
          begin
            if SaveDialog.Execute then
            begin
              SaveDspecStructure(SaveDialog.filename);
            end
            else
            begin
              CanClose := false;
              Exit;
            end;
          end;
          CanClose := true;
        end;
      mrNo :
        begin
          // Close without saving
          CanClose := true;
        end;
      mrCancel :
        begin
          // Do not close the form
          CanClose := false;
        end;
    end;
  end
  else
    CanClose := true; // No changes were made, so it's okay to close
end;

procedure TDSpecCreatorForm.ImgIconClick(Sender : TObject);
var
  relativePath : string;
begin
  if FOpenFile.filename.IsEmpty then
  begin
    ShowMessage('Save dspec file before adding the icon');
    Exit;
  end;

  if OpenPictureDialog1.Execute then
  begin
    ImgIcon.Picture.LoadFromFile(OpenPictureDialog1.filename);
    relativePath := ExtractRelativePath(FOpenFile.WorkingDir, OpenPictureDialog1.filename);
    FOpenFile.Spec.metadata.Icon := relativePath;
  end;
end;

procedure TDSpecCreatorForm.miOptionsClick(Sender : TObject);
var
  OptionsForm : TOptionsForm;
begin
  OptionsForm := TOptionsForm.Create(nil, FLogger);
  try
    OptionsForm.ShowModal;
  finally
    FreeAndNil(OptionsForm);
  end;
end;

procedure TDSpecCreatorForm.mmoDescriptionChange(Sender : TObject);
begin
  FOpenFile.Spec.metadata.Description := mmoDescription.Text;
end;

procedure TDSpecCreatorForm.MRUAdd(const filename : string);
begin
  FMRUMenu.Add(filename);
  FMRUMenu.Enabled := true;
end;

function TDSpecCreatorForm.MRUCount : integer;
begin
  result := Cardinal(FMRUMenu.Count);
end;

procedure TDSpecCreatorForm.MRUListClick(Sender : TObject; const filename : string);
begin
  if FileExists(filename) then
    OpenProject(filename)
  else
    MRURemove(filename);
end;

procedure TDSpecCreatorForm.MRULoad(const list : TStrings);
begin
  FMRUMenu.LoadFromList(list);
end;

function TDSpecCreatorForm.MRURemove(const filename : string) : Boolean;
begin
  FMRUMenu.Remove(filename);
  FMRUMenu.Enabled := MRUListService.GetItemCount > 0;
  result := true;
end;

procedure TDSpecCreatorForm.MRUSave(const list : TStrings);
begin
  FMRUMenu.SaveToList(list);
end;

procedure TDSpecCreatorForm.OpenProject(const filename : string);
var
  errorMessage : string;
begin
  if FOpenFile.LoadFromFile(filename, errorMessage) then
  begin
    UpdateFormCaption(filename);
    LoadDspecStructure;
    MRUListService.Add(filename);
  end
  else
  begin
    // restore back to default if we fail to open dspec
    FOpenFile := TDSpecFile.Create(FLogger);
    FOpenFile.Spec.newTemplate('default');
    ShowMessage(errorMessage);
    PageControl.ActivePage := tsLogging;
  end;

end;

procedure TDSpecCreatorForm.tvTemplatesChange(Sender : TObject; Node : TTreeNode);
var
  lNode : TTemplateTreeNode;
  i : integer;
begin
  lNode := Node as TTemplateTreeNode;

  case lNode.NodeType of
    ntTemplateHeading :
      begin
        edtTemplateName.Text := lNode.Template.name;
        CardPanel.ActiveCard := crdTemplate;
      end;
    ntBuildHeading  :      CardPanel.ActiveCard := crdBuildHeading;
    ntDesignHeading :      CardPanel.ActiveCard := crdDesignHeading;
    ntRuntimeHeading :     CardPanel.ActiveCard := crdRuntimeHeading;
    ntSourceHeading :      CardPanel.ActiveCard := crdSourceHeading;
    ntFileHeading :        CardPanel.ActiveCard := crdFileEntriesHeading;
    ntLibHeading :         CardPanel.ActiveCard := crdLibEntriesHeading;
    ntSeachPathHeading :   CardPanel.ActiveCard := crdSearchPathHeading;
    ntDependencyHeading :  CardPanel.ActiveCard := crdDependenciesHeading;
    ntBuild :
      begin
        edtBuildId.Text := lNode.build.id;
        edtProject.Text := lNode.build.project;
        edtConfiguration.Text := lNode.build.Config;
        chkBuildForDesign.Checked := lNode.build.buildForDesign;
        chkDesignOnly.Checked := lNode.build.designOnly;
        CardPanel.ActiveCard := crdBuild;
      end;
    ntDesign :
      begin
        lblRuntime.Caption := 'Design time Package (bpl)';
        edtBPLEntryBuildId.Text := lNode.bplEntry.buildId;
        edtBPLEntrySrc.Text := lNode.bplEntry.Source;
        chkCopyLocal.Checked := lNode.bplEntry.copyLocal;
        chkInstall.Checked := lNode.bplEntry.Install;
        CardPanel.ActiveCard := crdRuntimeOrDesignBpl;
      end;
    ntRuntime :
      begin
        lblRuntime.Caption := 'Runtime Package (bpl)';
        edtBPLEntryBuildId.Text := lNode.bplEntry.buildId;
        edtBPLEntrySrc.Text := lNode.bplEntry.Source;
        chkInstall.Checked := lNode.bplEntry.Install;
        chkCopyLocal.Checked := lNode.bplEntry.copyLocal;
        CardPanel.ActiveCard := crdRuntimeOrDesignBpl;
      end;
    ntSource, ntFile, ntLib :
      begin
        edtFileEntrySource.Text := lNode.fileEntry.Source;
        chkFileEntryFlatten.Checked := lNode.fileEntry.flatten;
        edtFileEntryDest.Text := lNode.fileEntry.Destination;
        lbFileEntryExclude.Clear;
        for i := 0 to lNode.fileEntry.exclude.Count - 1 do
          lbFileEntryExclude.Items.Add(lNode.fileEntry.exclude[i]);

        case lNode.NodeType of
          ntSource :  lblSourceItemHeader.Caption := 'Source Files';
          ntFile :    lblSourceItemHeader.Caption := 'Other Files';
          ntLib :     lblSourceItemHeader.Caption := 'Lib Files';
        end;

        CardPanel.ActiveCard := crdSource;
      end;
    ntSeachPath :
      begin
        edtSearchPath.Text := lNode.SearchPath.Path;
        CardPanel.ActiveCard := crdSearchPathItem;
      end;
    ntDependency :
      begin
        if not lNode.dependency.Version.IsEmpty then
          edtDependencyVersion.Text := lNode.dependency.Version.ToString
        else
          edtDependencyVersion.Text := '';
        edtDependencyId.Text := lNode.dependency.id;
        CardPanel.ActiveCard := crdDependency;
      end;
  else
    raise Exception.Create('Unknow node type in tvTemplateChange');
  end;
  CardPanel.Visible := true;
end;

procedure TDSpecCreatorForm.tvTemplatesCollapsing(Sender : TObject; Node : TTreeNode; var AllowCollapse : Boolean);
begin
  AllowCollapse := false;
end;

procedure TDSpecCreatorForm.tvTemplatesContextPopup(Sender : TObject; MousePos : TPoint; var Handled : Boolean);
var
  item : TMenuItem;
  localPos : TPoint;
  Node : TTemplateTreeNode;
  categoryNode : TTemplateTreeNode;
begin
  localPos := tvTemplates.ClientToScreen(MousePos);
  if Assigned(tvTemplates.Selected) then
  begin
    Node := tvTemplates.Selected as TTemplateTreeNode;
    if Node.TemplateHeading then
    begin
      Node.EditText;
      Handled := true;
    end;

    tvTemplates.PopupMenu.Items.Clear;
    Node := tvTemplates.GetNodeAt(MousePos.X, MousePos.Y) as TTemplateTreeNode;
    if Node = nil then
      Exit;
    tvTemplates.Selected := Node;
    FTemplate := nil;

    if Assigned(Node.Template) then
      FTemplate := Node.Template;

    categoryNode := Node.categoryNode;
    item := TMenuItem.Create(PopupMenu);

    item.Action := categoryNode.AddAction;

    tvTemplates.PopupMenu.Items.Add(item);

    item := TMenuItem.Create(PopupMenu);
    item.Action := categoryNode.DeleteAction;

    tvTemplates.PopupMenu.Items.Add(item);

    tvTemplates.PopupMenu.Popup(localPos.X, localPos.Y);

    Handled := true;
  end;
end;

procedure TDSpecCreatorForm.tvTemplatesCreateNodeClass(Sender : TCustomTreeView; var NodeClass : TTreeNodeClass);
begin
  NodeClass := TTemplateTreeNode;
end;

procedure TDSpecCreatorForm.tvTemplatesEdited(Sender : TObject; Node : TTreeNode; var S : string);
begin
  FOpenFile.Spec.RenameTemplate(Node.Text, S);
  edtTemplateName.Text := S;
end;

procedure TDSpecCreatorForm.tvTemplatesEditing(Sender : TObject; Node : TTreeNode; var AllowEdit : Boolean);
begin
  AllowEdit := (Node as TTemplateTreeNode).TemplateHeading;
end;

procedure TDSpecCreatorForm.VariablesListStringsChange(Sender : TObject);
var
  vPlatform : ISpecTargetPlatform;
begin
  vPlatform := FOpenFile.GetPlatform(clbCompilers.Items[clbCompilers.ItemIndex]);

  if clbCompilers.Checked[clbCompilers.ItemIndex] and not Assigned(vPlatform) then
  begin
    vPlatform := FOpenFile.AddCompiler(clbCompilers.Items[clbCompilers.ItemIndex]);
  end;
  if FInVariableUpdate then
    Exit;
  if Assigned(vPlatform) then
  begin
    vPlatform.Variables.Clear;
    vPlatform.Variables.AddStrings(VariablesList.Strings);
  end;
end;

end.
