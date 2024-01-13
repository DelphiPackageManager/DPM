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
  Vcl.ValEdit,
  System.RegularExpressions,
  Spring.Collections,
  DosCommand,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Creator.TemplateTreeNode,
  DPM.Creator.Dspec.FileHandler, System.Actions, Vcl.ActnList
  ;

type
  TDSpecCreatorForm = class(TForm)
    PageControl: TPageControl;
    tsInfo: TTabSheet;
    edtId: TEdit;
    lblId: TLabel;
    lblVersion: TLabel;
    edtVersion: TEdit;
    mmoDescription: TMemo;
    lblDescription: TLabel;
    lblProjectURL: TLabel;
    edtProjectURL: TEdit;
    lblRepositoryURL: TLabel;
    edtRepositoryURL: TEdit;
    lblLicense: TLabel;
    cboLicense: TComboBox;
    tsPlatforms: TTabSheet;
    lblTags: TLabel;
    edtTags: TEdit;
    clbCompilers: TCheckListBox;
    tsTemplates: TTabSheet;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miPrint: TMenuItem;
    miPrintSetup: TMenuItem;
    miExit: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    cboTemplate: TComboBox;
    clbPlatforms: TCheckListBox;
    lblTemplate: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnAddTemplate: TButton;
    btnDeleteTemplate: TButton;
    tvTemplates: TTreeView;
    CardPanel: TCardPanel;
    crdSource: TCard;
    crdSearchPaths: TCard;
    lblSrc: TLabel;
    edtFileEntrySource: TEdit;
    chkFileEntryFlatten: TCheckBox;
    lblDest: TLabel;
    edtFileEntryDest: TEdit;
    lbFileEntryExclude: TListBox;
    btnAddExclude: TButton;
    btnDeleteExclude: TButton;
    crdBuild: TCard;
    crdRuntimeOrDesignBpl: TCard;
    lblSearchPaths: TLabel;
    lblRuntime: TLabel;
    lblBuild: TLabel;
    lblBuildId: TLabel;
    edtBuildId: TEdit;
    lblProject: TLabel;
    edtProject: TEdit;
    lblRuntimeSrc: TLabel;
    edtBPLEntrySrc: TEdit;
    lblRuntimeBuildId: TLabel;
    edtBPLEntryBuildId: TEdit;
    chkCopyLocal: TCheckBox;
    PopupMenu: TPopupMenu;
    BalloonHint1: TBalloonHint;
    tsGenerate: TTabSheet;
    lblCompilers: TLabel;
    lblPlatform: TLabel;
    lblTemplateView: TLabel;
    edtSearchPath: TEdit;
    miOptions: TMenuItem;
    lblAuthor: TLabel;
    edtAuthor: TEdit;
    crdDependencies: TCard;
    Label1: TLabel;
    lblDependencyId: TLabel;
    edtDependencyId: TEdit;
    edtDependencyVersion: TEdit;
    ImageList1: TImageList;
    GridPanel1: TGridPanel;
    Panel1: TPanel;
    btnBuildPackages: TButton;
    Memo1: TMemo;
    edtPackageOutputPath: TEdit;
    Label2: TLabel;
    edtConfiguration: TEdit;
    lblConfiguration: TLabel;
    chkBuildForDesign: TCheckBox;
    chkDesignOnly: TCheckBox;
    btnDuplicateTemplate: TButton;
    crdTemplates: TCard;
    edtTemplateName: TEdit;
    lblTemplateName: TLabel;
    tsLogging: TTabSheet;
    Memo2: TMemo;
    chkInstall: TCheckBox;
    VariablesList: TValueListEditor;
    ActionList1: TActionList;
    actDeleteTemplate: TAction;
    actDuplicateTemplate: TAction;
    Label3: TLabel;
    lblSPDX: TLabel;
    Label4: TLabel;
    lblPackageId: TLabel;
    Label5: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure btnAddExcludeClick(Sender: TObject);
    procedure btnAddTemplateClick(Sender: TObject);
    procedure btnBuildPackagesClick(Sender: TObject);
    procedure btnDeleteExcludeClick(Sender: TObject);
    procedure cboLicenseChange(Sender: TObject);
    procedure cboTemplateChange(Sender: TObject);
    procedure chkBuildForDesignClick(Sender: TObject);
    procedure chkCopyLocalClick(Sender: TObject);
    procedure chkDesignInstallClick(Sender: TObject);
    procedure chkDesignOnlyClick(Sender: TObject);
    procedure clbCompilersClick(Sender: TObject);
    procedure clbCompilersClickCheck(Sender: TObject);
    procedure clbPlatformsClickCheck(Sender: TObject);
    procedure DosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
    procedure DosCommandTerminated(Sender: TObject);
    procedure edtAuthorChange(Sender: TObject);
    procedure edtBuildIdChange(Sender: TObject);
    procedure edtConfigurationChange(Sender: TObject);
    procedure edtDependencyIdChange(Sender: TObject);
    procedure edtDependencyVersionChange(Sender: TObject);
    procedure edtDesignBuildIdChange(Sender: TObject);
    procedure edtDesignSrcChange(Sender: TObject);
    procedure edtFileEntrySourceChange(Sender: TObject);
    procedure edtFileEntryDestChange(Sender: TObject);
    procedure edtIdChange(Sender: TObject);
    procedure edtProjectChange(Sender: TObject);
    procedure edtProjectURLChange(Sender: TObject);
    procedure edtRepositoryURLChange(Sender: TObject);
    procedure edtBPLEntryBuildIdOnChange(Sender: TObject);
    procedure edtBPLEntrySrcChange(Sender: TObject);
    procedure edtSearchPathChange(Sender: TObject);
    procedure edtTagsChange(Sender: TObject);
    procedure edtTemplateNameChange(Sender: TObject);
    procedure edtVersionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mmoDescriptionChange(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure tvTemplatesChange(Sender: TObject; Node: TTreeNode);
    procedure tvTemplatesCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure tvTemplatesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tvTemplatesCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure PopupAddBuildItem(Sender: TObject);
    procedure PopupDeleteBuildItem(Sender: TObject);
    procedure PopupAddRuntimeItem(Sender: TObject);
    procedure PopupDeleteRuntimeItem(Sender: TObject);
    procedure PopupAddDesignItem(Sender: TObject);
    procedure PopupDeleteDesignItem(Sender: TObject);
    procedure PopupAddSourceItem(Sender: TObject);
    procedure PopupDeleteSourceItem(Sender: TObject);
    procedure PopupAddFileItem(Sender: TObject);
    procedure PopupDeleteFileItem(Sender: TObject);
    procedure PopupAddLibItem(Sender: TObject);
    procedure PopupDeleteLibItem(Sender: TObject);
    procedure PopupAddSearchPathItem(Sender: TObject);
    procedure PopupDeleteSearchPathItem(Sender: TObject);
    procedure PopupAddDependencyItem(Sender: TObject);
    procedure PopupDeleteDependencyItem(Sender: TObject);
    procedure tvTemplatesEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvTemplatesEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure VariablesListStringsChange(Sender: TObject);
    procedure actDuplicateTemplateExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure actDeleteTemplateExecute(Sender: TObject);
    procedure lblSPDXClick(Sender: TObject);
    procedure UriLabelClick(Sender: TObject);
    procedure UriLabelMouseEnter(Sender: TObject);
    procedure UriLabelMouseLeave(Sender: TObject);
  private
    { Private declarations }
    FtmpFilename : string;
    FOpenFile : TDSpecFile;
    FDosCommand : TDosCommand;
    FTemplate : ISpecTemplate;
    FSavefilename : string;
    FLogger: ILogger;
    FInVariableUpdate: Boolean;
    FSPDXList : TStringList;
  protected


    procedure LoadTemplate(const template: ISpecTemplate);
    procedure LoadTemplates;
    procedure EnableDisablePlatform(compilerVersion : TCompilerVersion);
    function ReplaceVars(inputStr: String; compiler: TCompilerVersion): string;
    function AddRootTemplateNode(template: ISpecTemplate): TTemplateTreeNode;
    procedure AddFileEntryNode(node: TTemplateTreeNode; template: ISpecTemplate; fileList: IList<ISpecFileEntry>; NodeName: string);
    procedure AddSearchPathNodes(node: TTemplateTreeNode; template: ISpecTemplate);
    procedure AddBuildNode(node: TTemplateTreeNode; template: ISpecTemplate);
    procedure AddBPLNode(node: TTemplateTreeNode; template: ISpecTemplate; fileList: IList<ISpecBPLEntry>; nodeName: string);
    procedure AddDependencyNode(node: TTemplateTreeNode; template: ISpecTemplate);
    procedure UpdatePlatformCheckListbox(vplatform: ISpecTargetPlatform; platformName: string; platformListboxName: string = '');

    procedure UriClick(const uri : string);

    procedure LoadSPDXList;
    procedure LoadDspecStructure;
    procedure SaveDspecStructure(const filename: string);

    function SelectedPlatform: ISpecTargetPlatform;

  public
    { Public declarations }
  end;

var
  DSpecCreatorForm: TDSpecCreatorForm;

implementation

{$R *.dfm}

uses
  System.UITypes,
  System.IOUtils,
  WinApi.ShellAPI,
  DPM.Core.Dependency.Version,
  DPM.Creator.TemplateForm,
  DPM.Creator.FileForm,
  DPM.Creator.BuildForm,
  DPM.Creator.RuntimeForm,
  DPM.Creator.SearchPathForm,
  DPM.Creator.OptionsForm,
  DPM.Creator.DependencyForm,
  DPM.Creator.Logger,
  DPM.Creator.Dspec.Replacer
  ;


const
  cToolName = 'DPM dspec Creator';
  cNewTemplate = 'Create New Template...';

procedure TDSpecCreatorForm.btnDeleteExcludeClick(Sender: TObject);
var
  exclude : string;
  itemToDelete: Integer;
  entry: ISpecFileEntry;
begin
  if lbFileEntryExclude.ItemIndex < 0 then
    Exit;

  if Assigned(tvTemplates.Selected) then
  begin
    entry := (tvTemplates.Selected as TTemplateTreeNode).FileEntry;
    if not Assigned(entry) then
      Exit;

    exclude := lbFileEntryExclude.Items[lbFileEntryExclude.ItemIndex];
    lbFileEntryExclude.DeleteSelected;
    itemToDelete := entry.Exclude.IndexOf(exclude);
    entry.Exclude.Delete(itemToDelete);
  end;
end;

procedure TDSpecCreatorForm.btnAddExcludeClick(Sender: TObject);
var
  src : string;
  entry: ISpecFileEntry;
begin
  Src := InputBox('Add Exclude','Exclude to Add','');

  if Assigned(tvTemplates.Selected) then
  begin
    entry := (tvTemplates.Selected as TTemplateTreeNode).FileEntry;
    if not Assigned(entry) then
      Exit;

    entry.Exclude.Add(src);
    lbFileEntryExclude.Items.Add(src);
  end;
end;

procedure TDSpecCreatorForm.btnAddTemplateClick(Sender: TObject);
var
  templateName : string;
  TemplateForm: TTemplateForm;
begin
  TemplateForm := TTemplateForm.Create(nil);
  try
    if not FOpenFile.DoesTemplateExist('default') then
      TemplateForm.edtTemplate.Text := 'default';
    if TemplateForm.ShowModal =  mrCancel then
      Exit;
    templateName := TemplateForm.edtTemplate.Text;
    if templateName.IsEmpty then
      Exit;
  finally
    FreeAndNil(TemplateForm);
  end;
  FOpenfile.spec.NewTemplate(templateName);
  LoadTemplates;
end;

procedure TDSpecCreatorForm.btnBuildPackagesClick(Sender: TObject);
var
  guid: TGUID;
begin
  guid := TGUID.NewGuid;
  FtmpFilename := FOpenFile.WorkingDir;
  FtmpFilename := TPath.Combine(FtmpFilename, guid.ToString);
  FtmpFilename := ChangeFileExt(FtmpFilename, '.dspec');
  TFile.WriteAllText(FtmpFilename, FOpenFile.AsString);
  if DirectoryExists(edtPackageOutputPath.Text) then
    FDosCommand.CommandLine := 'dpm pack ' + FtmpFilename + ' -o=' + edtPackageOutputPath.Text;
  FDosCommand.Execute;
end;


procedure TDSpecCreatorForm.DosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
begin
  if AOutputType = otEntireLine then
    Memo1.Lines.Add(ANewLine);
end;

procedure TDSpecCreatorForm.DosCommandTerminated(Sender: TObject);
begin
  TFile.Delete(FtmpFilename);
  FtmpFilename := '';
end;

procedure TDSpecCreatorForm.cboLicenseChange(Sender: TObject);
begin
  FOpenFile.spec.metadata.license := cboLicense.Text;
  var i := FSPDXList.IndexOfName(cboLicense.Text);
  if i <> -1 then
  begin
    var data := FSPDXList.ValueFromIndex[i];
    var items := data.Split([',']);
    if Length(items) <> 2 then //just in case the txt file is messed up.
    begin
      lblSPDX.Visible := false;
      exit;
    end;

    lblSPDX.Caption := items[0];
    lblSPDX.Hint := items[1];
    lblSPDX.Visible := true;
  end
  else
  begin
    lblSPDX.Visible := false;

  end;
end;

procedure TDSpecCreatorForm.cboTemplateChange(Sender: TObject);
var
  templateName: string;
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
  vPlatform := FOpenfile.GetPlatform(clbCompilers.Items[clbCompilers.ItemIndex]);

  if not Assigned(vPlatform) then
  begin
    vPlatform := FOpenFile.AddCompiler(clbCompilers.Items[clbCompilers.ItemIndex]);
  end;
  vPlatform.TemplateName := templateName;
  cboTemplate.ItemIndex := cboTemplate.Items.IndexOf(templateName);
end;

procedure TDSpecCreatorForm.chkBuildForDesignClick(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).build.buildForDesign := chkBuildForDesign.Checked;
  end;
end;

procedure TDSpecCreatorForm.chkCopyLocalClick(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.copyLocal := chkCopyLocal.Checked;
  end;
end;

procedure TDSpecCreatorForm.chkDesignInstallClick(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.Install := chkInstall.Checked;
  end;
end;

procedure TDSpecCreatorForm.chkDesignOnlyClick(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).build.designOnly := chkDesignOnly.Checked;
  end;
end;

procedure TDSpecCreatorForm.clbCompilersClick(Sender: TObject);
var
  j : Integer;
  vplatform : ISpecTargetPlatform;
  compilerVersion : TCompilerVersion;
begin
  FInVariableUpdate := True;
  try
    VariablesList.Strings.Clear;
  finally
    FInVariableUpdate := False;
  end;
  if clbCompilers.ItemIndex < 0 then
  begin
    cboTemplate.ItemIndex := -1;
    Exit;
  end;

  vplatform := FOpenFile.GetPlatform(clbCompilers.Items[clbCompilers.ItemIndex]);
  compilerVersion := StringToCompilerVersion(clbCompilers.Items[clbCompilers.ItemIndex]);

  EnableDisablePlatform(compilerVersion);

  if clbCompilers.Checked[clbCompilers.ItemIndex] and not Assigned(vplatform) then
  begin
    vplatform := FOpenFile.AddCompiler(clbCompilers.Items[clbCompilers.ItemIndex]);
  end;

  if Assigned(vplatform) then
  begin
    for j := 0 to clbPlatforms.Count - 1 do
    begin
      clbPlatforms.Checked[j] := False;
    end;
    cboTemplate.ItemIndex := cboTemplate.Items.IndexOf(vplatform.TemplateName);
  end;

  if not Assigned(vplatform) then
  begin
    cboTemplate.ItemIndex := -1;
    Exit;
  end;

  UpdatePlatformCheckListbox(vplatform, 'Win32');
  UpdatePlatformCheckListbox(vplatform, 'Win64');
  UpdatePlatformCheckListbox(vplatform, 'Android32', 'Android');
  UpdatePlatformCheckListbox(vplatform, 'Android');
  UpdatePlatformCheckListbox(vplatform, 'Android64');
  UpdatePlatformCheckListbox(vplatform, 'LinuxIntel64', 'Linux');
  UpdatePlatformCheckListbox(vplatform, 'iOS64');
  UpdatePlatformCheckListbox(vplatform, 'OSX64');

  FInVariableUpdate := True;
  try
    VariablesList.Strings.Clear;
    VariablesList.Strings.AddStrings(vplatform.Variables);
  finally
    FInVariableUpdate := False;
  end;

  cboTemplate.Clear;
  LoadTemplates;

  cboTemplate.ItemIndex := cboTemplate.Items.IndexOf(vplatform.TemplateName);
end;

procedure TDSpecCreatorForm.clbCompilersClickCheck(Sender: TObject);
var
  vPlatform : ISpecTargetPlatform;
  compiler : string;
begin
  if clbCompilers.ItemIndex < 0 then
    Exit;
  compiler := clbCompilers.Items[clbCompilers.ItemIndex];
  vPlatform := FOpenfile.GetPlatform(compiler);
  if clbCompilers.Checked[clbCompilers.ItemIndex] and not Assigned(vPlatform) then
  begin
    vPlatform := FOpenFile.AddCompiler(compiler);
  end
  else if Assigned(vPlatform) and (clbCompilers.Checked[clbCompilers.ItemIndex] = False) then
  begin
    FOpenFile.DeleteCompiler(compiler);
  end;
end;

procedure TDSpecCreatorForm.clbPlatformsClickCheck(Sender: TObject);
var
  vPlatform : ISpecTargetPlatform;
  compiler : string;
  platformString : string;
  dpmPlatform: TDPMPlatform;
  i, j: Integer;
  platformCount: Integer;
  newPlatforms: TArray<TDPMPlatform>;
begin
  if clbCompilers.ItemIndex < 0 then
  begin
    raise Exception.Create('You must select a compiler before you can select platforms');
  end;
  if clbPlatforms.ItemIndex < 0 then
    Exit;

  compiler := clbCompilers.Items[clbCompilers.ItemIndex];
  vPlatform := FOpenfile.GetPlatform(compiler);
  platformCount := 0;
  for i := 0 to clbPlatforms.Count - 1 do
  begin
    if clbPlatforms.Checked[i] then
    begin
      platformString := clbPlatforms.Items[i];
      if platformString = 'Linux' then
        platformString := 'Linux64';
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
    if platformString = 'Linux' then
      platformString := 'Linux64';
    dpmPlatform := StringToDPMPlatform(platformString);

    if dpmPlatform = TDPMPlatform.UnknownPlatform then
      continue;
    newPlatforms[j] := dpmPlatform;
    Inc(j);
  end;

  vPlatform.platforms := newPlatforms;
end;

procedure TDSpecCreatorForm.edtAuthorChange(Sender: TObject);
begin
  FOpenFile.spec.metadata.authors := edtAuthor.Text;
end;

procedure TDSpecCreatorForm.edtBuildIdChange(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).build.id := edtBuildId.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtBuildId.Text;
  end;
end;

procedure TDSpecCreatorForm.edtConfigurationChange(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).build.Config := edtConfiguration.Text;
  end;
end;

procedure TDSpecCreatorForm.edtDependencyIdChange(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).dependency.id := edtDependencyId.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtDependencyId.Text + ' - ' + edtDependencyVersion.Text;
  end;
end;

procedure TDSpecCreatorForm.edtDependencyVersionChange(Sender: TObject);
var
  ver : TVersionRange;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    if length(edtDependencyVersion.Text) > 0 then
    begin
      if TVersionRange.TryParse(edtDependencyVersion.Text, ver) then
      begin
        (tvTemplates.Selected as TTemplateTreeNode).dependency.version := ver;
        (tvTemplates.Selected as TTemplateTreeNode).Text := edtDependencyId.Text + ' - ' + edtDependencyVersion.Text;
      end;
    end;
  end;
end;

procedure TDSpecCreatorForm.edtDesignSrcChange(Sender: TObject);
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
      str := str  + System.sLineBreak + ReplaceVars(edtBPLEntrySrc.Text, compiler);
    end;
    edtBPLEntrySrc.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.edtDesignBuildIdChange(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.buildId := edtBPLEntryBuildId.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtBPLEntryBuildId.Text;
  end;
end;

procedure TDSpecCreatorForm.edtFileEntryDestChange(Sender: TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).FileEntry.Destination := edtFileEntryDest.Text;

    str := 'Possible Expanded Paths:' + System.sLineBreak;

    for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if compiler = TCompilerVersion.UnknownVersion then
        continue;
      str := str  + System.sLineBreak + ReplaceVars(edtFileEntryDest.Text, compiler);
    end;
    edtFileEntryDest.Hint := str;
  end;
end;

function TDSpecCreatorForm.AddRootTemplateNode(template: ISpecTemplate): TTemplateTreeNode;
begin
  cboTemplate.Items.Insert(cboTemplate.Items.Count -1,template.name);
  Result := tvTemplates.Items.Add(nil, template.name) as TTemplateTreeNode;
  Result.NodeType := ntTemplateHeading;
  Result.Template := template;
  Result.ImageIndex := 5;
  Result.SelectedIndex := 5;
  Result.TemplateHeading := True;
end;

procedure TDSpecCreatorForm.AddFileEntryNode(node: TTemplateTreeNode; template: ISpecTemplate; fileList: IList<ISpecFileEntry>; NodeName: string);
var
  nodeSource: TTemplateTreeNode;
  sourceNode: TTemplateTreeNode;
  j: Integer;
  iconIndex : Integer;
  lNodeType : TNodeType;
  lOnNewText : string;
  lOnDeleteText : string;
  lOnNewClick : TNotifyEvent;
  lOnDeleteClick : TNotifyEvent;
begin
  lOnNewClick := nil;
  lOnDeleteClick := nil;
  if NodeName = 'Source' then
  begin
    iconIndex := 2;
    lNodeType := ntSourceHeading;
    lOnNewText := 'Add Source Item';
    lOnNewClick := PopupAddSourceItem;
    lOnDeleteText := 'Delete Source Item';
    lOnDeleteClick := PopupDeleteSourceItem;
  end
  else if NodeName = 'Files' then
  begin
    iconIndex := 2;
    lNodeType := ntFileHeading;
    lOnNewText := 'Add File Item';
    lOnNewClick := PopupAddFileItem;
    lOnDeleteText := 'Delete File Item';
    lOnDeleteClick := PopupDeleteFileItem;
  end
  else if NodeName = 'Lib' then
  begin
    iconIndex := 2;
    lNodeType := ntLibHeading;
    lOnNewText := 'Add Library Item';
    lOnNewClick := PopupAddLibItem;
    lOnDeleteText := 'Delete Library Item';
    lOnDeleteClick := PopupDeleteLibItem;
  end
  else
    raise Exception.Create('nodeName must be either Source, Files or Lib. Was [' + NodeName + ']');

  nodeSource := tvTemplates.Items.AddChild(node, NodeName) as TTemplateTreeNode;
  nodeSource.Template := template;
  nodeSource.NodeType := lNodeType;
  nodeSource.ImageIndex := iconIndex;
  nodeSource.SelectedIndex := iconIndex;
  nodeSource.OnNewText := lOnNewText;
  nodeSource.OnNewClick := lOnNewClick;
  nodeSource.OnDeleteText := lOnDeleteText;
  nodeSource.OnDeleteClick := lOnDeleteClick;
  for j := 0 to fileList.Count - 1 do
  begin
    sourceNode := tvTemplates.Items.AddChild(nodeSource, fileList[j].Source) as TTemplateTreeNode;
    sourceNode.fileEntry := fileList[j];
    if NodeName = 'Source' then
    begin
      sourceNode.NodeType := ntSource;
    end
    else if NodeName = 'Files' then
    begin
      sourceNode.NodeType := ntFile;
    end
    else if NodeName = 'Lib' then
    begin
      sourceNode.NodeType := ntLib;
    end;
    sourceNode.Template := template;
    sourceNode.ImageIndex := iconIndex;
    sourceNode.SelectedIndex := iconIndex;
  end;
end;

procedure TDSpecCreatorForm.AddSearchPathNodes(node: TTemplateTreeNode; template: ISpecTemplate);
var
  nodeSearchPath: TTemplateTreeNode;
  searchPathNode: TTemplateTreeNode;
  j: Integer;
begin
  nodeSearchPath := tvTemplates.Items.AddChild(node, 'SearchPaths') as TTemplateTreeNode;
  nodeSearchPath.Template := template;
  nodeSearchPath.ImageIndex := 3;
  nodeSearchPath.SelectedIndex := 3;
  nodeSearchPath.NodeType := ntSeachPathHeading;
  nodeSearchPath.OnNewText := 'Add SearchPath Item';
  nodeSearchPath.OnNewClick := PopupAddSearchPathItem;
  nodeSearchPath.OnDeleteText := 'Delete SearchPath Item';
  nodeSearchPath.OnDeleteClick := PopupDeleteSearchPathItem;

  for j := 0 to template.searchPaths.Count - 1 do
  begin
    searchPathNode := tvTemplates.Items.AddChild(nodeSearchPath, template.searchPaths[j].path) as TTemplateTreeNode;
    searchPathNode.searchpath := template.searchPaths[j];
    searchPathNode.Template := template;
    searchPathNode.NodeType := ntSeachPath;
    searchPathNode.ImageIndex := 3;
    searchPathNode.SelectedIndex := 3;
  end;
end;

procedure TDSpecCreatorForm.AddBuildNode(node: TTemplateTreeNode; template: ISpecTemplate);
var
  nodeBuild: TTemplateTreeNode;
  buildNode: TTemplateTreeNode;
  j: Integer;
begin
  nodeBuild := tvTemplates.Items.AddChild(node, 'Build') as TTemplateTreeNode;
  nodeBuild.Template := template;
  nodeBuild.ImageIndex := 0;
  nodeBuild.SelectedIndex := 0;
  nodeBuild.NodeType := ntBuildHeading;
  nodeBuild.OnNewText := 'Add Build Item';
  nodeBuild.OnNewClick := PopupAddBuildItem;
  nodeBuild.OnDeleteText := 'Delete Build Item';
  nodeBuild.OnDeleteClick := PopupDeleteBuildItem;
  for j := 0 to template.BuildEntries.Count - 1 do
  begin
    buildNode := tvTemplates.Items.AddChild(nodeBuild, template.BuildEntries[j].id) as TTemplateTreeNode;
    buildNode.build := template.BuildEntries[j];
    buildNode.Template := template;
    buildNode.ImageIndex := 0;
    buildNode.SelectedIndex := 0;
    buildNode.NodeType := ntBuild;
  end;
end;


procedure TDSpecCreatorForm.actDeleteTemplateExecute(Sender: TObject);
var
  templateName: string;
  selectedNode : TTemplateTreeNode;
  i : integer;
begin
  selectedNode := tvTemplates.Selected as TTemplateTreeNode;
  if not Assigned(selectedNode) then
    raise Exception.Create('Select Template to delete');
  templateName := selectedNode.Template.Name;
  FOpenFile.DeleteTemplate(templateName);

  if selectedNode.NodeType = ntTemplateHeading then
    selectedNode.Delete
  else
  begin
    while ((selectedNode <> nil) and ( selectedNode.NodeType <> ntTemplateHeading)) do
      selectedNode := selectedNode.Parent as TTemplateTreeNode;
    if selectedNode <> nil then
      selectedNode.Delete;
  end;

  i := cboTemplate.Items.IndexOf(templateName);
  if i <> -1 then
    cboTemplate.Items.Delete(i);
end;

procedure TDSpecCreatorForm.actDuplicateTemplateExecute(Sender: TObject);
var
  newTemplateName : string;
  sourceTemplate : ISpecTemplate;
  newTemplate : ISpecTemplate;
begin
  if not Assigned(tvTemplates.Selected) then
    Exit;

  sourceTemplate := (tvTemplates.Selected as TTemplateTreeNode).Template;
  newTemplateName := FOpenFile.GetNewTemplateName(sourceTemplate.name);
  newTemplate := sourceTemplate.Clone;
  newTemplate.Name := newTemplateName;

  LoadTemplate(newTemplate);
end;

procedure TDSpecCreatorForm.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actDuplicateTemplate.Enabled := tvTemplates.Selected <> nil;
  actDeleteTemplate.Enabled := tvTemplates.Selected <> nil;
end;

procedure TDSpecCreatorForm.AddBPLNode(node: TTemplateTreeNode; template: ISpecTemplate; fileList: IList<ISpecBPLEntry>; nodeName: string);
var
  nodeDesign: TTemplateTreeNode;
  designNode: TTemplateTreeNode;
  j: Integer;
  iconIndex : Integer;
  lNodeType : TNodeType;
  lOnNewText : string;
  lOnDeleteText : string;
  lOnNewClick : TNotifyEvent;
  lOnDeleteClick : TNotifyEvent;
begin
  if nodeName = 'Design' then
  begin
    iconIndex := 6;
    lNodeType := ntDesignHeading;
    lOnNewText := 'Add Design Item';
    lOnNewClick := PopupAddDesignItem;
    lOnDeleteText := 'Delete Design Item';
    lOnDeleteClick := PopupDeleteDesignItem;
  end
  else if nodeName = 'Runtime'  then
  begin
    iconIndex := 1;
    lNodeType := ntRuntimeHeading;
    lOnNewText := 'Add Runtime Item';
    lOnNewClick := PopupAddRuntimeItem;
    lOnDeleteText := 'Delete Runtime Item';
    lOnDeleteClick := PopupDeleteRuntimeItem;
  end
  else
    raise Exception.Create('Nodetype must be either Design or Runtime - was [' + nodeName + ']');


  nodeDesign := tvTemplates.Items.AddChild(node, nodeName) as TTemplateTreeNode;
  nodeDesign.Template := template;
  nodeDesign.NodeType := lNodeType;
  nodeDesign.ImageIndex := iconIndex;
  nodeDesign.SelectedIndex := iconIndex;
  nodeDesign.OnNewText := lOnNewText;
  nodeDesign.OnNewClick := lOnNewClick;
  nodeDesign.OnDeleteText := lOnDeleteText;
  nodeDesign.OnDeleteClick := lOnDeleteClick;
  for j := 0 to fileList.Count - 1 do
  begin
    designNode := tvTemplates.Items.AddChild(nodeDesign, fileList[j].BuildId) as TTemplateTreeNode;
    designNode.bplEntry := fileList[j];
    if nodeName = 'Design' then
    begin
      designNode.NodeType := ntDesign;
    end
    else if nodeName = 'Runtime'  then
    begin
      designNode.NodeType := ntRuntime;
    end;
    designNode.Template := template;
    designNode.ImageIndex := iconIndex;
    designNode.SelectedIndex := iconIndex;
  end;
end;

procedure TDSpecCreatorForm.AddDependencyNode(node: TTemplateTreeNode; template: ISpecTemplate);
var
  nodeDependency: TTemplateTreeNode;
  dependencyNode: TTemplateTreeNode;
  j: Integer;
begin
  nodeDependency := tvTemplates.Items.AddChild(node, 'Dependencies') as TTemplateTreeNode;
  nodeDependency.Template := template;
  nodeDependency.ImageIndex := 4;
  nodeDependency.SelectedIndex := 4;
  nodeDependency.NodeType := ntDependencyHeading;
  nodeDependency.OnNewText := 'Add Dependency Item';
  nodeDependency.OnNewClick := PopupAddDependencyItem;
  nodeDependency.OnDeleteText := 'Delete Dependency Item';
  nodeDependency.OnDeleteClick := PopupDeleteDependencyItem;

  for j := 0 to template.dependencies.Count - 1 do
  begin
    dependencyNode := tvTemplates.Items.AddChild(nodeDependency, template.dependencies[j].id) as TTemplateTreeNode;
    dependencyNode.dependency := template.dependencies[j];
    dependencyNode.Template := template;
    dependencyNode.ImageIndex := 4;
    dependencyNode.SelectedIndex := 4;
    dependencyNode.NodeType := ntDependency;
  end;
end;

procedure TDSpecCreatorForm.LoadTemplate(const template: ISpecTemplate);
var
  node: TTemplateTreeNode;
begin
    node := AddRootTemplateNode(template);

    AddFileEntryNode(node, template, template.SourceFiles, 'Source');
    AddFileEntryNode(node, template, template.Files, 'Files');
    AddFileEntryNode(node, template, template.LibFiles, 'Lib');
    AddSearchPathNodes(node, template);
    AddBuildNode(node, template);
    AddBPLNode(node, template, template.DesignFiles, 'Design');
    AddBPLNode(node, template, template.RuntimeFiles, 'Runtime');
    AddDependencyNode(node, template);
    node.Expand(True);

end;

procedure TDSpecCreatorForm.LoadTemplates;
var
  i : Integer;
  template: ISpecTemplate;
begin
  tvTemplates.Items.Clear;
  cboTemplate.Clear;
  cboTemplate.Items.Add(cNewTemplate);

  for i := 0 to FOpenFile.spec.templates.Count - 1 do
  begin
    template := FOpenFile.spec.templates[i];
    LoadTemplate(template);
  end;

end;

procedure TDSpecCreatorForm.edtIdChange(Sender: TObject);
begin
  FOpenFile.spec.metadata.id := edtId.Text;
end;

procedure TDSpecCreatorForm.edtProjectChange(Sender: TObject);
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
      str := str  + System.sLineBreak + ReplaceVars(edtProject.Text, compiler);
    end;
    edtProject.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.edtProjectURLChange(Sender: TObject);
begin
  FOpenFile.spec.metadata.projectUrl := edtProjectURL.Text;
end;

procedure TDSpecCreatorForm.edtRepositoryURLChange(Sender: TObject);
begin
  FOpenFile.spec.metadata.repositoryUrl := edtRepositoryURL.Text;
end;

procedure TDSpecCreatorForm.edtBPLEntryBuildIdOnChange(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).bplEntry.Source := edtBPLEntrySrc.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtBPLEntryBuildId.Text;
  end;
end;

function TDSpecCreatorForm.ReplaceVars(inputStr: String; compiler: TCompilerVersion): string;
begin
  Result := TClassReplacer.ReplaceVars(inputStr, compiler, FOpenFile.spec);
end;

procedure TDSpecCreatorForm.edtBPLEntrySrcChange(Sender: TObject);
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
      str := str  + System.sLineBreak + ReplaceVars(edtBPLEntrySrc.Text, compiler);
    end;
    edtBPLEntrySrc.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.edtSearchPathChange(Sender: TObject);
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).searchpath.path:= edtSearchPath.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtSearchPath.Text
  end;
end;

procedure TDSpecCreatorForm.edtFileEntrySourceChange(Sender: TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    (tvTemplates.Selected as TTemplateTreeNode).FileEntry.Source := edtFileEntrySource.Text;
    (tvTemplates.Selected as TTemplateTreeNode).Text := edtFileEntrySource.Text;


    str := 'Possible Expanded Paths:' + System.sLineBreak;

    for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if compiler = TCompilerVersion.UnknownVersion then
        continue;
      str := str  + System.sLineBreak + ReplaceVars(edtFileEntrySource.Text, compiler);
    end;
    edtFileEntrySource.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.edtTagsChange(Sender: TObject);
begin
  FOpenFile.spec.metadata.tags := edtTags.Text;
end;

procedure TDSpecCreatorForm.edtTemplateNameChange(Sender: TObject);
var
 templateName : string;
 template : ISpecTemplate;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    template := (tvTemplates.Selected as TTemplateTreeNode).Template;
    templateName := Template.name;
    if SameText(templateName, edtTemplateName.Text) then
      exit;

    (tvTemplates.Selected as TTemplateTreeNode).Text := edtTemplateName.Text;
    FOpenFile.spec.RenameTemplate(templateName, edtTemplateName.Text);
  end;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.edtVersionChange(Sender: TObject);
begin
  if length(edtVersion.Text) > 0 then
    FOpenFile.spec.metadata.version := TPackageVersion.Parse(edtVersion.Text);
end;

procedure TDSpecCreatorForm.FormCreate(Sender: TObject);
begin
  FLogger := TDSpecLogger.Create(Memo2.Lines);
  FOpenFile := TDSpecFile.Create(FLogger);
  FDosCommand := TDosCommand.Create(nil);
  FDosCommand.OnNewLine := DosCommandNewLine;
  FDosCommand.OnTerminated := DosCommandTerminated;
  FSPDXList := TStringList.Create;
  LoadSPDXList;
  LoadDspecStructure;
  FSavefilename := '';
  FtmpFilename := '';
  PageControl.ActivePage := tsInfo;
  edtDependencyVersion.Text := '';
  Caption := 'Untitled - ' + cToolName;
end;

procedure TDSpecCreatorForm.FormDestroy(Sender: TObject);
begin
  FSPDXList.Free;
  FreeAndNil(FDosCommand);
end;

procedure TDSpecCreatorForm.lblSPDXClick(Sender: TObject);
begin
  UriClick('https://spdx.org/licenses/');
end;

procedure TDSpecCreatorForm.LoadDspecStructure;
var
  i : Integer;
  j: Integer;
begin
  edtId.Text := FOpenFile.spec.metadata.id;
  edtVersion.Text := FOpenFile.spec.MetaData.Version.ToString;
  mmoDescription.Text := FOpenFile.spec.MetaData.Description;
  edtProjectURL.Text := FOpenFile.spec.MetaData.ProjectUrl;
  edtRepositoryURL.Text := FOpenFile.spec.MetaData.RepositoryUrl;
  edtAuthor.Text := FOpenFile.spec.MetaData.Authors;
  cboLicense.Text := FOpenFile.spec.MetaData.License;
  edtTags.Text := FOpenFile.spec.MetaData.Tags;
  cboTemplate.Text := '';

  CardPanel.Visible := False;
  for j := 0 to clbCompilers.Count - 1 do
  begin
    clbCompilers.Checked[j] := False;
  end;

  for i := 0 to FOpenFile.spec.targetPlatforms.Count - 1 do
  begin
     j := clbCompilers.Items.IndexOf(CompilerToString(FOpenFile.spec.targetPlatforms[i].compiler));
     if j >= 0 then
       clbCompilers.Checked[j] := j >= 0;
  end;

  LoadTemplates;
end;

procedure TDSpecCreatorForm.LoadSPDXList;
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, 'SPDX', RT_RCDATA);
  try
    FSPDXList.LoadFromStream(Stream);
    cboLicense.Items.Clear;
    for var i := 0 to  FSPDXList.Count -1  do
      cboLicense.Items.Add(FSPDXList.Names[i]);

  finally
    Stream.Free;
  end;

end;

procedure TDSpecCreatorForm.SaveDspecStructure(const filename: string);
begin
  FOpenFile.SaveToFile(filename);
  FSavefilename := Filename;
  Caption := FSavefilename + ' - dspec Creator';
end;

function TDSpecCreatorForm.SelectedPlatform: ISpecTargetPlatform;
begin
  Result := nil;
  if clbPlatforms.ItemIndex < 0  then
    Exit;
  Result := FOpenFile.GetPlatform(clbPlatforms.Items[clbPlatforms.ItemIndex]);
end;

procedure TDSpecCreatorForm.UpdatePlatformCheckListbox(vplatform: ISpecTargetPlatform; platformName: string; platformListboxName: string);
var
  j: Integer;
begin
  if Length(platformListboxName) = 0 then
  begin
    platformListboxName := platformName;
  end;

  if vplatform.PlatformContains(platformName) then
  begin
    j := clbPlatforms.Items.IndexOf(platformListboxName);
    if j >= 0 then
      clbPlatforms.Checked[j] := j >= 0;
  end;
end;

procedure TDSpecCreatorForm.UriClick(const uri: string);
begin
  ShellExecute(Application.Handle, 'open', PChar(uri), nil, nil, SW_SHOWNORMAL);
end;

procedure TDSpecCreatorForm.UriLabelClick(Sender: TObject);
var
  lbl : TLabel;
begin
  lbl := Sender as TLabel;
  if ((lbl <> nil) and lbl.Hint.StartsWith('https'))  then
    UriClick(lbl.Hint);
end;

procedure TDSpecCreatorForm.UriLabelMouseEnter(Sender: TObject);
begin
  with (Sender As TLabel) do
  begin
    Font.Style := lblSPDX.Font.Style + [fsUnderline];
    Font.Color := $00C57321;
    Enabled := true;
    Cursor := crHandPoint;
  end;

end;

procedure TDSpecCreatorForm.UriLabelMouseLeave(Sender: TObject);
begin
  with (Sender As TLabel) do
  begin
    Font.Style := lblSPDX.Font.Style - [fsUnderline];
    Enabled := false;
    Cursor := crDefault;
  end;
end;

procedure TDSpecCreatorForm.EnableDisablePlatform(compilerVersion : TCompilerVersion);
var
  DpmPlatforms : TDPMPlatforms;
  DpmPlatform: TDPMPlatform;
  platformString : string;
  i: Integer;
begin
  DpmPlatforms := AllPlatforms(compilerVersion);

  for i := 0 to clbPlatforms.Count - 1 do
  begin
    platformString := clbPlatforms.Items[i];
    if platformString.Equals('Linux') then
      platformString := 'Linux64'
    else if platformString.Equals('IOS') then
      platformString := 'iOS64';

    DpmPlatform := StringToDPMPlatform(platformString);
    clbPlatforms.ItemEnabled[i] := DpmPlatform in DpmPlatforms;
    clbPlatforms.Checked[i] := False;
  end;
end;

procedure TDSpecCreatorForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TDSpecCreatorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  UserChoice: Integer;
begin
  if FOpenFile.IsModified then
  begin
    UserChoice := MessageDlg('Do you want to save the changes?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case UserChoice of
      mrYes:
        begin
          // Call your save function here
          if not FSavefilename.IsEmpty then
          begin
            SaveDspecStructure(FSavefilename);
          end
          else
          begin
            if SaveDialog.Execute then
            begin
              SaveDspecStructure(SaveDialog.FileName);
            end
            else
            begin
              CanClose := False;
              Exit;
            end;
          end;
          CanClose := True;
        end;
      mrNo:
        begin
          // Close without saving
          CanClose := True;
        end;
      mrCancel:
        begin
          // Do not close the form
          CanClose := False;
        end;
    end;
  end
  else
    CanClose := True; // No changes were made, so it's okay to close
end;

procedure TDSpecCreatorForm.miNewClick(Sender: TObject);
begin
  FreeAndNil(FOpenFile);
  FOpenFile := TDSpecFile.Create(FLogger);
  FSavefilename := '';
  Caption := 'Untitled - dspec Creator';
  LoadDspecStructure;
end;

procedure TDSpecCreatorForm.miOpenClick(Sender: TObject);
var
  dspecFilename : string;
begin
  if OpenDialog.Execute then
  begin
    dspecFilename := OpenDialog.FileName;
    FOpenFile.LoadFromFile( dspecFilename);
    FSavefilename := dspecFilename;
    Caption := dspecFilename + ' - ' + cToolName;
    LoadDspecStructure;
  end;
end;

procedure TDSpecCreatorForm.miOptionsClick(Sender: TObject);
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

procedure TDSpecCreatorForm.miSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    SaveDspecStructure(SaveDialog.Filename);
  end;
end;

procedure TDSpecCreatorForm.miSaveClick(Sender: TObject);
begin
  if FSavefilename.IsEmpty then
  begin
    if SaveDialog.Execute then
    begin
      SaveDspecStructure(SaveDialog.Filename);
    end;
  end
  else
  begin
    SaveDspecStructure(FSavefilename);
  end;
end;


procedure TDSpecCreatorForm.mmoDescriptionChange(Sender: TObject);
begin
  FOpenFile.spec.metadata.description := mmoDescription.Text;
end;

procedure TDSpecCreatorForm.PopupAddBuildItem(Sender: TObject);
var
  buildId : string;
  BuildForm: TBuildForm;
  build : ISpecBuildEntry;
begin
  BuildForm := TBuildForm.Create(nil);
  try
    BuildForm.edtBuildId.Text := 'default';

    if BuildForm.ShowModal =  mrCancel then
      Exit;
    buildId := BuildForm.edtBuildId.Text;
    if buildId.IsEmpty then
      Exit;
    build := FTemplate.NewBuildEntryById(buildId);
    build.project := BuildForm.edtProject.Text;
  finally
    FreeAndNil(BuildForm);
  end;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupAddDependencyItem(Sender: TObject);
var
  dependancyId : string;
  DependencyForm: TDependencyForm;
  dependency : ISpecDependency;
  ver : TVersionRange;
begin
  DependencyForm := TDependencyForm.Create(nil);
  try
    DependencyForm.edtDependencyId.Text := 'default';

    if DependencyForm.ShowModal =  mrCancel then
      Exit;
    dependancyId := DependencyForm.edtDependencyId.Text;
    if dependancyId.IsEmpty then
      Exit;
    dependency := FTemplate.NewDependencyById(dependancyId);
    if length(DependencyForm.edtVersion.Text) > 0 then
    begin
       if TVersionRange.TryParse(DependencyForm.edtVersion.Text, ver) then
       begin
         dependency.version := ver;
       end;
    end;
  finally
    FreeAndNil(DependencyForm);
  end;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupAddDesignItem(Sender: TObject);
var
  designBuidId : string;
  DesignForm: TBplForm;
  design : ISpecBPLEntry;
begin
  DesignForm := TBplForm.Create(nil);
  try
    DesignForm.edtBuildId.Text := 'buildId';

    if DesignForm.ShowModal =  mrCancel then
      Exit;
    designBuidId := DesignForm.edtBuildId.Text;
    if designBuidId.IsEmpty then
      Exit;
    design := FTemplate.NewDesignBplBySrc(DesignForm.edtSource.Text);
    design.buildId := DesignForm.edtBuildId.Text;
    design.Source := DesignForm.edtSource.Text;
    design.install := DesignForm.chkInstall.Checked;
  finally
    FreeAndNil(DesignForm);
  end;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupAddRuntimeItem(Sender: TObject);
var
  runtimeBuildId : string;
  RuntimeForm: TBplForm;
  runtime : ISpecBPLEntry;
begin
  RuntimeForm := TBplForm.Create(nil);
  try
    RuntimeForm.edtBuildId.Text := 'default';

    if RuntimeForm.ShowModal =  mrCancel then
      Exit;
    runtimeBuildId := RuntimeForm.edtBuildId.Text;
    if runtimeBuildId.IsEmpty then
      Exit;
    runtime := FTemplate.NewRuntimeBplBySrc(RuntimeForm.edtSource.Text);
    runtime.Source := RuntimeForm.edtSource.Text;
    runtime.BuildId := runtimeBuildId;
    runtime.copyLocal := RuntimeForm.chkCopyLocal.Checked;
  finally
    FreeAndNil(RuntimeForm);
  end;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupAddSearchPathItem(Sender: TObject);
var
  searchPathStr : string;
  SearchPathForm: TSearchPathForm;
  searchPath : ISpecSearchPath;
begin
  SearchPathForm := TSearchPathForm.Create(nil);
  try
      SearchPathForm.edtSearchPath.Text := 'default';

    if SearchPathForm.ShowModal =  mrCancel then
      Exit;
    searchPathStr := SearchPathForm.edtSearchPath.Text;
    if searchPathStr.IsEmpty then
      Exit;
    searchPath := FTemplate.NewSearchPath(searchPathStr);
  finally
    FreeAndNil(SearchPathForm);
  end;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupAddSourceItem(Sender: TObject);
var
  SourceSrc : string;
  SourceForm: TSourceForm;
  source : ISpecFileEntry;
begin
  SourceForm := TSourceForm.Create(nil);
  try
      SourceForm.edtSource.Text := 'default';

    if SourceForm.ShowModal =  mrCancel then
      Exit;
    SourceSrc := SourceForm.edtSource.Text;
    if SourceSrc.IsEmpty then
      Exit;

    source := FTemplate.NewSource(SourceSrc);
    source.flatten := SourceForm.chkFlatten.Checked;
    source.Destination := SourceForm.edtDest.Text;
  finally
    FreeAndNil(SourceForm);
  end;
  LoadTemplates;
end;


procedure TDSpecCreatorForm.PopupAddFileItem(Sender: TObject);
var
  SourceSrc : string;
  FileForm: TSourceForm;
  FileEntry : ISpecFileEntry;
begin
  FileForm := TSourceForm.Create(nil);
  try
      FileForm.edtSource.Text := 'default';

    if FileForm.ShowModal =  mrCancel then
      Exit;
    SourceSrc := FileForm.edtSource.Text;
    if SourceSrc.IsEmpty then
      Exit;
    FileEntry := FTemplate.NewFiles(SourceSrc);
    FileEntry.flatten := FileForm.chkFlatten.Checked;
    FileEntry.Destination := FileForm.edtDest.Text;
  finally
    FreeAndNil(FileForm);
  end;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupAddLibItem(Sender: TObject);
var
  SourceSrc : string;
  LibForm: TSourceForm;
  LibEntry : ISpecFileEntry;
begin
  LibForm := TSourceForm.Create(nil);
  try
      LibForm.edtSource.Text := 'default';

    if LibForm.ShowModal =  mrCancel then
      Exit;
    SourceSrc := LibForm.edtSource.Text;
    if SourceSrc.IsEmpty then
      Exit;
    LibEntry := FTemplate.NewLib(SourceSrc);
    LibEntry.flatten := LibForm.chkFlatten.Checked;
    LibEntry.Destination := LibForm.edtDest.Text;
  finally
    FreeAndNil(LibForm);
  end;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupDeleteBuildItem(Sender: TObject);
begin
  (tvTemplates.Selected as TTemplateTreeNode).DeleteBuild;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupDeleteDependencyItem(Sender: TObject);
begin
  (tvTemplates.Selected as TTemplateTreeNode).DeleteDependency;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupDeleteDesignItem(Sender: TObject);
begin
  (tvTemplates.Selected as TTemplateTreeNode).DeleteDesign;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupDeleteRuntimeItem(Sender: TObject);
begin
  (tvTemplates.Selected as TTemplateTreeNode).DeleteRuntime;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupDeleteSearchPathItem(Sender: TObject);
begin
  (tvTemplates.Selected as TTemplateTreeNode).DeleteSearchPath;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupDeleteSourceItem(Sender: TObject);
begin
  (tvTemplates.Selected as TTemplateTreeNode).DeleteSource;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupDeleteFileItem(Sender: TObject);
begin
  (tvTemplates.Selected as TTemplateTreeNode).DeleteFileEntry;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.PopupDeleteLibItem(Sender: TObject);
begin
  (tvTemplates.Selected as TTemplateTreeNode).DeleteLibEntry;
  LoadTemplates;
end;

procedure TDSpecCreatorForm.tvTemplatesChange(Sender: TObject; Node: TTreeNode);
var
  lNode: TTemplateTreeNode;
  lNodeParent: TTemplateTreeNode;
  j: Integer;
begin
  lNode := Node as TTemplateTreeNode;
  lNodeParent := lNode.Parent as TTemplateTreeNode;
  if lNode.IsSearchPathHeading then
  begin
    CardPanel.ActiveCard := crdSearchPaths;
    CardPanel.Visible := False;
  end
  else if lNode.IsSourceHeading or
          lNode.IsFileEntryHeading or
          lNode.IsLibEntryHeading then
  begin
    CardPanel.ActiveCard := crdSource;
    CardPanel.Visible := False;
  end
  else if lNode.IsBuildHeading then
  begin
    CardPanel.ActiveCard := crdBuild;
    CardPanel.Visible := False;
  end
  else if lNode.IsRuntimeHeading then
  begin
    CardPanel.ActiveCard := crdRuntimeOrDesignBpl;
    CardPanel.Visible := False;
  end
  else if lNode.IsDependencyHeading then
  begin
    CardPanel.ActiveCard := crdDependencies;
    CardPanel.Visible := False;
  end
  else if (lNode.TemplateHeading) then
  begin
    CardPanel.Visible := True;
    CardPanel.ActiveCard := crdTemplates;
    edtTemplateName.Text := lNode.Template.name;
  end
  else
  begin
    if (lNodeParent <> nil) then
    begin
      if lNodeParent.IsSearchPathHeading then
      begin
        edtSearchPath.Text := lNode.searchpath.path;
        CardPanel.Visible := True;
        CardPanel.ActiveCard := crdSearchPaths;
      end;
      if lNodeParent.IsSourceHeading or
         lNodeParent.IsFileEntryHeading or
         lNodeParent.IsLibEntryHeading then
      begin
        CardPanel.Visible := True;
        CardPanel.ActiveCard := crdSource;
        edtFileEntrySource.Text := lNode.fileEntry.Source;
        chkFileEntryFlatten.Checked := lNode.fileEntry.flatten;
        edtFileEntryDest.Text := lNode.fileEntry.Destination;
        lbFileEntryExclude.Clear;
        for j := 0 to lNode.fileEntry.Exclude.Count - 1 do
        begin
          lbFileEntryExclude.Items.Add(lNode.fileEntry.Exclude[j]);
        end;
      end;
      if lNodeParent.IsBuildHeading then
      begin
        CardPanel.Visible := True;
        CardPanel.ActiveCard := crdBuild;
        edtBuildId.Text := lNode.build.id;
        edtProject.Text := lNode.build.project;
        edtConfiguration.Text := lNode.build.Config;
        chkBuildForDesign.Checked := lNode.build.buildForDesign;
        chkDesignOnly.Checked := lNode.build.DesignOnly;
      end;
      if lNodeParent.IsRuntimeHeading or lNodeParent.IsDesignHeading then
      begin
        CardPanel.Visible := True;
        CardPanel.ActiveCard := crdRuntimeOrDesignBpl;
        edtBPLEntryBuildId.Text := lNode.bplEntry.buildId;
        edtBPLEntrySrc.Text := lNode.bplEntry.Source;
        chkCopyLocal.Checked := lNode.bplEntry.copyLocal;
      end;
      if lNodeParent.IsDependencyHeading then
      begin
        CardPanel.Visible := True;
        CardPanel.ActiveCard := crdDependencies;
        edtDependencyId.Text := lNode.dependency.id;
        if not lNode.dependency.version.IsEmpty then
          edtDependencyVersion.Text := lNode.dependency.version.ToString
        else
          edtDependencyVersion.Text := '';
      end;
    end;
  end;
end;

procedure TDSpecCreatorForm.tvTemplatesCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse := false;
end;

procedure TDSpecCreatorForm.tvTemplatesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  item : TMenuItem;
  localPos : TPoint;
  node : TTemplateTreeNode;
  categoryNode : TTemplateTreeNode;
begin
  localPos := tvTemplates.ClientToScreen(MousePos);
  if Assigned(tvTemplates.Selected) then
  begin
    node := tvTemplates.Selected as TTemplateTreeNode;
    if node.TemplateHeading then
    begin
      node.EditText;
      Handled := True;
    end;

    tvTemplates.PopupMenu.Items.Clear;
    node := tvTemplates.GetNodeAt(MousePos.X, MousePos.Y) as TTemplateTreeNode;
    FTemplate := nil;

    if Assigned(node.Template) then
      FTemplate := node.Template;

    categoryNode := node.CategoryNode;
    item := TMenuItem.Create(PopupMenu);
    item.Caption := categoryNode.OnNewText;
    item.OnClick := categoryNode.OnNewClick;
    tvTemplates.PopupMenu.Items.Add(item);

    item := TMenuItem.Create(PopupMenu);
    item.Caption := categoryNode.OnDeleteText;
    item.OnClick := categoryNode.OnDeleteClick;
    tvTemplates.PopupMenu.Items.Add(item);

    tvTemplates.PopupMenu.Popup(localPos.X, localPos.Y);

    Handled := True;
  end;
end;

procedure TDSpecCreatorForm.tvTemplatesCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TTemplateTreeNode;
end;

procedure TDSpecCreatorForm.tvTemplatesEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  FOpenFile.spec.RenameTemplate(Node.Text, s);
  edtTemplateName.Text := s;
end;

procedure TDSpecCreatorForm.tvTemplatesEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := (Node as TTemplateTreenode).TemplateHeading;
end;

procedure TDSpecCreatorForm.VariablesListStringsChange(Sender: TObject);
var
  vplatform : ISpecTargetPlatform;
begin
  vplatform := FOpenFile.GetPlatform(clbCompilers.Items[clbCompilers.ItemIndex]);

  if clbCompilers.Checked[clbCompilers.ItemIndex] and not Assigned(vplatform) then
  begin
    vplatform := FOpenFile.AddCompiler(clbCompilers.Items[clbCompilers.ItemIndex]);
  end;
  if FInVariableUpdate then
    Exit;
  if Assigned(vplatform) then
  begin
    vplatform.Variables.Clear;
    vplatform.Variables.AddStrings(VariablesList.Strings);
  end;
end;

end.
