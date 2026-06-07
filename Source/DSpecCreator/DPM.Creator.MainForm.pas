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
  System.Generics.Collections,
  Spring.Collections,
  Spring.Container,
  VSoft.CancellationToken,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Packaging,
  DPM.Core.Options.Sign,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Package.Signing.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Creator.Logger,
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
    lblSrc : TLabel;
    edtFileEntrySource : TEdit;
    lblDest : TLabel;
    edtFileEntryDest : TEdit;
    lbFileEntryExclude : TListBox;
    btnAddExclude : TButton;
    btnDeleteExclude : TButton;
    crdBuild : TCard;
    lblBuild : TLabel;
    lblProject : TLabel;
    edtProject : TEdit;
    lblBuildDefines : TLabel;
    edtBuildDefines : TEdit;
    lblBuildPlatforms : TLabel;
    clbBuildPlatforms : TCheckListBox;
    lblBuildReferences : TLabel;
    lbBuildReferences : TListBox;
    btnAddBuildRef : TButton;
    btnDeleteBuildRef : TButton;
    crdDesign : TCard;
    lblDesign : TLabel;
    lblDesignProject : TLabel;
    edtDesignProject : TEdit;
    lblDesignDefines : TLabel;
    edtDesignDefines : TEdit;
    lblDesignPlatforms : TLabel;
    clbDesignPlatforms : TCheckListBox;
    lblDesignReferences : TLabel;
    lbDesignReferences : TListBox;
    btnAddDesignRef : TButton;
    btnDeleteDesignRef : TButton;
    lblLibPrefix : TLabel;
    edtLibPrefix : TEdit;
    lblLibSuffix : TLabel;
    edtLibSuffix : TEdit;
    lblLibVersion : TLabel;
    edtLibVersion : TEdit;
    PopupMenu : TPopupMenu;
    BalloonHint1 : TBalloonHint;
    tsGenerate : TTabSheet;
    lblCompilers : TLabel;
    lblPlatform : TLabel;
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
    crdTemplate : TCard;
    edtTemplateName : TEdit;
    lblTemplateName : TLabel;
    tsLogging : TTabSheet;
    Memo2 : TMemo;
    VariablesList : TValueListEditor;
    PackageVariablesList : TValueListEditor;
    lblPackageVariables : TLabel;
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
    actAddDesignItem : TAction;
    actDeleteDesignItem : TAction;
    actAddSourceItem : TAction;
    actDeleteSourceItem : TAction;
    actAddDependency : TAction;
    actDeleteDependency : TAction;
    OpenPictureDialog1 : TOpenPictureDialog;
    pnlIcon : TPanel;
    ImgIcon : TImage;
    crdBuildHeading : TCard;
    lblBuildHeading : TLabel;
    lblBuildDescription : TLabel;
    crdSourceHeading : TCard;
    lblSourceItemsHeading : TLabel;
    lblSourceItemsDescription : TLabel;
    crdDependenciesHeading : TCard;
    Label6 : TLabel;
    Label7 : TLabel;
    crdDesignHeading : TCard;
    lblDesignHeading : TLabel;
    lblDesignDescription : TLabel;
    lblSourceItemHeader : TLabel;
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
    lblCopyright: TLabel;
    edtCopyright : TEdit;
    Panel2 : TPanel;
    lblTemplateView : TLabel;
    tvTemplates : TTreeView;
    Label3 : TLabel;
    btnAddTemplate : TButton;
    btnDeleteTemplate : TButton;
    btnDuplicateTemplate : TButton;
    Splitter1: TSplitter;
    edtReadme: TEdit;
    Label14: TLabel;
    edtRepositoryCommit: TEdit;
    Label13: TLabel;
    tsSigning: TTabSheet;
    pnlProviders: TPanel;
    Panel4: TPanel;
    chkEnableSigning: TCheckBox;
    cboSigningProvider: TComboBox;
    Label8: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    // Common signing fields (on pnlProviders)
    lblTimestampUrl : TLabel;
    edtTimestampUrl : TEdit;
    lblDigest : TLabel;
    cboDigest : TComboBox;
    // Windows certificate store (TabSheet1)
    lblCertThumbprint : TLabel;
    edtCertThumbprint : TEdit;
    lblStoreLocation : TLabel;
    cboStoreLocation : TComboBox;
    // PFX file (TabSheet2)
    lblPfxFile : TLabel;
    edtPfxFile : TEdit;
    btnBrowsePfx : TButton;
    lblPfxPassword : TLabel;
    edtPfxPassword : TEdit;
    // Signotaur (TabSheet3)
    lblSignotaurEndpoint : TLabel;
    edtSignotaurEndpoint : TEdit;
    lblSignotaurApiKey : TLabel;
    edtSignotaurApiKey : TEdit;
    lblSignotaurThumbprint : TLabel;
    edtSignotaurThumbprint : TEdit;
    lblSignotaurSubject : TLabel;
    edtSignotaurSubject : TEdit;
    lblSignotaurLabel : TLabel;
    edtSignotaurLabel : TEdit;
    chkSignotaurAllowSelfSigned : TCheckBox;
    // Azure Key Vault (TabSheet4)
    lblVaultUrl : TLabel;
    edtVaultUrl : TEdit;
    lblCertName : TLabel;
    edtCertName : TEdit;
    lblKeyVersion : TLabel;
    edtKeyVersion : TEdit;
    lblTenantId : TLabel;
    edtTenantId : TEdit;
    lblClientId : TLabel;
    edtClientId : TEdit;
    lblClientSecret : TLabel;
    edtClientSecret : TEdit;
    btnCancelPack : TButton;
    // Upload tab
    tsUpload : TTabSheet;
    pnlUploadTop : TPanel;
    lblUploadSource : TLabel;
    cboUploadSource : TComboBox;
    lblUploadApiKey : TLabel;
    edtUploadApiKey : TEdit;
    chkUploadSkipDuplicate : TCheckBox;
    rgUploadScope : TRadioGroup;
    lblUploadPackages : TLabel;
    lstUploadPackages : TListBox;
    btnRefreshPackages : TButton;
    btnUpload : TButton;
    btnCancelUpload : TButton;
    Memo3 : TMemo;
    crdPackageDefsHeading: TCard;
    lblPackageDefsHeading: TLabel;
    lblPackageDefsDescription: TLabel;
    crdPackageDef : TCard;
    lblPackageDef : TLabel;
    lblPackageDefProject : TLabel;
    edtPackageDefProject : TEdit;
    lblPackageDefKind : TLabel;
    cboPackageDefKind : TComboBox;
    lblPackageDefPlatforms : TLabel;
    clbPackageDefPlatforms : TCheckListBox;
    lblPackageDefFiles : TLabel;
    lbPackageDefFiles : TListBox;
    btnAddPackageDefFile : TButton;
    btnDeletePackageDefFile : TButton;
    lblPackageDefExclude : TLabel;
    lbPackageDefExclude : TListBox;
    btnAddPackageDefExclude : TButton;
    btnDeletePackageDefExclude : TButton;
    lblPackageDefRequires : TLabel;
    lbPackageDefRequires : TListBox;
    btnAddPackageDefRequire : TButton;
    btnDeletePackageDefRequire : TButton;
    actAddPackageDefItem : TAction;
    actDeletePackageDefItem : TAction;
    crdEnvironmentVariables : TCard;
    lblEnvironmentVariablesHeading : TLabel;
    lblEnvironmentVariablesDescription : TLabel;
    envVariablesList : TValueListEditor;
    procedure FormDestroy(Sender : TObject);
    procedure btnAddExcludeClick(Sender : TObject);
    procedure btnAddTemplateClick(Sender : TObject);
    procedure btnBuildPackagesClick(Sender : TObject);
    procedure btnCancelPackClick(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure cboUploadSourceChange(Sender : TObject);
    procedure rgUploadScopeClick(Sender : TObject);
    procedure btnRefreshPackagesClick(Sender : TObject);
    procedure btnUploadClick(Sender : TObject);
    procedure btnCancelUploadClick(Sender : TObject);
    procedure PageControlChange(Sender : TObject);
    procedure btnDeleteExcludeClick(Sender : TObject);
    procedure cboLicenseChange(Sender : TObject);
    procedure cboTemplateChange(Sender : TObject);
    procedure clbCompilersClick(Sender : TObject);
    procedure clbPlatformsClickCheck(Sender : TObject);
    procedure edtAuthorChange(Sender : TObject);
    procedure edtBuildDefinesChange(Sender : TObject);
    procedure clbBuildPlatformsClickCheck(Sender : TObject);
    procedure btnAddBuildRefClick(Sender : TObject);
    procedure btnDeleteBuildRefClick(Sender : TObject);
    procedure edtDesignProjectChange(Sender : TObject);
    procedure edtDesignDefinesChange(Sender : TObject);
    procedure clbDesignPlatformsClickCheck(Sender : TObject);
    procedure btnAddDesignRefClick(Sender : TObject);
    procedure btnDeleteDesignRefClick(Sender : TObject);
    procedure edtLibPrefixChange(Sender : TObject);
    procedure edtLibSuffixChange(Sender : TObject);
    procedure edtLibVersionChange(Sender : TObject);
    procedure edtDependencyIdChange(Sender : TObject);
    procedure edtDependencyVersionChange(Sender : TObject);
    procedure edtFileEntrySourceChange(Sender : TObject);
    procedure edtFileEntryDestChange(Sender : TObject);
    procedure edtIdChange(Sender : TObject);
    procedure edtProjectChange(Sender : TObject);
    procedure edtProjectURLChange(Sender : TObject);
    procedure edtRepositoryURLChange(Sender : TObject);
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
    procedure PackageVariablesListStringsChange(Sender : TObject);
    procedure envVariablesListStringsChange(Sender : TObject);
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
    procedure actAddPackageDefItemExecute(Sender : TObject);
    procedure actDeletePackageDefItemExecute(Sender : TObject);
    procedure edtPackageDefProjectChange(Sender : TObject);
    procedure cboPackageDefKindChange(Sender : TObject);
    procedure clbPackageDefPlatformsClickCheck(Sender : TObject);
    procedure btnAddPackageDefFileClick(Sender : TObject);
    procedure btnDeletePackageDefFileClick(Sender : TObject);
    procedure btnAddPackageDefExcludeClick(Sender : TObject);
    procedure btnDeletePackageDefExcludeClick(Sender : TObject);
    procedure btnAddPackageDefRequireClick(Sender : TObject);
    procedure btnDeletePackageDefRequireClick(Sender : TObject);
    procedure actFileOpenExecute(Sender : TObject);
    procedure actFileSaveExecute(Sender : TObject);
    procedure actFileSaveAsExecute(Sender : TObject);
    procedure actFileNewExecute(Sender : TObject);
    procedure actFilePackageWizardExecute(Sender : TObject);
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
    procedure clbCompilersKeyPress(Sender: TObject; var Key: Char);
    procedure edtReadmeChange(Sender: TObject);
    procedure edtRepositoryCommitChange(Sender: TObject);
    procedure chkEnableSigningClick(Sender: TObject);
    procedure cboSigningProviderChange(Sender : TObject);
    procedure btnBrowsePfxClick(Sender : TObject);
  private
    { Private declarations }
    FtmpFilename : string;
    FOpenFile : TDSpecFile;
    FTemplate : ISpecTemplate;
    FLogger : ILogger;
    FInVariableUpdate : Boolean;
    FLoadingCard : Boolean;
    FSPDXList : TStringList;
    FMRUMenu : TMRUMenu;
    // In-process pack / sign / upload
    FContainer : TContainer;
    FPackageWriter : IPackageWriter;
    FSigningService : IPackageSigningService;
    FX509 : IX509Service;
    FConfigManager : IConfigurationManager;
    FRepositoryManager : IPackageRepositoryManager;
    FOpsLogger : TDSpecQueuedLogger;   // concrete instance (for SetTarget)
    FPackLogger : ILogger;             // same object as FOpsLogger
    FCancellationTokenSource : ICancellationTokenSource;
    FPacking : Boolean;
    FUploading : Boolean;
    // Upload page state
    FUploadConfig : IConfiguration;
    FUploadConfigFile : string;
    FUploadCurrentSource : string;
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

    // In-process pack / sign helpers
    procedure InitCoreContainer;
    procedure UpdateSigningProviderPage;
    function BuildSignOptionsFromUI : TSignOptions;
    procedure SignProducedPackages(const cancelToken : ICancellationToken; const outputFolder : string;
                                   const sinceTime : TDateTime; const signOptions : TSignOptions);
    procedure LoadSigningSettings;
    procedure SaveSigningSettings;
    // Secrets are kept in Windows Credential Manager, never in the ini file.
    function ReadSecret(const target : string) : string;
    procedure WriteSecret(const target : string; const secret : string);

    // In-process upload (push) helpers
    procedure LoadUploadSources;
    function ResolveUploadFiles : TArray<string>;
    procedure RefreshUploadPackages;
    procedure UpdateUploadApiKeyState;
    procedure LoadUploadSettings;
    procedure SaveUploadSettings;
    function UploadApiKeyCredTarget(const sourceName : string) : string;

    function FindTemplateNode(const template : ISpecTemplate) : TTemplateTreeNode;
    function FindHeadingNode(const templateNode : TTemplateTreeNode; nodeType : TNodeType) : TTemplateTreeNode;

    function LoadTemplate(const template : ISpecTemplate) : TTemplateTreeNode;
    procedure LoadTemplates;
    procedure EnableDisablePlatform(compilerVersion : TCompilerVersion);
    function ReplaceVars(const inputStr : String; compiler : TCompilerVersion) : string;
    function AddRootTemplateNode(template : ISpecTemplate) : TTemplateTreeNode;

    procedure SetCheckListPlatforms(const clb : TCheckListBox; const platforms : TDPMPlatforms);
    function GetCheckListPlatforms(const clb : TCheckListBox) : TDPMPlatforms;

    function LoadSourceNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const sourceEntry : ISpecSourceEntry) : TTemplateTreeNode;
    procedure LoadSourceNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecSourceEntry>);

    function LoadBuildNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const buildEntry : ISpecBuildEntry) : TTemplateTreeNode;
    procedure LoadBuildNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);

    function LoadDesignNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const item : ISpecDesignEntry) : TTemplateTreeNode;
    procedure LoadDesignNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecDesignEntry>);

    function LoadPackageDefNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const item : ISpecPackageDefinition) : TTemplateTreeNode;
    procedure LoadPackageDefNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const defList : IList<ISpecPackageDefinition>);

    function LoadDependency(const parentNode : TTemplateTreeNode; template : ISpecTemplate; const dependency : ISpecDependency) : TTemplateTreeNode;
    procedure LoadDependencies(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);

    procedure LoadEnvironmentVariablesNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);

    procedure UriClick(const uri : string);

    procedure LoadSPDXList;
    procedure LoadDspecStructure;
    procedure SaveDspecStructure(const filename : string);

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
  DPM.Core.Constants,
  DPM.Core.dependency.Version,
  DPM.Core.Init,
  DPM.Core.Options.Pack,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Provider.Interfaces,
  DPM.Core.Crypto.Provider.Factory,
  DPM.Core.Options.Push,
  DPM.Core.Utils.Config,
  VSoft.Windows.CredentialManager,
  DPM.Creator.TemplateForm,
  DPM.Creator.FileForm,
  DPM.Creator.BuildForm,
  DPM.Creator.OptionsForm,
  DPM.Creator.DependencyForm,
  DPM.Creator.Dspec.Replacer,
  DPM.Creator.PackageWizardForm,
  DPM.IDE.AboutForm;

const
  cToolName = 'DPM dspec Creator';
  cNewTemplate = 'Create New Template...';
  // Windows Credential Manager target names for signing secrets.
  cCredUserName = 'DPM';
  cCredPfxPassword = 'DPM.DspecCreator.Signing.PfxPassword';
  cCredSignotaurApiKey = 'DPM.DspecCreator.Signing.SignotaurApiKey';
  cCredAzureClientSecret = 'DPM.DspecCreator.Signing.AzureClientSecret';
  // Azure tenant/client ids are not secrets, but they identify the tenant and
  // app registration, so we keep them out of the plaintext ini too.
  cCredAzureTenantId = 'DPM.DspecCreator.Signing.AzureTenantId';
  cCredAzureClientId = 'DPM.DspecCreator.Signing.AzureClientId';
  // Upload api keys are stored per source name in the credential manager.
  cCredUploadApiKeyPrefix = 'DPM.DspecCreator.Upload.ApiKey.';

procedure TDSpecCreatorForm.btnDeleteExcludeClick(Sender : TObject);
var
  exclude : string;
  itemToDelete : integer;
  entry : ISpecSourceEntry;
begin
  if lbFileEntryExclude.ItemIndex < 0 then
    Exit;

  if Assigned(tvTemplates.Selected) then
  begin
    entry := (tvTemplates.Selected as TTemplateTreeNode).sourceEntry;
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
  entry : ISpecSourceEntry;
begin
  src := InputBox('Add Exclude', 'Exclude to Add', '');

  if Assigned(tvTemplates.Selected) then
  begin
    entry := (tvTemplates.Selected as TTemplateTreeNode).sourceEntry;
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
  newTemplate := FOpenFile.PackageSpec.newTemplate(templateName);
  templateNode := LoadTemplate(newTemplate);
  tvTemplates.Selected := templateNode;
end;

procedure TDSpecCreatorForm.btnBuildPackagesClick(Sender : TObject);
var
  guid : TGUID;
  packOptions : TPackOptions;
  signOptions : TSignOptions;
  outputFolder : string;
  workingDir : string;
  versionStr : string;
  signingEnabled : boolean;
  packStartTime : TDateTime;
  cleanup : TProc;
begin
  if FPacking then
    Exit;

  // The packer resolves relative source/readme paths against the current
  // directory (only '.\'-prefixed values use BasePath), so we must run from the
  // dspec's folder. That only exists once the project has been saved.
  if (FOpenFile.FileName = '') or (not FileExists(FOpenFile.FileName)) then
  begin
    MessageDlg('Please save the dspec before packing - relative paths in the spec are resolved ' +
               'against the dspec''s folder.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if not DirectoryExists(edtPackageOutputPath.Text) then
  begin
    FPackLogger.Error('Output folder does not exist: ' + edtPackageOutputPath.Text);
    Exit;
  end;

  // Write the in-memory dspec to a temp file the core spec reader can load.
  guid := TGUID.NewGuid;
  workingDir := FOpenFile.WorkingDir;
  FtmpFilename := TPath.Combine(workingDir, guid.ToString);
  FtmpFilename := ChangeFileExt(FtmpFilename, cPackageSpecExt);
  TFile.WriteAllText(FtmpFilename, FOpenFile.AsString);

  outputFolder := edtPackageOutputPath.Text;
  signingEnabled := chkEnableSigning.Checked;

  packOptions := TPackOptions.Create;
  packOptions.SpecFile := FtmpFilename;
  packOptions.OutputFolder := outputFolder;
  packOptions.BasePath := workingDir;
  versionStr := Trim(edtVersion.Text);
  if versionStr <> '' then
    packOptions.Version := versionStr;

  // Read the Signing tab on the UI thread; the worker only consumes the options.
  if signingEnabled then
    signOptions := BuildSignOptionsFromUI
  else
    signOptions := nil;

  cleanup :=
    procedure
    begin
      FPacking := false;
      btnBuildPackages.Enabled := true;
      btnCancelPack.Enabled := false;
      packOptions.Free;
      packOptions := nil;
      signOptions.Free;
      signOptions := nil;
      if FtmpFilename <> '' then
      begin
        TFile.Delete(FtmpFilename);
        FtmpFilename := '';
      end;
    end;

  FOpsLogger.SetTarget(Memo1.Lines);
  Memo1.Clear;
  PageControl.ActivePage := tsGenerate;
  FPacking := true;
  btnBuildPackages.Enabled := false;
  btnCancelPack.Enabled := true;
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;
  packStartTime := Now;

  TAsync.Configure<boolean>(
    function(const cancelToken : ICancellationToken) : boolean
    var
      savedDir : string;
    begin
      // The packer resolves relative paths against the process current
      // directory; point it at the dspec folder for the duration of the pack.
      savedDir := GetCurrentDir;
      SetCurrentDir(workingDir);
      try
        result := FPackageWriter.WritePackageFromSpec(cancelToken, packOptions);
        if result and signingEnabled and (signOptions <> nil) then
          SignProducedPackages(cancelToken, outputFolder, packStartTime, signOptions);
      finally
        SetCurrentDir(savedDir);
      end;
    end, FCancellationTokenSource.Token)
  .OnException(
    procedure(const e : Exception)
    begin
      FPackLogger.Error('Error creating package : ' + e.Message);
      cleanup();
    end)
  .OnCancellation(
    procedure
    begin
      FPackLogger.Warning('Pack cancelled.');
      cleanup();
    end)
  .Await(
    procedure(const ok : boolean)
    begin
      if ok then
        FPackLogger.Success('Pack completed.')
      else
        FPackLogger.Error('Pack failed.');
      cleanup();
    end);
end;

procedure TDSpecCreatorForm.btnCancelPackClick(Sender : TObject);
begin
  // Invoked by the Cancel button and by Esc (via FormKeyDown). Ignore unless a
  // pack is actually running, and only signal cancellation once.
  if not FPacking then
    Exit;
  if (FCancellationTokenSource <> nil) and (not FCancellationTokenSource.Token.IsCancelled) then
  begin
    FPackLogger.Information('Cancelling...');
    btnCancelPack.Enabled := false;
    FCancellationTokenSource.Cancel;
  end;
end;

procedure TDSpecCreatorForm.btnCancelUploadClick(Sender : TObject);
begin
  // Invoked by the Cancel button and by Esc (via FormKeyDown). Ignore unless an
  // upload is actually running, and only signal cancellation once.
  if not FUploading then
    Exit;
  if (FCancellationTokenSource <> nil) and (not FCancellationTokenSource.Token.IsCancelled) then
  begin
    FPackLogger.Information('Cancelling...');
    btnCancelUpload.Enabled := false;
    FCancellationTokenSource.Cancel;
  end;
end;

procedure TDSpecCreatorForm.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  // Esc cancels a running pack/sign/upload from anywhere in the app. Only consume
  // the key while an operation is running so normal Esc behaviour is unaffected.
  if Key <> VK_ESCAPE then
    Exit;
  if FPacking then
  begin
    btnCancelPackClick(nil);
    Key := 0;
  end
  else if FUploading then
  begin
    btnCancelUploadClick(nil);
    Key := 0;
  end;
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

procedure TDSpecCreatorForm.cboLicenseChange(Sender : TObject);
begin
  FOpenFile.PackageSpec.metadata.license := cboLicense.Text;
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

  if vPlatform = nil then
    vPlatform := FOpenFile.AddCompiler(clbCompilers.Items[clbCompilers.ItemIndex]);
  if vPlatform = nil then
    exit; //should never get here but have seen it, can't reproduce though.

  vPlatform.TemplateName := templateName;
  //cboTemplate.ItemIndex := cboTemplate.Items.IndexOf(templateName);
end;

procedure TDSpecCreatorForm.chkEnableSigningClick(Sender: TObject);
begin
  pnlProviders.Enabled := chkEnableSigning.Checked;
end;

procedure TDSpecCreatorForm.cboSigningProviderChange(Sender : TObject);
begin
  UpdateSigningProviderPage;
end;

procedure TDSpecCreatorForm.btnBrowsePfxClick(Sender : TObject);
begin
  if OpenDialog.Execute then
    edtPfxFile.Text := OpenDialog.FileName;
end;

procedure TDSpecCreatorForm.InitCoreContainer;
begin
  FContainer := TContainer.Create;
  // InitCore does not register an ILogger - register ours first so the pack,
  // signing and upload services log into the active page's memo (retargeted
  // per operation via FOpsLogger.SetTarget).
  FContainer.RegisterInstance<ILogger>(FPackLogger);
  DPM.Core.Init.InitCore(FContainer);
  FContainer.Build;
  FPackageWriter := FContainer.Resolve<IPackageWriter>;
  FSigningService := FContainer.Resolve<IPackageSigningService>;
  FX509 := FContainer.Resolve<IX509Service>;
  FConfigManager := FContainer.Resolve<IConfigurationManager>;
  FRepositoryManager := FContainer.Resolve<IPackageRepositoryManager>;
end;

procedure TDSpecCreatorForm.UpdateSigningProviderPage;
begin
  // The provider combo (not the raw tabs) drives which provider page is shown.
  // Combo index maps 1:1 to PageControl1 page index.
  if (cboSigningProvider.ItemIndex >= 0) and (cboSigningProvider.ItemIndex < PageControl1.PageCount) then
    PageControl1.ActivePageIndex := cboSigningProvider.ItemIndex;
end;

function TDSpecCreatorForm.BuildSignOptionsFromUI : TSignOptions;
begin
  result := TSignOptions.Create;
  result.TimestampUrl := Trim(edtTimestampUrl.Text);
  if cboDigest.ItemIndex <= 0 then
    result.Digest := ''                 // Auto - signing service picks from the cert key type.
  else
    result.Digest := cboDigest.Text;

  case cboSigningProvider.ItemIndex of
    0 : // Windows Certificate Store
      begin
        result.Provider := spLocal;
        result.Thumbprint := Trim(edtCertThumbprint.Text);
        result.StoreLocation := TSignStoreLocation(cboStoreLocation.ItemIndex);
      end;
    1 : // PFX file
      begin
        result.Provider := spLocal;
        result.PfxFile := Trim(edtPfxFile.Text);
        result.PfxPassword := edtPfxPassword.Text;     // literal - not trimmed, passwords may contain spaces
      end;
    2 : // Signotaur
      begin
        result.Provider := spSignotaur;
        result.SignotaurEndpoint := Trim(edtSignotaurEndpoint.Text);
        result.SignotaurApiKey := edtSignotaurApiKey.Text;   // literal secret
        result.Thumbprint := Trim(edtSignotaurThumbprint.Text);
        result.SignotaurSubject := Trim(edtSignotaurSubject.Text);
        result.SignotaurLabel := Trim(edtSignotaurLabel.Text);
        result.SignotaurAllowSelfSigned := chkSignotaurAllowSelfSigned.Checked;
      end;
    3 : // Azure Key Vault
      begin
        result.Provider := spKeyVault;
        result.VaultUrl := Trim(edtVaultUrl.Text);
        result.CertName := Trim(edtCertName.Text);
        result.KeyVersion := Trim(edtKeyVersion.Text);
        result.TenantId := Trim(edtTenantId.Text);
        result.ClientId := Trim(edtClientId.Text);
        result.ClientSecret := edtClientSecret.Text;   // literal secret
      end;
  end;
end;

procedure TDSpecCreatorForm.SignProducedPackages(const cancelToken : ICancellationToken; const outputFolder : string;
                                                 const sinceTime : TDateTime; const signOptions : TSignOptions);
var
  provider : ISigningProvider;
  signOpts : ISignOptions;
  alg : THashAlgorithm;
  files : TArray<string>;
  f : string;
  toSign : IList<string>;
  okCount : integer;
  failCount : integer;
begin
  if signOptions.Digest = '' then
    alg := haUnknown
  else if not TAlgorithmProfile.ParseHashName(signOptions.Digest, alg) then
  begin
    FPackLogger.Error('Unsupported digest "' + signOptions.Digest + '" (SHA256/SHA384/SHA512 only).');
    Exit;
  end;

  provider := TSigningProviderFactory.CreateProvider(FPackLogger, FX509, signOptions);
  if provider = nil then
  begin
    FPackLogger.Error('Signing skipped - the signing provider could not be created.');
    Exit;
  end;

  // The pack pipeline returns only a boolean, so identify the produced files by
  // collecting every .dpkg written since packing started.
  toSign := TCollections.CreateList<string>;
  files := TDirectory.GetFiles(outputFolder, '*.dpkg');
  for f in files do
    if TFile.GetLastWriteTime(f) >= sinceTime then
      toSign.Add(f);

  if toSign.Count = 0 then
  begin
    FPackLogger.Warning('No newly produced .dpkg files found to sign.');
    Exit;
  end;

  signOpts.TimestampUrl := signOptions.TimestampUrl;
  signOpts.DigestAlgorithm := alg;

  FPackLogger.Information(Format('Signing %d package(s)...', [toSign.Count]));
  okCount := 0;
  failCount := 0;
  // One session for the whole batch - smart-card / HSM providers prompt once.
  provider.BeginSession;
  try
    for f in toSign do
    begin
      if cancelToken.IsCancelled then
        Break;
      try
        FSigningService.SignPackage(f, provider, signOpts);
        FPackLogger.Success('Signed ' + ExtractFileName(f));
        Inc(okCount);
      except
        on e : Exception do
        begin
          Inc(failCount);
          FPackLogger.Error('Sign failed for [' + ExtractFileName(f) + ']: ' + e.Message);
        end;
      end;
    end;
  finally
    provider.EndSession;
  end;

  if failCount = 0 then
    FPackLogger.Success(Format('Signed %d package(s) successfully.', [okCount]))
  else
    FPackLogger.Warning(Format('%d signed, %d failed.', [okCount, failCount]));
end;

procedure TDSpecCreatorForm.LoadSigningSettings;
var
  iniFile : TIniFile;
begin
  iniFile := TIniFile.Create(MRUListService.GetIniFilePath);
  try
    chkEnableSigning.Checked := iniFile.ReadBool('Signing', 'Enabled', false);
    cboSigningProvider.ItemIndex := iniFile.ReadInteger('Signing', 'Provider', 0);
    edtTimestampUrl.Text := iniFile.ReadString('Signing', 'TimestampUrl', 'http://timestamp.digicert.com');
    cboDigest.ItemIndex := iniFile.ReadInteger('Signing', 'Digest', 0);
    edtCertThumbprint.Text := iniFile.ReadString('Signing', 'CertThumbprint', '');
    cboStoreLocation.ItemIndex := iniFile.ReadInteger('Signing', 'StoreLocation', 0);
    edtPfxFile.Text := iniFile.ReadString('Signing', 'PfxFile', '');
    edtSignotaurEndpoint.Text := iniFile.ReadString('Signing', 'SignotaurEndpoint', '');
    edtSignotaurThumbprint.Text := iniFile.ReadString('Signing', 'SignotaurThumbprint', '');
    edtSignotaurSubject.Text := iniFile.ReadString('Signing', 'SignotaurSubject', '');
    edtSignotaurLabel.Text := iniFile.ReadString('Signing', 'SignotaurLabel', '');
    chkSignotaurAllowSelfSigned.Checked := iniFile.ReadBool('Signing', 'SignotaurAllowSelfSigned', false);
    edtVaultUrl.Text := iniFile.ReadString('Signing', 'VaultUrl', '');
    edtCertName.Text := iniFile.ReadString('Signing', 'CertName', '');
    edtKeyVersion.Text := iniFile.ReadString('Signing', 'KeyVersion', '');
  finally
    iniFile.Free;
  end;

  // Secrets and the sensitive Azure tenant/client identifiers live in Windows
  // Credential Manager, not the ini.
  edtPfxPassword.Text := ReadSecret(cCredPfxPassword);
  edtSignotaurApiKey.Text := ReadSecret(cCredSignotaurApiKey);
  edtClientSecret.Text := ReadSecret(cCredAzureClientSecret);
  edtTenantId.Text := ReadSecret(cCredAzureTenantId);
  edtClientId.Text := ReadSecret(cCredAzureClientId);
end;

procedure TDSpecCreatorForm.SaveSigningSettings;
var
  iniFile : TIniFile;
begin
  iniFile := TIniFile.Create(MRUListService.GetIniFilePath);
  try
    iniFile.WriteBool('Signing', 'Enabled', chkEnableSigning.Checked);
    iniFile.WriteInteger('Signing', 'Provider', cboSigningProvider.ItemIndex);
    iniFile.WriteString('Signing', 'TimestampUrl', edtTimestampUrl.Text);
    iniFile.WriteInteger('Signing', 'Digest', cboDigest.ItemIndex);
    iniFile.WriteString('Signing', 'CertThumbprint', edtCertThumbprint.Text);
    iniFile.WriteInteger('Signing', 'StoreLocation', cboStoreLocation.ItemIndex);
    iniFile.WriteString('Signing', 'PfxFile', edtPfxFile.Text);
    iniFile.WriteString('Signing', 'SignotaurEndpoint', edtSignotaurEndpoint.Text);
    iniFile.WriteString('Signing', 'SignotaurThumbprint', edtSignotaurThumbprint.Text);
    iniFile.WriteString('Signing', 'SignotaurSubject', edtSignotaurSubject.Text);
    iniFile.WriteString('Signing', 'SignotaurLabel', edtSignotaurLabel.Text);
    iniFile.WriteBool('Signing', 'SignotaurAllowSelfSigned', chkSignotaurAllowSelfSigned.Checked);
    iniFile.WriteString('Signing', 'VaultUrl', edtVaultUrl.Text);
    iniFile.WriteString('Signing', 'CertName', edtCertName.Text);
    iniFile.WriteString('Signing', 'KeyVersion', edtKeyVersion.Text);
  finally
    iniFile.Free;
  end;

  // Secrets and the sensitive Azure tenant/client identifiers go to Windows
  // Credential Manager, never the ini.
  WriteSecret(cCredPfxPassword, edtPfxPassword.Text);
  WriteSecret(cCredSignotaurApiKey, edtSignotaurApiKey.Text);
  WriteSecret(cCredAzureClientSecret, edtClientSecret.Text);
  WriteSecret(cCredAzureTenantId, edtTenantId.Text);
  WriteSecret(cCredAzureClientId, edtClientId.Text);
end;

function TDSpecCreatorForm.ReadSecret(const target : string) : string;
var
  cred : ICredential;
begin
  result := '';
  cred := TCredentialManager.ReadCredential(target);
  if cred <> nil then
    result := cred.Secret;
end;

procedure TDSpecCreatorForm.WriteSecret(const target : string; const secret : string);
begin
  if secret = '' then
  begin
    // Nothing to store - remove any previously saved value. DeleteCredential
    // raises if the target does not exist, so swallow that case.
    try
      TCredentialManager.DeleteCredential(target);
    except
      on EOSError do; // not found - nothing to delete
    end;
  end
  else
    TCredentialManager.WriteCredential(target, cCredUserName, secret, TCredentialPersistence.LocalMachine);
end;

function TDSpecCreatorForm.UploadApiKeyCredTarget(const sourceName : string) : string;
begin
  result := cCredUploadApiKeyPrefix + sourceName;
end;

procedure TDSpecCreatorForm.LoadUploadSources;
var
  i : integer;
  savedSource : string;
begin
  savedSource := cboUploadSource.Text;
  cboUploadSource.Items.BeginUpdate;
  try
    cboUploadSource.Items.Clear;
    FUploadConfig := nil;
    try
      FConfigManager.EnsureDefaultConfig;
      FUploadConfigFile := TConfigUtils.GetDefaultConfigFileName;
      FUploadConfig := FConfigManager.LoadConfig(FUploadConfigFile);
    except
      on e : Exception do
        FPackLogger.Error('Could not load DPM configuration: ' + e.Message);
    end;
    if FUploadConfig <> nil then
      for i := 0 to FUploadConfig.Sources.Count - 1 do
        cboUploadSource.Items.Add(FUploadConfig.Sources[i].Name);
  finally
    cboUploadSource.Items.EndUpdate;
  end;
  // Restore the previous selection if still present, else pick the first.
  if savedSource <> '' then
    cboUploadSource.ItemIndex := cboUploadSource.Items.IndexOf(savedSource);
  if (cboUploadSource.ItemIndex < 0) and (cboUploadSource.Items.Count > 0) then
    cboUploadSource.ItemIndex := 0;
end;

procedure TDSpecCreatorForm.UpdateUploadApiKeyState;
var
  src : ISourceConfig;
  isRemote : boolean;
begin
  isRemote := false;
  if (FUploadConfig <> nil) and (cboUploadSource.Text <> '') then
  begin
    src := FUploadConfig.GetSourceByName(cboUploadSource.Text);
    if src <> nil then
      isRemote := src.SourceType = TSourceType.DPMServer;
  end;
  // API key only applies to https/DPMServer sources; folder sources ignore it.
  lblUploadApiKey.Enabled := isRemote;
  edtUploadApiKey.Enabled := isRemote;
end;

procedure TDSpecCreatorForm.cboUploadSourceChange(Sender : TObject);
begin
  // Persist the api key for the previously selected source before switching.
  if FUploadCurrentSource <> '' then
    WriteSecret(UploadApiKeyCredTarget(FUploadCurrentSource), edtUploadApiKey.Text);
  FUploadCurrentSource := cboUploadSource.Text;
  if FUploadCurrentSource <> '' then
    edtUploadApiKey.Text := ReadSecret(UploadApiKeyCredTarget(FUploadCurrentSource))
  else
    edtUploadApiKey.Text := '';
  UpdateUploadApiKeyState;
end;

function TDSpecCreatorForm.ResolveUploadFiles : TArray<string>;
var
  outputFolder : string;
  id : string;
  version : string;
  mask : string;
begin
  result := nil;
  outputFolder := edtPackageOutputPath.Text;
  if not DirectoryExists(outputFolder) then
    Exit;
  if rgUploadScope.ItemIndex = 0 then
  begin
    // Current package : {id}-*-{version}.dpkg
    id := Trim(FOpenFile.PackageSpec.metadata.id);
    version := Trim(edtVersion.Text);
    if (id = '') or (version = '') then
      Exit;
    mask := id + '-*-' + version + cPackageFileExt;
  end
  else
    mask := '*' + cPackageFileExt;
  result := TDirectory.GetFiles(outputFolder, mask);
end;

procedure TDSpecCreatorForm.RefreshUploadPackages;
var
  files : TArray<string>;
  f : string;
begin
  lstUploadPackages.Items.BeginUpdate;
  try
    lstUploadPackages.Items.Clear;
    files := ResolveUploadFiles;
    for f in files do
      lstUploadPackages.Items.Add(ExtractFileName(f));
  finally
    lstUploadPackages.Items.EndUpdate;
  end;
end;

procedure TDSpecCreatorForm.rgUploadScopeClick(Sender : TObject);
begin
  RefreshUploadPackages;
end;

procedure TDSpecCreatorForm.btnRefreshPackagesClick(Sender : TObject);
begin
  LoadUploadSources;
  UpdateUploadApiKeyState;
  RefreshUploadPackages;
end;

procedure TDSpecCreatorForm.PageControlChange(Sender : TObject);
begin
  if PageControl.ActivePage = tsUpload then
    RefreshUploadPackages;
end;

procedure TDSpecCreatorForm.btnUploadClick(Sender : TObject);
var
  files : TArray<string>;
  sourceName : string;
  apiKey : string;
  skipDuplicate : boolean;
  isRemote : boolean;
  src : ISourceConfig;
  cleanup : TProc;
begin
  if FPacking or FUploading then
    Exit;

  sourceName := cboUploadSource.Text;
  if sourceName = '' then
  begin
    MessageDlg('Please select a source to upload to.', mtWarning, [mbOK], 0);
    Exit;
  end;
  if FUploadConfig = nil then
  begin
    MessageDlg('No DPM configuration is loaded.', mtWarning, [mbOK], 0);
    Exit;
  end;

  src := FUploadConfig.GetSourceByName(sourceName);
  isRemote := (src <> nil) and (src.SourceType = TSourceType.DPMServer);
  apiKey := edtUploadApiKey.Text;
  if isRemote and (Trim(apiKey) = '') then
  begin
    MessageDlg('An API key is required for the selected https source.', mtWarning, [mbOK], 0);
    Exit;
  end;

  files := ResolveUploadFiles;
  if Length(files) = 0 then
  begin
    MessageDlg('No .dpkg files found to upload in the output folder.', mtWarning, [mbOK], 0);
    Exit;
  end;
  skipDuplicate := chkUploadSkipDuplicate.Checked;

  // Persist the api key for this source before we start.
  WriteSecret(UploadApiKeyCredTarget(sourceName), edtUploadApiKey.Text);

  cleanup :=
    procedure
    begin
      FUploading := false;
      btnUpload.Enabled := true;
      btnCancelUpload.Enabled := false;
    end;

  FOpsLogger.SetTarget(Memo3.Lines);
  Memo3.Clear;
  PageControl.ActivePage := tsUpload;
  FUploading := true;
  btnUpload.Enabled := false;
  btnCancelUpload.Enabled := true;
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;

  TAsync.Configure<boolean>(
    function(const cancelToken : ICancellationToken) : boolean
    var
      config : IConfiguration;
      pushOptions : TPushOptions;
      f : string;
      okCount : integer;
      failCount : integer;
    begin
      result := false;
      config := FConfigManager.LoadConfig(FUploadConfigFile);
      if config = nil then
      begin
        FPackLogger.Error('Could not load DPM configuration.');
        Exit;
      end;
      FRepositoryManager.Initialize(config);

      okCount := 0;
      failCount := 0;
      pushOptions := TPushOptions.Create;
      try
        pushOptions.Source := sourceName;
        pushOptions.ApiKey := apiKey;
        pushOptions.SkipDuplicate := skipDuplicate;
        for f in files do
        begin
          if cancelToken.IsCancelled then
            Break;
          pushOptions.PackagePath := f;
          FPackLogger.Information('Uploading ' + ExtractFileName(f) + ' to ' + sourceName + '...');
          if FRepositoryManager.Push(cancelToken, pushOptions) then
            Inc(okCount)
          else
            Inc(failCount);
        end;
      finally
        pushOptions.Free;
      end;

      FPackLogger.Information(Format('Uploaded %d package(s), %d failed.', [okCount, failCount]));
      result := failCount = 0;
    end, FCancellationTokenSource.Token)
  .OnException(
    procedure(const e : Exception)
    begin
      FPackLogger.Error('Error uploading : ' + e.Message);
      cleanup();
    end)
  .OnCancellation(
    procedure
    begin
      FPackLogger.Warning('Upload cancelled.');
      cleanup();
    end)
  .Await(
    procedure(const ok : boolean)
    begin
      if ok then
        FPackLogger.Success('Upload completed.')
      else
        FPackLogger.Error('Upload completed with errors.');
      cleanup();
    end);
end;

procedure TDSpecCreatorForm.LoadUploadSettings;
var
  iniFile : TIniFile;
  lastSource : string;
  idx : integer;
begin
  iniFile := TIniFile.Create(MRUListService.GetIniFilePath);
  try
    chkUploadSkipDuplicate.Checked := iniFile.ReadBool('Upload', 'SkipDuplicate', true);
    rgUploadScope.ItemIndex := iniFile.ReadInteger('Upload', 'Scope', 0);
    lastSource := iniFile.ReadString('Upload', 'Source', '');
  finally
    iniFile.Free;
  end;
  if lastSource <> '' then
  begin
    idx := cboUploadSource.Items.IndexOf(lastSource);
    if idx >= 0 then
      cboUploadSource.ItemIndex := idx;
  end;
  // Programmatic selection does not raise OnChange - init the current source and
  // load its api key from the credential manager.
  FUploadCurrentSource := cboUploadSource.Text;
  if FUploadCurrentSource <> '' then
    edtUploadApiKey.Text := ReadSecret(UploadApiKeyCredTarget(FUploadCurrentSource));
  RefreshUploadPackages;
end;

procedure TDSpecCreatorForm.SaveUploadSettings;
var
  iniFile : TIniFile;
begin
  iniFile := TIniFile.Create(MRUListService.GetIniFilePath);
  try
    iniFile.WriteString('Upload', 'Source', cboUploadSource.Text);
    iniFile.WriteBool('Upload', 'SkipDuplicate', chkUploadSkipDuplicate.Checked);
    iniFile.WriteInteger('Upload', 'Scope', rgUploadScope.ItemIndex);
  finally
    iniFile.Free;
  end;
  // The api key (per source) goes to the credential manager, never the ini.
  if cboUploadSource.Text <> '' then
    WriteSecret(UploadApiKeyCredTarget(cboUploadSource.Text), edtUploadApiKey.Text);
end;

procedure TDSpecCreatorForm.clbCompilersClick(Sender : TObject);
var
  vPlatform : ISpecTargetPlatform;
  compilerVersion : TCompilerVersion;
  platform : TDPMPlatform;
  checked : boolean;
  index : integer;
  sDebug : string;
  pair : TPair<string,string>;
begin
  FInVariableUpdate := true;
  try
    VariablesList.Strings.Clear;
  finally
    FInVariableUpdate := false;
  end;
  index := clbCompilers.ItemIndex;

  sDebug := 'Index [' + IntToStr(index) + ']';

  if index < 0 then
  begin
    OutputDebugString(PChar(sDebug));
  //  cboTemplate.ItemIndex := -1;
    clbPlatforms.CheckAll(cbUnchecked);
    EnableControls(false);
    Exit;
  end;

  checked := clbCompilers.Checked[index];
  sDebug := sDebug + ' Checked [' + BoolToStr(checked, true) + ']';
  OutputDebugString(PChar(sDebug));

  clbPlatforms.CheckAll(cbUnchecked);
  EnableControls(checked);

  vPlatform := FOpenFile.GetPlatform(clbCompilers.Items[index]);
  compilerVersion := StringToCompilerVersion(clbCompilers.Items[index]);

  lblPlatform.Caption := clbCompilers.Items[index] + ' - Platforms';
  lblTemplate.Caption := clbCompilers.Items[index] + ' - Template';
  EnableDisablePlatform(compilerVersion);
  // these default to disabled until a compiler version is selected.
  // if the compiler version is unchecked then the controls will be disabled.

  clbPlatforms.CheckAll(cbUnchecked);

  if checked and not Assigned(vPlatform) then
    vPlatform := FOpenFile.AddCompiler(clbCompilers.Items[index]);

  if not checked then
    FOpenFile.DeleteCompiler(clbCompilers.Items[index]);

  if vPlatform = nil then
  begin
    cboTemplate.ItemIndex := -1;
    Exit;
  end;

  if checked then
  begin
    for platform in vPlatform.Platforms do
    begin
      var platformName := DPMPlatformToString(platform);
      var i := clbPlatforms.Items.IndexOf(platformName);
      if i <> -1 then
        clbPlatforms.Checked[i] := true;
    end;

    if vPlatform.TemplateName = cUnset then
      vPlatform.TemplateName := 'default';
  end;

  FInVariableUpdate := true;
  try
    VariablesList.Strings.Clear;
    for pair in vPlatform.Variables do
      VariablesList.Strings.Add(pair.Key + '=' + pair.Value);
  finally
    FInVariableUpdate := false;
  end;

  cboTemplate.Clear;
  LoadTemplates;

  cboTemplate.ItemIndex := cboTemplate.Items.IndexOf(vPlatform.TemplateName);
end;

procedure TDSpecCreatorForm.clbCompilersKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #32 then
    clbCompilersClick(Sender);
end;

procedure TDSpecCreatorForm.clbPlatformsClickCheck(Sender : TObject);
var
  vPlatform : ISpecTargetPlatform;
  compiler : string;
begin
  if clbCompilers.ItemIndex < 0 then
  begin
    raise Exception.Create('You must select a compiler before you can select platforms');
  end;

  compiler := clbCompilers.Items[clbCompilers.ItemIndex];
  vPlatform := FOpenFile.GetPlatform(compiler);
  if vPlatform = nil then
    Exit;

  vPlatform.Platforms := GetCheckListPlatforms(clbPlatforms);
end;

procedure TDSpecCreatorForm.SetCheckListPlatforms(const clb : TCheckListBox; const platforms : TDPMPlatforms);
var
  i : integer;
  dpmPlatform : TDPMPlatform;
begin
  for i := 0 to clb.Count - 1 do
  begin
    dpmPlatform := StringToDPMPlatform(clb.Items[i]);
    clb.Checked[i] := (dpmPlatform <> TDPMPlatform.UnknownPlatform) and (dpmPlatform in platforms);
  end;
end;

function TDSpecCreatorForm.GetCheckListPlatforms(const clb : TCheckListBox) : TDPMPlatforms;
var
  i : integer;
  dpmPlatform : TDPMPlatform;
begin
  result := [];
  for i := 0 to clb.Count - 1 do
  begin
    if not clb.Checked[i] then
      continue;
    dpmPlatform := StringToDPMPlatform(clb.Items[i]);
    if dpmPlatform <> TDPMPlatform.UnknownPlatform then
      Include(result, dpmPlatform);
  end;
end;

procedure TDSpecCreatorForm.CreatingPackages1Click(Sender : TObject);
begin
  UriClick('https://docs.delphi.dev/getting-started/creating-packages.html');
end;

procedure TDSpecCreatorForm.edtAuthorChange(Sender : TObject);
begin
  FOpenFile.PackageSpec.metadata.authors.Clear;
  if edtAuthor.Text <> '' then
    FOpenFile.PackageSpec.metadata.authors.Add(edtAuthor.Text);
end;

procedure TDSpecCreatorForm.edtBuildDefinesChange(Sender : TObject);
begin
  if FLoadingCard then
    Exit;
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).build.Defines := edtBuildDefines.Text;
end;

procedure TDSpecCreatorForm.clbBuildPlatformsClickCheck(Sender : TObject);
begin
  if FLoadingCard then
    Exit;
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).build.Platforms := GetCheckListPlatforms(clbBuildPlatforms);
end;

procedure TDSpecCreatorForm.btnAddBuildRefClick(Sender : TObject);
var
  reference : string;
  build : ISpecBuildEntry;
begin
  if not Assigned(tvTemplates.Selected) then
    Exit;
  build := (tvTemplates.Selected as TTemplateTreeNode).build;
  if not Assigned(build) then
    Exit;
  reference := InputBox('Add Reference', 'Package name to require', '');
  if reference = '' then
    Exit;
  build.References.Add(reference);
  lbBuildReferences.Items.Add(reference);
end;

procedure TDSpecCreatorForm.btnDeleteBuildRefClick(Sender : TObject);
var
  reference : string;
  itemToDelete : integer;
  build : ISpecBuildEntry;
begin
  if lbBuildReferences.ItemIndex < 0 then
    Exit;
  if not Assigned(tvTemplates.Selected) then
    Exit;
  build := (tvTemplates.Selected as TTemplateTreeNode).build;
  if not Assigned(build) then
    Exit;
  reference := lbBuildReferences.Items[lbBuildReferences.ItemIndex];
  lbBuildReferences.DeleteSelected;
  itemToDelete := build.References.IndexOf(reference);
  if itemToDelete >= 0 then
    build.References.Delete(itemToDelete);
end;

procedure TDSpecCreatorForm.edtDesignProjectChange(Sender : TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    if not FLoadingCard then
    begin
      (tvTemplates.Selected as TTemplateTreeNode).designEntry.Project := edtDesignProject.Text;
      (tvTemplates.Selected as TTemplateTreeNode).Text := edtDesignProject.Text;
    end;

    str := 'Possible Expanded Paths:' + System.sLineBreak;
    for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if compiler = TCompilerVersion.UnknownVersion then
        continue;
      str := str + System.sLineBreak + ReplaceVars(edtDesignProject.Text, compiler);
    end;
    edtDesignProject.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.edtDesignDefinesChange(Sender : TObject);
begin
  if FLoadingCard then
    Exit;
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).designEntry.Defines := edtDesignDefines.Text;
end;

procedure TDSpecCreatorForm.clbDesignPlatformsClickCheck(Sender : TObject);
begin
  if FLoadingCard then
    Exit;
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).designEntry.Platforms := GetCheckListPlatforms(clbDesignPlatforms);
end;

procedure TDSpecCreatorForm.btnAddDesignRefClick(Sender : TObject);
var
  reference : string;
  design : ISpecDesignEntry;
begin
  if not Assigned(tvTemplates.Selected) then
    Exit;
  design := (tvTemplates.Selected as TTemplateTreeNode).designEntry;
  if not Assigned(design) then
    Exit;
  reference := InputBox('Add Reference', 'Package name to require', '');
  if reference = '' then
    Exit;
  design.References.Add(reference);
  lbDesignReferences.Items.Add(reference);
end;

procedure TDSpecCreatorForm.btnDeleteDesignRefClick(Sender : TObject);
var
  reference : string;
  itemToDelete : integer;
  design : ISpecDesignEntry;
begin
  if lbDesignReferences.ItemIndex < 0 then
    Exit;
  if not Assigned(tvTemplates.Selected) then
    Exit;
  design := (tvTemplates.Selected as TTemplateTreeNode).designEntry;
  if not Assigned(design) then
    Exit;
  reference := lbDesignReferences.Items[lbDesignReferences.ItemIndex];
  lbDesignReferences.DeleteSelected;
  itemToDelete := design.References.IndexOf(reference);
  if itemToDelete >= 0 then
    design.References.Delete(itemToDelete);
end;

procedure TDSpecCreatorForm.edtLibPrefixChange(Sender : TObject);
begin
  if FLoadingCard then
    Exit;
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).designEntry.LibPrefix := edtLibPrefix.Text;
end;

procedure TDSpecCreatorForm.edtLibSuffixChange(Sender : TObject);
begin
  if FLoadingCard then
    Exit;
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).designEntry.LibSuffix := edtLibSuffix.Text;
end;

procedure TDSpecCreatorForm.edtLibVersionChange(Sender : TObject);
begin
  if FLoadingCard then
    Exit;
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).designEntry.LibVersion := edtLibVersion.Text;
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

procedure TDSpecCreatorForm.edtFileEntryDestChange(Sender : TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    if not FLoadingCard then
      (tvTemplates.Selected as TTemplateTreeNode).sourceEntry.Destination := edtFileEntryDest.Text;

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

function TDSpecCreatorForm.LoadSourceNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const sourceEntry : ISpecSourceEntry) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, sourceEntry.Source) as TTemplateTreeNode;
  result.sourceEntry := sourceEntry;
  result.NodeType := ntSource;
  result.Template := template;
  result.ImageIndex := 2;
  result.SelectedIndex := 2;
  result.AddAction := actAddSourceItem;
  result.DeleteAction := actDeleteSourceItem;
end;

procedure TDSpecCreatorForm.LoadSourceNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecSourceEntry>);
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

function TDSpecCreatorForm.LoadBuildNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const buildEntry : ISpecBuildEntry) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, buildEntry.Project) as TTemplateTreeNode;
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
  BuildForm : TBuildForm;
  projectName : string;
  build : ISpecBuildEntry;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  buildNode : TTemplateTreeNode;
begin
  BuildForm := TBuildForm.Create(nil);
  try
    BuildForm.Caption := 'Add Build entry';
    if BuildForm.ShowModal = mrCancel then
      Exit;
    projectName := BuildForm.edtProject.Text;
    if projectName.IsEmpty then
      Exit;
    build := FTemplate.NewBuildEntry(projectName);
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
    if DependencyForm.ShowModal = mrCancel then
      Exit;
    dependancyId := DependencyForm.edtDependencyId.Text;
    if dependancyId.IsEmpty then
      Exit;
    dependency := FTemplate.NewDependency(dependancyId);
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
  DesignForm : TBuildForm;
  projectName : string;
  design : ISpecDesignEntry;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  designNode : TTemplateTreeNode;
begin
  DesignForm := TBuildForm.Create(nil);
  try
    DesignForm.Caption := 'Add Design entry';
    if DesignForm.ShowModal = mrCancel then
      Exit;
    projectName := DesignForm.edtProject.Text;
    if projectName.IsEmpty then
      Exit;
    design := FTemplate.NewDesignEntry(projectName);
  finally
    FreeAndNil(DesignForm);
  end;

  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntDesignHeading);
  tvTemplates.Items.BeginUpdate;
  try
    designNode := LoadDesignNode(parentNode, FTemplate, design);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := designNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;
end;

procedure TDSpecCreatorForm.actAddSourceItemExecute(Sender : TObject);
var
  SourceSrc : string;
  SourceForm : TSourceForm;
  Source : ISpecSourceEntry;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  sourceNode : TTemplateTreeNode;
begin
  SourceForm := TSourceForm.Create(nil);
  try
    SourceForm.Caption := 'Add Source Item';

    if SourceForm.ShowModal = mrCancel then
      Exit;
    SourceSrc := SourceForm.edtSource.Text;
    if SourceSrc.IsEmpty then
      Exit;

    Source := FTemplate.NewSource(SourceSrc);
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
  FOpenFile.ClearCompilers;
  clbCompilers.CheckAll(cbUnchecked);
  clbCompilersClick(clbPlatforms);
end;

procedure TDSpecCreatorForm.actCompilersSelectAllExecute(Sender : TObject);
var
  i : integer;
  vPlatform : ISpecTargetPlatform;
begin
  clbCompilers.CheckAll(TCheckBoxState.cbChecked);
  for i := 0 to clbCompilers.Count -1 do
  begin
    vPlatform := FOpenFile.GetPlatform(clbCompilers.Items[i]);
    if vPlatform = nil then
      vPlatform := FOpenFile.AddCompiler(clbCompilers.Items[i]);
    if vPlatform.TemplateName = cUnset then
      vPlatform.TemplateName := 'default';
  end;

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

procedure TDSpecCreatorForm.actAddPackageDefItemExecute(Sender : TObject);
var
  PackageDefForm : TBuildForm;
  projectName : string;
  packageDef : ISpecPackageDefinition;
  templateNode : TTemplateTreeNode;
  parentNode : TTemplateTreeNode;
  packageDefNode : TTemplateTreeNode;
begin
  PackageDefForm := TBuildForm.Create(nil);
  try
    PackageDefForm.Caption := 'Add Package Definition';
    if PackageDefForm.ShowModal = mrCancel then
      Exit;
    projectName := PackageDefForm.edtProject.Text;
    if projectName.IsEmpty then
      Exit;
    packageDef := FTemplate.NewPackageDefinition(projectName);
    packageDef.Kind := 'runtime';
    packageDef.Requires.Add('rtl');
  finally
    FreeAndNil(PackageDefForm);
  end;

  templateNode := FindTemplateNode(FTemplate);
  parentNode := FindHeadingNode(templateNode, ntPackageDefsHeading);
  tvTemplates.Items.BeginUpdate;
  try
    packageDefNode := LoadPackageDefNode(parentNode, FTemplate, packageDef);
    templateNode.Expanded := true;
    parentNode.Expand(false);
    tvTemplates.Selected := packageDefNode;
  finally
    tvTemplates.Items.EndUpdate;
  end;
end;

procedure TDSpecCreatorForm.actDeletePackageDefItemExecute(Sender : TObject);
begin
  DeleteSelectedEntry;
end;

procedure TDSpecCreatorForm.edtPackageDefProjectChange(Sender : TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    if not FLoadingCard then
    begin
      (tvTemplates.Selected as TTemplateTreeNode).packageDef.Project := edtPackageDefProject.Text;
      (tvTemplates.Selected as TTemplateTreeNode).Text := edtPackageDefProject.Text;
    end;

    str := 'Possible Expanded Paths:' + System.sLineBreak;
    for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if compiler = TCompilerVersion.UnknownVersion then
        continue;
      str := str + System.sLineBreak + ReplaceVars(edtPackageDefProject.Text, compiler);
    end;
    edtPackageDefProject.Hint := str;
  end;
end;

procedure TDSpecCreatorForm.cboPackageDefKindChange(Sender : TObject);
begin
  if FLoadingCard then
    Exit;
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).packageDef.Kind := Trim(cboPackageDefKind.Text);
end;

procedure TDSpecCreatorForm.clbPackageDefPlatformsClickCheck(Sender : TObject);
begin
  if FLoadingCard then
    Exit;
  if Assigned(tvTemplates.Selected) then
    (tvTemplates.Selected as TTemplateTreeNode).packageDef.Platforms := GetCheckListPlatforms(clbPackageDefPlatforms);
end;

procedure TDSpecCreatorForm.btnAddPackageDefFileClick(Sender : TObject);
var
  fileGlob : string;
  packageDef : ISpecPackageDefinition;
begin
  if not Assigned(tvTemplates.Selected) then
    Exit;
  packageDef := (tvTemplates.Selected as TTemplateTreeNode).packageDef;
  if not Assigned(packageDef) then
    Exit;
  fileGlob := InputBox('Add File', 'File glob pattern to include', '');
  if fileGlob = '' then
    Exit;
  packageDef.Files.Add(fileGlob);
  lbPackageDefFiles.Items.Add(fileGlob);
end;

procedure TDSpecCreatorForm.btnDeletePackageDefFileClick(Sender : TObject);
var
  fileGlob : string;
  itemToDelete : integer;
  packageDef : ISpecPackageDefinition;
begin
  if lbPackageDefFiles.ItemIndex < 0 then
    Exit;
  if not Assigned(tvTemplates.Selected) then
    Exit;
  packageDef := (tvTemplates.Selected as TTemplateTreeNode).packageDef;
  if not Assigned(packageDef) then
    Exit;
  fileGlob := lbPackageDefFiles.Items[lbPackageDefFiles.ItemIndex];
  lbPackageDefFiles.DeleteSelected;
  itemToDelete := packageDef.Files.IndexOf(fileGlob);
  if itemToDelete >= 0 then
    packageDef.Files.Delete(itemToDelete);
end;

procedure TDSpecCreatorForm.btnAddPackageDefExcludeClick(Sender : TObject);
var
  excludeGlob : string;
  packageDef : ISpecPackageDefinition;
begin
  if not Assigned(tvTemplates.Selected) then
    Exit;
  packageDef := (tvTemplates.Selected as TTemplateTreeNode).packageDef;
  if not Assigned(packageDef) then
    Exit;
  excludeGlob := InputBox('Add Exclude', 'File-name glob pattern to exclude', '');
  if excludeGlob = '' then
    Exit;
  packageDef.Exclude.Add(excludeGlob);
  lbPackageDefExclude.Items.Add(excludeGlob);
end;

procedure TDSpecCreatorForm.btnDeletePackageDefExcludeClick(Sender : TObject);
var
  excludeGlob : string;
  itemToDelete : integer;
  packageDef : ISpecPackageDefinition;
begin
  if lbPackageDefExclude.ItemIndex < 0 then
    Exit;
  if not Assigned(tvTemplates.Selected) then
    Exit;
  packageDef := (tvTemplates.Selected as TTemplateTreeNode).packageDef;
  if not Assigned(packageDef) then
    Exit;
  excludeGlob := lbPackageDefExclude.Items[lbPackageDefExclude.ItemIndex];
  lbPackageDefExclude.DeleteSelected;
  itemToDelete := packageDef.Exclude.IndexOf(excludeGlob);
  if itemToDelete >= 0 then
    packageDef.Exclude.Delete(itemToDelete);
end;

procedure TDSpecCreatorForm.btnAddPackageDefRequireClick(Sender : TObject);
var
  reference : string;
  i : integer;
  packageDef : ISpecPackageDefinition;
begin
  if not Assigned(tvTemplates.Selected) then
    Exit;
  packageDef := (tvTemplates.Selected as TTemplateTreeNode).packageDef;
  if not Assigned(packageDef) then
    Exit;
  reference := Trim(InputBox('Add Require', 'Package name to require', ''));
  if reference = '' then
    Exit;
  // requires are package names - reject case-insensitive duplicates (e.g. a second rtl).
  for i := 0 to packageDef.Requires.Count - 1 do
    if SameText(Trim(packageDef.Requires[i]), reference) then
    begin
      ShowMessage(Format('"%s" is already in the requires list.', [reference]));
      Exit;
    end;
  packageDef.Requires.Add(reference);
  lbPackageDefRequires.Items.Add(reference);
end;

procedure TDSpecCreatorForm.btnDeletePackageDefRequireClick(Sender : TObject);
var
  reference : string;
  itemToDelete : integer;
  packageDef : ISpecPackageDefinition;
begin
  if lbPackageDefRequires.ItemIndex < 0 then
    Exit;
  if not Assigned(tvTemplates.Selected) then
    Exit;
  packageDef := (tvTemplates.Selected as TTemplateTreeNode).packageDef;
  if not Assigned(packageDef) then
    Exit;
  reference := lbPackageDefRequires.Items[lbPackageDefRequires.ItemIndex];
  lbPackageDefRequires.DeleteSelected;
  itemToDelete := packageDef.Requires.IndexOf(reference);
  if itemToDelete >= 0 then
    packageDef.Requires.Delete(itemToDelete);
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
  FOpenFile.PackageSpec.Templates.Add(newTemplate);

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
  FOpenFile.PackageSpec.newTemplate('default');
  UpdateFormCaption('');
  EnableControls(false);
  PageControl.ActivePageIndex :=  0;
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

  actAddPackageDefItem.Enabled := hasNode and (selectedNode.IsPackageDefHeading or selectedNode.IsPackageDef);
  actDeletePackageDefItem.Enabled := hasNode and selectedNode.IsPackageDef;

  actAddSourceItem.Enabled := hasNode and (selectedNode.IsSourceHeading or selectedNode.IsSource);
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

function TDSpecCreatorForm.LoadDesignNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const item : ISpecDesignEntry) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, item.Project) as TTemplateTreeNode;
  result.designEntry := item;
  result.NodeType := ntDesign;
  result.Template := template;
  result.ImageIndex := 6;
  result.SelectedIndex := 6;
end;

procedure TDSpecCreatorForm.LoadDesignNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const fileList : IList<ISpecDesignEntry>);
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
    LoadDesignNode(designtimeNode, template, fileList[j]);
end;

function TDSpecCreatorForm.LoadPackageDefNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const item : ISpecPackageDefinition) : TTemplateTreeNode;
begin
  result := tvTemplates.Items.AddChild(parentNode, item.Project) as TTemplateTreeNode;
  result.packageDef := item;
  result.NodeType := ntPackageDef;
  result.Template := template;
  result.ImageIndex := 6;
  result.SelectedIndex := 6;
end;

procedure TDSpecCreatorForm.LoadPackageDefNodes(const parentNode : TTemplateTreeNode; const template : ISpecTemplate; const defList : IList<ISpecPackageDefinition>);
var
  packageDefsNode : TTemplateTreeNode;
  j : integer;
begin
  packageDefsNode := tvTemplates.Items.AddChild(parentNode, 'Package Definitions') as TTemplateTreeNode;
  packageDefsNode.Template := template;
  packageDefsNode.NodeType := ntPackageDefsHeading;
  packageDefsNode.ImageIndex := 6;
  packageDefsNode.SelectedIndex := 6;

  packageDefsNode.AddAction := actAddPackageDefItem;
  packageDefsNode.DeleteAction := actDeletePackageDefItem;

  for j := 0 to defList.Count - 1 do
    LoadPackageDefNode(packageDefsNode, template, defList[j]);
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

procedure TDSpecCreatorForm.LoadEnvironmentVariablesNode(const parentNode : TTemplateTreeNode; const template : ISpecTemplate);
var
  envNode : TTemplateTreeNode;
begin
  //Heading-only node - the key/value pairs are edited directly in envVariablesList (a
  //TValueListEditor), so there are no child item nodes and no Add/Delete actions.
  envNode := tvTemplates.Items.AddChild(parentNode, 'Environment Variables') as TTemplateTreeNode;
  envNode.Template := template;
  envNode.NodeType := ntEnvironmentVariablesHeading;
  envNode.ImageIndex := 4;
  envNode.SelectedIndex := 4;
end;

function TDSpecCreatorForm.LoadTemplate(const template : ISpecTemplate) : TTemplateTreeNode;
var
  Node : TTemplateTreeNode;
begin
  Node := AddRootTemplateNode(template);
  result := Node;

  LoadSourceNodes(Node, template, template.SourceEntries);
  LoadBuildNodes(Node, template);
  LoadDesignNodes(Node, template, template.DesignEntries);
  LoadPackageDefNodes(Node, template, template.PackageDefinitions);
  LoadDependencies(Node, template);
  LoadEnvironmentVariablesNode(Node, template);
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

    for i := 0 to FOpenFile.PackageSpec.Templates.Count - 1 do
    begin
      template := FOpenFile.PackageSpec.Templates[i];
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
  FOpenFile.PackageSpec.metadata.id := edtId.Text;
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
    if not FLoadingCard then
    begin
      (tvTemplates.Selected as TTemplateTreeNode).build.project := edtProject.Text;
      (tvTemplates.Selected as TTemplateTreeNode).Text := edtProject.Text;
    end;

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
  FOpenFile.PackageSpec.metadata.projectUrl := edtProjectURL.Text;
end;

procedure TDSpecCreatorForm.edtReadmeChange(Sender: TObject);
begin
  FOpenFile.PackageSpec.metadata.Readme := edtReadme.Text;
end;

procedure TDSpecCreatorForm.edtRepositoryCommitChange(Sender: TObject);
begin
  FOpenFile.PackageSpec.metadata.RepositoryCommit := edtRepositoryCommit.Text;
end;

procedure TDSpecCreatorForm.edtRepositoryURLChange(Sender : TObject);
begin
  FOpenFile.PackageSpec.metadata.repositoryUrl := edtRepositoryURL.Text;
end;

function TDSpecCreatorForm.ReplaceVars(const inputStr : String; compiler : TCompilerVersion) : string;
begin
  result := TClassReplacer.ReplaceVars(inputStr, compiler, FOpenFile.PackageSpec);
end;

procedure TDSpecCreatorForm.edtFileEntrySourceChange(Sender : TObject);
var
  str : string;
  compiler : TCompilerVersion;
begin
  if Assigned(tvTemplates.Selected) then
  begin
    if not FLoadingCard then
    begin
      (tvTemplates.Selected as TTemplateTreeNode).sourceEntry.Source := edtFileEntrySource.Text;
      (tvTemplates.Selected as TTemplateTreeNode).Text := edtFileEntrySource.Text;
    end;

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
  FOpenFile.PackageSpec.metadata.tags.CommaText := edtTags.Text;
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
    FOpenFile.PackageSpec.RenameTemplate(templateName, edtTemplateName.Text);
  end;
end;

procedure TDSpecCreatorForm.edtVersionChange(Sender : TObject);
var
  Version : TPackageVersion;
begin
  if Length(edtVersion.Text) > 0 then
  begin
    if TPackageVersion.TryParse(edtVersion.Text, Version) then
      FOpenFile.PackageSpec.metadata.Version := Version;
  end;
end;

procedure TDSpecCreatorForm.edtVersionExit(Sender : TObject);
begin
  if Length(edtVersion.Text) > 0 then
    FOpenFile.PackageSpec.metadata.Version := TPackageVersion.Parse(edtVersion.Text);
end;

procedure TDSpecCreatorForm.FormCreate(Sender : TObject);
var
  idx : integer;
  iniFile : TIniFile;
  cs : TControlStyle;
begin
  FLogger := TDSpecLogger.Create(Memo2.Lines);
  FOpenFile := TDSpecFile.Create(FLogger);
  FOpenFile.PackageSpec.newTemplate('default');
  // Pack / sign / upload run in-process via the DPM core; output goes to the
  // active page's memo (retargeted per operation). Pack/sign default to Memo1.
  FOpsLogger := TDSpecQueuedLogger.Create(Memo1.Lines);
  FPackLogger := FOpsLogger;
  InitCoreContainer;
  FPacking := false;
  FUploading := false;
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

  //double clicks really mess with the state of the checkboxes.
  cs := clbCompilers.ControlStyle;
  Exclude(cs, csDoubleClicks);
  clbCompilers.ControlStyle := cs;

  iniFile := TIniFile.Create(MRUListService.GetIniFilePath);
  try
    edtPackageOutputPath.Text := iniFile.ReadString('Paths', 'PackageOutput', '');
  finally
    iniFile.Free;
  end;

  // Signing tab: hide the raw page tabs (the provider combo drives the page) and
  // restore saved settings.
  TabSheet1.TabVisible := false;
  TabSheet2.TabVisible := false;
  TabSheet3.TabVisible := false;
  TabSheet4.TabVisible := false;
  LoadSigningSettings;
  pnlProviders.Enabled := chkEnableSigning.Checked;
  UpdateSigningProviderPage;

  // Upload tab: load configured sources and restore saved settings.
  LoadUploadSources;
  LoadUploadSettings;
  UpdateUploadApiKeyState;
end;

procedure TDSpecCreatorForm.FormDestroy(Sender : TObject);
begin
  SaveSigningSettings;
  SaveUploadSettings;
  MRUListService.SetSource(nil);
  if FCancellationTokenSource <> nil then
    FCancellationTokenSource.Cancel;
  FSPDXList.Free;
  // Interfaces (FPackageWriter etc.) release with the container.
  FreeAndNil(FContainer);
end;

procedure TDSpecCreatorForm.lblSPDXClick(Sender : TObject);
begin
  UriClick('https://spdx.org/licenses/');
end;

procedure TDSpecCreatorForm.LoadDspecStructure;
var
  i : integer;
  j : integer;
  compilerVersion : TCompilerVersion;
  pair : TPair<string,string>;
begin
  edtId.Text := FOpenFile.PackageSpec.metadata.id;
  edtVersion.Text := FOpenFile.PackageSpec.metadata.Version.ToString;
  mmoDescription.Text := FOpenFile.PackageSpec.metadata.Description;
  edtProjectURL.Text := FOpenFile.PackageSpec.metadata.projectUrl;
  edtRepositoryURL.Text := FOpenFile.PackageSpec.metadata.repositoryUrl;
  edtRepositoryCommit.Text := FOpenFile.PackageSpec.metadata.RepositoryCommit;
  if FOpenFile.PackageSpec.metadata.authors.Count > 0 then
    edtAuthor.Text := FOpenFile.PackageSpec.metadata.authors[0]
  else
    edtAuthor.Text := '';
  edtCopyright.Text := FOpenFile.PackageSpec.metadata.Copyright;
  cboLicense.Text := FOpenFile.PackageSpec.metadata.license;
  edtTags.Text := FOpenFile.PackageSpec.metadata.tags.CommaText;
  edtReadme.Text := FOpenFile.PackageSpec.metadata.Readme;
  if Length(FOpenFile.PackageSpec.metadata.Icon) > 0 then
  begin
    ImgIcon.Picture.LoadFromFile(TPath.Combine(FOpenFile.WorkingDir, FOpenFile.PackageSpec.metadata.Icon));
  end;

  //package-level (global) variables - shared across all compilers, overridden by per-platform vars.
  FInVariableUpdate := true;
  try
    PackageVariablesList.Strings.Clear;
    for pair in FOpenFile.PackageSpec.Variables do
      PackageVariablesList.Strings.Add(pair.Key + '=' + pair.Value);
  finally
    FInVariableUpdate := false;
  end;

  cboTemplate.Text := '';

  CardPanel.Visible := false;

  //Check each compiler box that any targetPlatform covers. Using IsForCompiler handles all three
  //authoring forms (single compiler, compilers list, compiler from/to range) and avoids the
  //CompilerToString ('delphi12.0') vs checklist item ('12.0') naming mismatch.
  for j := 0 to clbCompilers.Count - 1 do
  begin
    compilerVersion := StringToCompilerVersion(clbCompilers.Items[j]);
    clbCompilers.Checked[j] := false;
    for i := 0 to FOpenFile.PackageSpec.targetPlatforms.Count - 1 do
    begin
      if FOpenFile.PackageSpec.targetPlatforms[i].IsForCompiler(compilerVersion) then
      begin
        clbCompilers.Checked[j] := true;
        break;
      end;
    end;
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
    FOpenFile.PackageSpec.metadata.Icon := relativePath;
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

procedure TDSpecCreatorForm.actFilePackageWizardExecute(Sender : TObject);
var
  wizard : TPackageWizardForm;
begin
  wizard := TPackageWizardForm.Create(nil, FLogger, FConfigManager);
  try
    if (wizard.ShowModal = mrOk) and (wizard.ResultFile <> '') then
      OpenProject(wizard.ResultFile);
  finally
    FreeAndNil(wizard);
  end;
end;

procedure TDSpecCreatorForm.mmoDescriptionChange(Sender : TObject);
begin
  FOpenFile.PackageSpec.metadata.Description := mmoDescription.Text;
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
    FOpenFile.PackageSpec.newTemplate('default');
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

  FLoadingCard := true;
  try
    case lNode.NodeType of
      ntTemplateHeading :
        begin
          edtTemplateName.Text := lNode.Template.name;
          CardPanel.ActiveCard := crdTemplate;
        end;
      ntBuildHeading  :      CardPanel.ActiveCard := crdBuildHeading;
      ntDesignHeading :      CardPanel.ActiveCard := crdDesignHeading;
      ntSourceHeading :      CardPanel.ActiveCard := crdSourceHeading;
      ntDependencyHeading :  CardPanel.ActiveCard := crdDependenciesHeading;
      ntPackageDefsHeading : CardPanel.ActiveCard := crdPackageDefsHeading;
      ntBuild :
        begin
          edtProject.Text := lNode.build.project;
          edtBuildDefines.Text := lNode.build.Defines;
          SetCheckListPlatforms(clbBuildPlatforms, lNode.build.Platforms);
          lbBuildReferences.Clear;
          for i := 0 to lNode.build.References.Count - 1 do
            lbBuildReferences.Items.Add(lNode.build.References[i]);
          CardPanel.ActiveCard := crdBuild;
        end;
      ntDesign :
        begin
          edtDesignProject.Text := lNode.designEntry.Project;
          edtDesignDefines.Text := lNode.designEntry.Defines;
          SetCheckListPlatforms(clbDesignPlatforms, lNode.designEntry.Platforms);
          lbDesignReferences.Clear;
          for i := 0 to lNode.designEntry.References.Count - 1 do
            lbDesignReferences.Items.Add(lNode.designEntry.References[i]);
          edtLibPrefix.Text := lNode.designEntry.LibPrefix;
          edtLibSuffix.Text := lNode.designEntry.LibSuffix;
          edtLibVersion.Text := lNode.designEntry.LibVersion;
          CardPanel.ActiveCard := crdDesign;
        end;
      ntSource :
        begin
          edtFileEntrySource.Text := lNode.sourceEntry.Source;
          edtFileEntryDest.Text := lNode.sourceEntry.Destination;
          lbFileEntryExclude.Clear;
          for i := 0 to lNode.sourceEntry.exclude.Count - 1 do
            lbFileEntryExclude.Items.Add(lNode.sourceEntry.exclude[i]);
          lblSourceItemHeader.Caption := 'Source Files';
          CardPanel.ActiveCard := crdSource;
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
      ntPackageDef :
        begin
          edtPackageDefProject.Text := lNode.packageDef.Project;
          cboPackageDefKind.Text := lNode.packageDef.Kind;
          SetCheckListPlatforms(clbPackageDefPlatforms, lNode.packageDef.Platforms);
          lbPackageDefFiles.Clear;
          for i := 0 to lNode.packageDef.Files.Count - 1 do
            lbPackageDefFiles.Items.Add(lNode.packageDef.Files[i]);
          lbPackageDefExclude.Clear;
          for i := 0 to lNode.packageDef.Exclude.Count - 1 do
            lbPackageDefExclude.Items.Add(lNode.packageDef.Exclude[i]);
          lbPackageDefRequires.Clear;
          for i := 0 to lNode.packageDef.Requires.Count - 1 do
            lbPackageDefRequires.Items.Add(lNode.packageDef.Requires[i]);
          CardPanel.ActiveCard := crdPackageDef;
        end;
      ntEnvironmentVariablesHeading :
        begin
          FInVariableUpdate := true;
          try
            envVariablesList.Strings.Clear;
            for i := 0 to lNode.Template.EnvironmentVariables.Count - 1 do
              envVariablesList.Strings.Add(lNode.Template.EnvironmentVariables.Items[i].Key + '=' + lNode.Template.EnvironmentVariables.Items[i].Value);
          finally
            FInVariableUpdate := false;
          end;
          CardPanel.ActiveCard := crdEnvironmentVariables;
        end;
    else
      raise Exception.Create('Unknow node type in tvTemplateChange');
    end;
  finally
    FLoadingCard := false;
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

    //Some node types (e.g. the Environment Variables heading) have no Add/Delete actions - they are
    //edited inline. Only add a menu item when its action is assigned, and skip an empty popup.
    if Assigned(categoryNode.AddAction) then
    begin
      item := TMenuItem.Create(PopupMenu);
      item.Action := categoryNode.AddAction;
      tvTemplates.PopupMenu.Items.Add(item);
    end;

    if Assigned(categoryNode.DeleteAction) then
    begin
      item := TMenuItem.Create(PopupMenu);
      item.Action := categoryNode.DeleteAction;
      tvTemplates.PopupMenu.Items.Add(item);
    end;

    if tvTemplates.PopupMenu.Items.Count > 0 then
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
  FOpenFile.PackageSpec.RenameTemplate(Node.Text, S);
  edtTemplateName.Text := S;
end;

procedure TDSpecCreatorForm.tvTemplatesEditing(Sender : TObject; Node : TTreeNode; var AllowEdit : Boolean);
begin
  AllowEdit := (Node as TTemplateTreeNode).TemplateHeading;
end;

procedure TDSpecCreatorForm.VariablesListStringsChange(Sender : TObject);
var
  vPlatform : ISpecTargetPlatform;
  i : integer;
  key : string;
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
    for i := 0 to VariablesList.Strings.Count - 1 do
    begin
      key := VariablesList.Strings.Names[i];
      if key <> '' then
        vPlatform.Variables[key] := VariablesList.Strings.ValueFromIndex[i];
    end;
  end;
end;

procedure TDSpecCreatorForm.PackageVariablesListStringsChange(Sender : TObject);
var
  i : integer;
  key : string;
begin
  if FInVariableUpdate then
    Exit;
  FOpenFile.PackageSpec.Variables.Clear;
  for i := 0 to PackageVariablesList.Strings.Count - 1 do
  begin
    key := PackageVariablesList.Strings.Names[i];
    if key <> '' then
      FOpenFile.PackageSpec.Variables[LowerCase(key)] := PackageVariablesList.Strings.ValueFromIndex[i];
  end;
end;

procedure TDSpecCreatorForm.envVariablesListStringsChange(Sender : TObject);
var
  i : integer;
  key : string;
  lNode : TTemplateTreeNode;
begin
  if FInVariableUpdate or FLoadingCard then
    Exit;
  if not Assigned(tvTemplates.Selected) then
    Exit;
  lNode := tvTemplates.Selected as TTemplateTreeNode;
  if not Assigned(lNode.Template) then
    Exit;
  //Preserve the author's key casing (unlike package variables which are lower-cased) - IDE
  //environment variable names are echoed as written, e.g. SKIADIR.
  lNode.Template.EnvironmentVariables.Clear;
  for i := 0 to envVariablesList.Strings.Count - 1 do
  begin
    key := envVariablesList.Strings.Names[i];
    if key <> '' then
      lNode.Template.EnvironmentVariables[key] := envVariablesList.Strings.ValueFromIndex[i];
  end;
end;

procedure TDSpecCreatorForm.edtCopyrightChange(Sender : TObject);
begin
  FOpenFile.PackageSpec.metadata.Copyright := edtCopyright.Text;
end;

end.
