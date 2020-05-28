unit DPM.IDE.PackageDetailsFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Logging,
  DPM.IDE.PackageDetailsPanel,
  DPM.IDE.IconCache,
  DPM.IDE.Types,
  Spring.Container,
  Spring.Collections,
  VSoft.Awaitable;

type
  TPackageDetailsFrame = class(TFrame)
    sbPackageDetails: TScrollBox;
    pnlPackageId: TPanel;
    pnlInstalled: TPanel;
    lblPackageId: TLabel;
    imgPackageLogo: TImage;
    pnlVersion: TPanel;
    Label1: TLabel;
    txtInstalledVersion: TEdit;
    btnUninstall: TButton;
    lblVersionTitle: TLabel;
    cboVersions: TComboBox;
    btnInstallUpdate: TButton;
  private
    FContainer : TContainer;
    FIconCache : TDPMIconCache;
    FPackageMetaData : IPackageSearchResultItem;
    FDetailsPanel : TPackageDetailsPanel;
    FCurrentTab : TCurrentTab;
    FCancellationTokenSource : ICancellationTokenSource;
    FRequestInFlight : boolean;
    FVersionsDelayTimer : TTimer;
    FConfiguration : IConfiguration;
    FLogger : ILogger;
    FClosing : boolean;
  protected
    procedure VersionsDelayTimerEvent(Sender : TObject);
  public
    constructor Create(AOwner : TComponent);override;
    procedure Init(const container : TContainer; const iconCache : TDPMIconCache; const config : IConfiguration);
    procedure Configure(const value : TCurrentTab);
    procedure SetPackage(const package : IPackageSearchResultItem);
    procedure ViewClosing;
  end;

implementation

{$R *.dfm}

uses
  DPM.Core.Types,
  DPM.Core.Options.Search,
  DPM.Core.Dependency.Version,
  DPM.Core.Repository.Interfaces;

{ TPackageDetailsFrame }

procedure TPackageDetailsFrame.Configure(const value : TCurrentTab);
begin
  if FCurrentTab <> value then
  begin
    FPackageMetaData := nil;
    FDetailsPanel.SetDetails(nil);
    FCurrentTab := value;
    case FCurrentTab of
      TCurrentTab.Installed:
      begin
//        pnlInstalled.Visible := True;
        btnInstallUpdate.Caption := 'Update';
      end;
      TCurrentTab.Updates:
      begin
//        pnlInstalled.Visible := True;
        btnInstallUpdate.Caption := 'Update';

      end;
      TCurrentTab.Search:
      begin
//        pnlInstalled.Visible := false;
        btnInstallUpdate.Caption := 'Install';

      end;
      TCurrentTab.Conflicts: ;
    end;

  end;

end;

constructor TPackageDetailsFrame.Create(AOwner: TComponent);
begin
  inherited;
  FVersionsDelayTimer := TTimer.Create(AOwner);
  FVersionsDelayTimer.Interval := 200;
  FVersionsDelayTimer.Enabled := false;
  FVersionsDelayTimer.OnTimer := VersionsDelayTimerEvent;
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;

  FDetailsPanel := TPackageDetailsPanel.Create(AOwner);
  FDetailsPanel.Align := alTop;
  FDetailsPanel.Height := 200;
  FDetailsPanel.Color := clRed;
  FDetailsPanel.Top := pnlVErsion.Top + pnlVErsion.Top + 20;
  FDetailsPanel.Parent := sbPackageDetails;

end;

procedure TPackageDetailsFrame.Init(const container: TContainer; const iconCache: TDPMIconCache; const config : IConfiguration);
begin
  FContainer := container;
  FIconCache := iconCache;
  FConfiguration := config;
  FLogger := FContainer.Resolve<ILogger>;
end;


procedure TPackageDetailsFrame.SetPackage(const package: IPackageSearchResultItem);
var
  logo : TPngImage;
begin
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;
  FPackageMetaData := package;
  if package <> nil then
  begin
    lblPackageId.Caption := package.Id;
    logo := FIconCache.Request(package.Id);
    if logo = nil then
      logo := FIconCache.Request('missing_icon');
    if logo <> nil then
    begin
      imgPackageLogo.Picture.Assign(logo);
      imgPackageLogo.Visible := true;
    end
    else
      imgPackageLogo.Visible := false;

    pnlPackageId.Visible := true;
    pnlInstalled.Visible := package.Installed;
    if package.Installed then
      txtInstalledVersion.Text := package.InstalledVersion;
    pnlVersion.Visible := true;
    FVersionsDelayTimer.Enabled := true;
  end
  else
  begin
    pnlPackageId.Visible := false;
    pnlInstalled.Visible := false;
    pnlVErsion.Visible := false;
  end;

  FDetailsPanel.SetDetails(package);
end;

procedure TPackageDetailsFrame.VersionsDelayTimerEvent(Sender: TObject);
var
  versions : IList<TPackageVersion>;
  repoManager : IPackageRepositoryManager;
  options : TSearchOptions;
  config : IConfiguration;
begin
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;
  while FRequestInFlight do
    Application.ProcessMessages;
  FCancellationTokenSource.Reset;

  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
  options := TSearchOptions.Create;
  options.CompilerVersion := IDECompilerVersion;
  options.AllVersions := true;
  options.SearchTerms := FPackageMetaData.Id;

  config := FConfiguration;

  TAsync.Configure<IList<TPackageVersion>>(
        function (const cancelToken : ICancellationToken) : IList<TPackageVersion>
        begin
          //this is calling the wrong overload.
          result := repoManager.GetPackageVersions(cancelToken, options,config);
          //simulating long running.
        end,FCancellationTokenSource.Token)
        .OnException(
          procedure(const e : Exception)
          begin
            FRequestInFlight := false;
            if FClosing then
              exit;
            FLogger.Error(e.Message);
          end)
        .OnCancellation(
        procedure
        begin
          FRequestInFlight := false;
          //if the view is closing do not do anything else.
          if FClosing then
            exit;
          FLogger.Debug('Cancelled getting conflicting packages.');
        end)
        .Await(
          procedure(const theResult : IList<TPackageVersion>)
          var
            version : TPackageVersion;
          begin
            FRequestInFlight := false;
            //if the view is closing do not do anything else.
            if FClosing then
              exit;
            versions := theResult;
            FLogger.Debug('Got package versions .');
            cboVersions.Clear;

            if versions.Any then
            begin
              if options.Prerelease then
              begin
                version := versions.Where(
                  function(const value : TPackageVersion) : boolean
                  begin
                    result := value.IsStable;
                  end).FirstOrDefault;
                if not version.IsEmpty then
                  cboVersions.Items.Add('Latest Prerelease ' + version.ToStringNoMeta);

              end
              else
              begin
                version := versions.Where(
                  function(const value : TPackageVersion) : boolean
                  begin
                    result := value.IsStable;
                  end).FirstOrDefault;
                if not version.IsEmpty then
                  cboVersions.Items.Add('Latest Stable ' + version.ToStringNoMeta);

              end;
              for version in versions do
                cboVersions.Items.Add(version.ToStringNoMeta);
              cboVersions.ItemIndex := 0;
            end;

          end);

end;

procedure TPackageDetailsFrame.ViewClosing;
begin
  FClosing := true;
  FCancellationTokenSource.Cancel;
end;

end.
