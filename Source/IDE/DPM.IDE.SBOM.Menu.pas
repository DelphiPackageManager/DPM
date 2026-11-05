{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

unit DPM.IDE.SBOM.Menu;

{ IDE project-tree menu entry for "Generate SBOM".

  Right-clicking a .dproj or .groupproj in the IDE Project Manager exposes a
  "Generate SBOM" item alongside "Manage DPM Packages". Selecting it shows the
  Generate SBOM dialog (DPM.IDE.SBOMOptionsForm) - the product information,
  saved to the project's <name>.sbom.yaml sidecar when asked, and the formats
  and output folder - then runs the same ISBOMGenerator the CLI uses, against
  the project / group. Progress + completion show up in the standard DPM
  message tab.

  Kept synchronous - SBOM generation is fast for typical projects, and IDE
  operations stay on the main thread. }

interface

uses
  System.Classes,
  Spring.Container,
  ToolsAPI,
  DPM.Core.Logging;

type
  TDPMSBOMProjectMenuNotifier = class(TInterfacedObject, IOTAProjectMenuItemCreatorNotifier, IOTANotifier)
  private
    FContainer : TContainer;
    FLogger : ILogger;
  protected
    procedure AddMenu(const Project : IOTAProject; const IdentList : TStrings; const ProjectManagerMenuList : IInterfaceList; IsMultiSelect : Boolean);
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  public
    constructor Create(const container : TContainer; const logger : ILogger);
  end;

  //Minimal IOTAProjectManagerMenu impl whose GetCaption returns '-' so the IDE
  //renders it as a separator. Position is taken in the constructor so callers
  //can place it either side of the SBOM menu item.
  TDPMSBOMMenuSeparator = class(TInterfacedObject, IOTANotifier, IOTALocalMenu, IOTAProjectManagerMenu)
  private
    FPosition : integer;
    FName : string;
  protected
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    function GetCaption : string;
    function GetChecked : Boolean;
    function GetEnabled : Boolean;
    function GetHelpContext : Integer;
    function GetName : string;
    function GetParent : string;
    function GetPosition : Integer;
    function GetVerb : string;
    procedure SetCaption(const Value : string);
    procedure SetChecked(Value : Boolean);
    procedure SetEnabled(Value : Boolean);
    procedure SetHelpContext(Value : Integer);
    procedure SetName(const Value : string);
    procedure SetParent(const Value : string);
    procedure SetPosition(Value : Integer);
    procedure SetVerb(const Value : string);
    procedure Execute(const MenuContextList : IInterfaceList);
    function GetIsMultiSelectable : Boolean;
    function PostExecute(const MenuContextList : IInterfaceList) : Boolean;
    function PreExecute(const MenuContextList : IInterfaceList) : Boolean;
    procedure SetIsMultiSelectable(Value : Boolean);
  public
    //Each separator instance needs a unique Name + Position. The IDE de-dupes
    //by Name, and Position controls vertical order in the project-tree menu.
    constructor Create(const position : integer; const name : string);
  end;

  TDPMSBOMProjectMenu = class(TInterfacedObject, IOTANotifier, IOTALocalMenu, IOTAProjectManagerMenu)
  private
    FProject : IOTAProject;
    FProjectGroup : IOTAProjectGroup;
    FContainer : TContainer;
    FLogger : ILogger;
    FIsGroupContext : boolean;
  protected
    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    //IOTALocalMenu
    function GetCaption : string;
    function GetChecked : Boolean;
    function GetEnabled : Boolean;
    function GetHelpContext : Integer;
    function GetName : string;
    function GetParent : string;
    function GetPosition : Integer;
    function GetVerb : string;
    procedure SetCaption(const Value : string);
    procedure SetChecked(Value : Boolean);
    procedure SetEnabled(Value : Boolean);
    procedure SetHelpContext(Value : Integer);
    procedure SetName(const Value : string);
    procedure SetParent(const Value : string);
    procedure SetPosition(Value : Integer);
    procedure SetVerb(const Value : string);
    procedure Execute(const MenuContextList : IInterfaceList);
    function GetIsMultiSelectable : Boolean;
    function PostExecute(const MenuContextList : IInterfaceList) : Boolean;
    function PreExecute(const MenuContextList : IInterfaceList) : Boolean;
    procedure SetIsMultiSelectable(Value : Boolean);
  public
    constructor Create(const projectGroup : IOTAProjectGroup; const project : IOTAProject;
                       const container : TContainer; const logger : ILogger;
                       const isGroupContext : boolean);
    //Shared SBOM generation routine. Shows the Generate SBOM dialog for targetPath (a
    //.dproj or .groupproj), then resolves ISbomGenerator from the container and runs it
    //with the dialog's choices. Cancelling the dialog generates and writes nothing.
    //Called both from this project-tree menu's Execute and from the top-level
    //DPM menu in TDPMWizard, so the logic lives in one place.
    class procedure RunSBOM(const container : TContainer; const logger : ILogger; const targetPath : string); static;
  end;

implementation

uses
  System.SysUtils,
  System.UITypes,
  Vcl.Dialogs,
  VSoft.CancellationToken,
  DPM.Core.Options.SBOM,
  DPM.Core.SBOM.Interfaces,
  DPM.Core.SBOM.ProductInfo,
  DPM.Core.Utils.Config,
  DPM.IDE.Constants,
  DPM.IDE.Logger,
  DPM.IDE.Options,
  DPM.IDE.SBOMOptionsForm,
  DPM.Core.Diagnostics;

{ TDPMSBOMProjectMenuNotifier }

constructor TDPMSBOMProjectMenuNotifier.Create(const container : TContainer; const logger : ILogger);
begin
  FContainer := container;
  FLogger := logger;
end;

procedure TDPMSBOMProjectMenuNotifier.AddMenu(const Project : IOTAProject; const IdentList : TStrings;
                                              const ProjectManagerMenuList : IInterfaceList; IsMultiSelect : Boolean);
var
//  separator : IOTAProjectManagerMenu;
  menu : IOTAProjectManagerMenu;
  projectGroup : IOTAProjectGroup;
  proj : IOTAProject;
  isGroupContext : boolean;
begin
  //We want different behaviour depending on whether the user right-clicked the
  //project-group root or an individual project:
  //  - group root  -> generate ONE aggregated SBOM for the whole .groupproj
  //  - single dproj -> generate an SBOM for that one project
  //The existing TDPMProjectMenuNotifier promotes proj := ActiveProject when the
  //right-click was on the group, which loses the distinction we need - so we DON'T
  //do that promotion here. Instead we leave proj=nil for group-context and let
  //TDPMSBOMProjectMenu decide based on which of project/projectGroup is set.
  if not (Assigned(Project) and
          ((IdentList.IndexOf(cDPMContainer) <> -1) or
           (IdentList.IndexOf(sProjectContainer) <> -1) or
           (IdentList.IndexOf(sProjectGroupContainer) <> -1))) then
    exit;

  proj := Project;
  projectGroup := nil;
  if (proj <> nil) and Supports(proj, IOTAProjectGroup, projectGroup) then
  begin
    //Right-clicked the project-group root - aggregated mode.
    proj := nil;
    isGroupContext := true;
  end
  else
  begin
    //Right-clicked a specific project - single-project mode. We still capture
    //the group so the generator can resolve sibling projects if needed later.
    projectGroup := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
    isGroupContext := false;
  end;
  Assert(projectGroup <> nil);

  //Visual separators bracket the Generate SBOM item so it sits in its own
  //group visually distinct from neighbouring menu sections. IOTAProjectManagerMenu
  //treats GetCaption = '-' as a separator - the IDE renders it as a horizontal
  //rule rather than a clickable item.
  //Layout:
  //  Manage DPM Packages        (pmmpBuildSection + 1)
  //  ---                         (pmmpBuildSection + 2)
  //  Generate SBOM              (pmmpBuildSection + 3)
  //  ---                         (pmmpBuildSection + 4)
//  separator := TDPMSBOMMenuSeparator.Create(pmmpBuildSection + 2, 'DPMSBOMSeparatorTop');
//  ProjectManagerMenuList.Add(separator);

  menu := TDPMSBOMProjectMenu.Create(projectGroup, proj, FContainer, FLogger, isGroupContext);
  ProjectManagerMenuList.Add(menu);

//  separator := TDPMSBOMMenuSeparator.Create(pmmpBuildSection + 4, 'DPMSBOMSeparatorBottom');
//  ProjectManagerMenuList.Add(separator);
end;

procedure TDPMSBOMProjectMenuNotifier.AfterSave;
begin
end;

procedure TDPMSBOMProjectMenuNotifier.BeforeSave;
begin
end;

procedure TDPMSBOMProjectMenuNotifier.Destroyed;
begin
  FContainer := nil;
  FLogger := nil;
end;

procedure TDPMSBOMProjectMenuNotifier.Modified;
begin
end;

{ TDPMSBOMProjectMenu }

constructor TDPMSBOMProjectMenu.Create(const projectGroup : IOTAProjectGroup; const project : IOTAProject;
                                       const container : TContainer; const logger : ILogger;
                                       const isGroupContext : boolean);
begin
  FProjectGroup := projectGroup;
  FProject := project;
  FContainer := container;
  FLogger := logger;
  FIsGroupContext := isGroupContext;
  Assert(FProjectGroup <> nil);
end;

{ TDPMSBOMMenuSeparator }

//Open Tools API renders an IOTAProjectManagerMenu whose GetCaption returns '-' as
//a horizontal separator line. None of the other IOTANotifier / IOTALocalMenu /
//IOTAProjectManagerMenu methods need meaningful behaviour for a separator.

constructor TDPMSBOMMenuSeparator.Create(const position : integer; const name : string);
begin
  inherited Create;
  FPosition := position;
  FName := name;
end;

procedure TDPMSBOMMenuSeparator.AfterSave;
begin
end;

procedure TDPMSBOMMenuSeparator.BeforeSave;
begin
end;

procedure TDPMSBOMMenuSeparator.Destroyed;
begin
end;

procedure TDPMSBOMMenuSeparator.Modified;
begin
end;

procedure TDPMSBOMMenuSeparator.Execute(const MenuContextList : IInterfaceList);
begin
end;

function TDPMSBOMMenuSeparator.GetCaption : string;
begin
  result := '-';
end;

function TDPMSBOMMenuSeparator.GetChecked : Boolean;
begin
  result := false;
end;

function TDPMSBOMMenuSeparator.GetEnabled : Boolean;
begin
  result := true;
end;

function TDPMSBOMMenuSeparator.GetHelpContext : Integer;
begin
  result := -1;
end;

function TDPMSBOMMenuSeparator.GetIsMultiSelectable : Boolean;
begin
  result := false;
end;

function TDPMSBOMMenuSeparator.GetName : string;
begin
  result := FName;
end;

function TDPMSBOMMenuSeparator.GetParent : string;
begin
  result := '';
end;

function TDPMSBOMMenuSeparator.GetPosition : Integer;
begin
  result := FPosition;
end;

function TDPMSBOMMenuSeparator.GetVerb : string;
begin
  result := FName;
end;

function TDPMSBOMMenuSeparator.PostExecute(const MenuContextList : IInterfaceList) : Boolean;
begin
  result := true;
end;

function TDPMSBOMMenuSeparator.PreExecute(const MenuContextList : IInterfaceList) : Boolean;
begin
  result := true;
end;

procedure TDPMSBOMMenuSeparator.SetCaption(const Value : string);
begin
end;

procedure TDPMSBOMMenuSeparator.SetChecked(Value : Boolean);
begin
end;

procedure TDPMSBOMMenuSeparator.SetEnabled(Value : Boolean);
begin
end;

procedure TDPMSBOMMenuSeparator.SetHelpContext(Value : Integer);
begin
end;

procedure TDPMSBOMMenuSeparator.SetIsMultiSelectable(Value : Boolean);
begin
end;

procedure TDPMSBOMMenuSeparator.SetName(const Value : string);
begin
end;

procedure TDPMSBOMMenuSeparator.SetParent(const Value : string);
begin
end;

procedure TDPMSBOMMenuSeparator.SetPosition(Value : Integer);
begin
end;

procedure TDPMSBOMMenuSeparator.SetVerb(const Value : string);
begin
end;

procedure TDPMSBOMProjectMenu.AfterSave;
begin
end;

procedure TDPMSBOMProjectMenu.BeforeSave;
begin
end;

procedure TDPMSBOMProjectMenu.Destroyed;
begin
  FProject := nil;
  FProjectGroup := nil;
  FContainer := nil;
  FLogger := nil;
end;

procedure TDPMSBOMProjectMenu.Modified;
begin
end;

procedure TDPMSBOMProjectMenu.Execute(const MenuContextList : IInterfaceList);
var
  targetPath : string;
begin
  //Right-click on the project-group root  -> aggregate the whole .groupproj
  //Right-click on a single project        -> just that dproj
  //FIsGroupContext is set by the notifier based on the right-click target;
  //FProject may be set even in group context (some IDE versions hand us the
  //active project even when the group node was clicked) so the explicit flag
  //is the source of truth, not "FProject = nil".
  if FIsGroupContext and (FProjectGroup <> nil) then
    targetPath := FProjectGroup.FileName
  else if FProject <> nil then
    targetPath := FProject.FileName
  else if FProjectGroup <> nil then
    targetPath := FProjectGroup.FileName
  else
  begin
    MessageDlg('Could not determine the project / group to generate an SBOM for.', mtError, [mbOK], 0);
    exit;
  end;

  RunSBOM(FContainer, FLogger, targetPath);
end;

class procedure TDPMSBOMProjectMenu.RunSBOM(const container : TContainer; const logger : ILogger; const targetPath : string);
var
  generator : ISbomGenerator;
  options : TSBOMOptions;
  outDir : string;
  ideLogger : IDPMIDELogger;
  ideOptions : IDPMIDEOptions;
  cancelTokenSource : ICancellationTokenSource;
  ok : boolean;
  guard : TOperationGuard;
  sidecarInfo : TSBOMProductInfo;
  values : TSBOMDialogValues;
begin
  if (targetPath = '') or (not FileExists(targetPath)) then
  begin
    MessageDlg('Project file [' + targetPath + '] not found - save the project first and try again.',
               mtWarning, [mbOK], 0);
    exit;
  end;

  generator := nil;
  try
    generator := container.Resolve<ISbomGenerator>;
  except
    on e : Exception do
    begin
      MessageDlg('Could not resolve the SBOM generator from the container : ' + e.Message,
                 mtError, [mbOK], 0);
      exit;
    end;
  end;

  ideOptions := container.Resolve<IDPMIDEOptions>;
  Supports(logger, IDPMIDELogger, ideLogger);

  options := TSBOMOptions.Create;
  try
    options.ProjectPath := targetPath;
    //The generator's first step is FConfigManager.LoadConfig(options.ConfigFile)
    //which fails with 'Cannot open file ""' when ConfigFile is blank. The CLI
    //populates this via TOptionsBase.ApplyCommon(TCommonOptions.Default), which
    //isn't wired in IDE-land - the CLI option-registration code never runs here.
    //Fall back to the user's default config explicitly.
    TConfigUtils.EnsureDefaultConfigDir;
    options.ConfigFile := TConfigUtils.GetDefaultConfigFileName;

    //What the sidecar says now, to prefill the dialog. One that can't be read is reported here
    //rather than overwritten by the dialog's values (LoadSBOMSidecar logged why).
    if not LoadSBOMSidecar(logger, targetPath, sidecarInfo, nil) then
    begin
      if ideLogger <> nil then
        ideLogger.ShowMessageTab;
      MessageDlg('The SBOM product information file [' + SBOMSidecarFileName(targetPath) + '] could not be read - ' +
                 'see the DPM message tab. Fix or delete it, then generate the SBOM again.', mtError, [mbOK], 0);
      exit;
    end;

    values.Info := sidecarInfo;
    values.Fallback := generator.GetVersionInfoDefaults(options);
    values.Formats := ideOptions.SBOMFormats;
    //Output dir defaults to next to the project. Same convention as the CLI - keeps
    //the SBOM files alongside the .dproj where reviewers tend to look first.
    values.OutputDir := ExtractFileDir(targetPath);
    values.SaveToSidecar := true;
    if not TSBOMOptionsForm.Execute(targetPath, values) then
      exit;

    //the formats are the user's preference, not the project's
    if ideOptions.SBOMFormats <> values.Formats then
    begin
      ideOptions.SBOMFormats := values.Formats;
      try
        ideOptions.SaveToFile;
      except
        //only a convenience for next time - not a reason to stop
        on e : Exception do
          logger.Information('[SBOM] could not remember the chosen formats : ' + e.Message);
      end;
    end;

    if values.SaveToSidecar and not SaveSBOMSidecar(logger, targetPath, values.Info) then
    begin
      //SaveSBOMSidecar logged why. The values below still apply to this run.
      if ideLogger <> nil then
        ideLogger.ShowMessageTab;
      MessageDlg('The product information could not be saved to [' + SBOMSidecarFileName(targetPath) + '] - see the ' +
                 'DPM message tab. The SBOM will still be generated with the values you entered.', mtWarning, [mbOK], 0);
    end;

    //The dialog's values go to the generator as options, which override the sidecar - so this
    //run uses what the user saw whether or not it was saved. A blank one falls through to the
    //sidecar and then the version info, as on the command line.
    options.Supplier := values.Info.Supplier;
    options.SupplierUrl := values.Info.SupplierUrl;
    options.License := values.Info.License;
    options.SbomCreator := values.Info.SbomCreator;
    options.ProductVersion := values.Info.Version;
    options.Formats := values.Formats;
    outDir := values.OutputDir;
    options.OutputDir := outDir;
    options.IncludeRuntime := true;
    options.Strict := false;
    //PerProject stays false - aggregated group SBOM when the input is a .groupproj.

    //Surface progress via the existing DPM message tab. The generator's
    //Information / Warning / Error calls land there via TDPMIDELogger.
    if ideLogger <> nil then
      ideLogger.ShowMessageTab;
    logger.Information('[SBOM] generating SBOM for ' + targetPath + ' -> ' + outDir);

    cancelTokenSource := TCancellationTokenSourceFactory.Create;
    guard := TOperationGuard.Start(logger);
    try
      ok := generator.Generate(cancelTokenSource.Token, options);
    except
      on e : Exception do
        ok := guard.Failed(e);
    end;
    guard.Finish(ok, false, 'Generating the SBOM');

    if ok then
    begin
      logger.Success('[SBOM] generated successfully. Output in ' + outDir, true);
      MessageDlg('SBOM generated successfully. Output in:'#13#10 + outDir, mtInformation, [mbOK], 0);
    end
    else
    begin
      logger.Information('[SBOM] the SBOM was not generated.', true);
      MessageDlg('SBOM generation reported a failure. See the DPM message tab for details.',
                 mtWarning, [mbOK], 0);
    end;
  finally
    options.Free;
  end;
end;

function TDPMSBOMProjectMenu.GetCaption : string;
begin
  //Trailing ellipsis: the command opens the Generate SBOM dialog.
  //Caption tracks the actual target: when the user right-clicks the group root we
  //drop the project name to avoid the misleading "Generate SBOM : OneProject"
  //label while we're actually about to aggregate the entire group.
  if FIsGroupContext then
    result := 'Generate SBOM for Project Group...'
  else if FProject <> nil then
    result := Format('Generate SBOM : %s...', [ExtractFileName(FProject.FileName)])
  else
    result := 'Generate SBOM for Project Group...';
end;

function TDPMSBOMProjectMenu.GetChecked : Boolean;
begin
  result := false;
end;

function TDPMSBOMProjectMenu.GetEnabled : Boolean;
begin
  //We tolerate unsaved projects in Execute (will warn the user) so always enabled.
  result := true;
end;

function TDPMSBOMProjectMenu.GetHelpContext : Integer;
begin
  result := -1;
end;

function TDPMSBOMProjectMenu.GetIsMultiSelectable : Boolean;
begin
  result := true;
end;

function TDPMSBOMProjectMenu.GetName : string;
begin
  result := 'DPMGenerateSBOM';
end;

function TDPMSBOMProjectMenu.GetParent : string;
begin
  result := '';
end;

function TDPMSBOMProjectMenu.GetPosition : Integer;
begin
  result := pmmpUserUtils + 3;
end;

function TDPMSBOMProjectMenu.GetVerb : string;
begin
  result := 'DPMGenerateSBOM';
end;

function TDPMSBOMProjectMenu.PostExecute(const MenuContextList : IInterfaceList) : Boolean;
begin
  Result := True;
end;

function TDPMSBOMProjectMenu.PreExecute(const MenuContextList : IInterfaceList) : Boolean;
begin
  Result := True;
end;

procedure TDPMSBOMProjectMenu.SetCaption(const Value : string);
begin
end;

procedure TDPMSBOMProjectMenu.SetChecked(Value : Boolean);
begin
end;

procedure TDPMSBOMProjectMenu.SetEnabled(Value : Boolean);
begin
end;

procedure TDPMSBOMProjectMenu.SetHelpContext(Value : Integer);
begin
end;

procedure TDPMSBOMProjectMenu.SetIsMultiSelectable(Value : Boolean);
begin
end;

procedure TDPMSBOMProjectMenu.SetName(const Value : string);
begin
end;

procedure TDPMSBOMProjectMenu.SetParent(const Value : string);
begin
end;

procedure TDPMSBOMProjectMenu.SetPosition(Value : Integer);
begin
end;

procedure TDPMSBOMProjectMenu.SetVerb(const Value : string);
begin
end;

end.
