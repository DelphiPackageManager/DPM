unit DPM.IDE.ToolsAPI;

interface

uses
  ToolsApi,
  System.Classes,
  Vcl.Forms,
  Vcl.Menus;

type
  TToolsApiUtils = class
    class procedure RegisterFormClassForTheming(const AFormClass : TCustomFormClass; const Component : TComponent = nil);static;
    class function FindProjectInGroup(const projectGroup : IOTAProjectGroup; const projectName : string) : IOTAProject;
    class function FindTopLevelMenu(const name : string) : TMenuItem;static;
    class function FindChildMenuItem(const parent : TMenuItem; const name : string) : TMenuItem;static;
    class function IsProjectAvailable : boolean;static;
    class function GetMainProjectGroup : IOTAProjectGroup;static;
    class function GetActiveProject : IOTAProject;static;

    /// <summary>
    ///  Saves every modified open module. Used before shutting the IDE down to
    ///  install a dpm upgrade, so the user does not lose work and is not left
    ///  answering save prompts while an installer is waiting.
    ///  Never raises - a module that refuses to save must not abort the upgrade.
    /// </summary>
    class procedure SaveModifiedFiles;static;

    /// <summary>
    ///  Asks the IDE to close, as though the user chose File > Exit. Posts
    ///  rather than sends, so the caller returns and unwinds first. This goes
    ///  through the IDE's normal shutdown, so anything still unsaved will
    ///  prompt as usual.
    /// </summary>
    class procedure CloseIDE;static;
  end;

implementation

uses
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils;

{ TToolsApiUtils }

{$IF CompilerVersion >= 24.0} //XE3
{$LEGACYIFEND ON}
{$IFEND}

class function TToolsApiUtils.FindChildMenuItem(const parent: TMenuItem; const name : string): TMenuItem;
var
  childItem : TMenuItem;
  i : integer;
begin
  result := nil;
  for i := 0 to parent.Count -1 do
  begin
    childItem := parent.Items[i];
    if SameText(childItem.Name, name) then
    begin
      result := childItem;
      exit;
    end;
    if SameText(childItem.Caption, name) then
    begin
      result := childItem;
      exit;
    end;
  end;
end;

class function TToolsApiUtils.FindProjectInGroup(const projectGroup: IOTAProjectGroup; const projectName: string): IOTAProject;
{$IF CompilerVersion < 24.0}  //XE3
var
  i : integer;
{$IFEND}
begin
  result := nil;
  if projectGroup = nil then
    exit;
{$IF CompilerVersion < 24.0}  //XE3
  for i := 0 to projectGroup.ProjectCount -1 do
  begin
    if SameText(projectGroup.Projects[i].FileName, projectName) then
      exit(projectGroup.Projects[i]);
  end;
{$ELSE}
  result := projectGroup.FindProject(projectName);
{$IFEND}
end;

class function TToolsApiUtils.FindTopLevelMenu(const name: string): TMenuItem;
var
  NTAServices: INTAServices;
  mainMenu : TMainMenu;
  i : integer;
begin
  result := nil;
  if Supports(BorlandIDEServices, INTAServices, NTAServices) then
  begin
    mainMenu := NTAServices.MainMenu;
    for i := 0 to mainMenu.Items.Count -1 do
    begin
      if SameText(mainMenu.Items[i].Name, name) then
      begin
        result := mainMenu.Items[i];
        exit;
      end;
      if SameText(mainMenu.Items[i].Caption, name) then
      begin
        result := mainMenu.Items[i];
        exit;
      end;
    end;
  end;
end;

class function TToolsApiUtils.GetActiveProject: IOTAProject;
var
  ModServices: IOTAModuleServices;
begin
  ModServices := BorlandIDEServices as IOTAModuleServices;
  result := ModServices.GetActiveProject;
end;

class procedure TToolsApiUtils.SaveModifiedFiles;
var
  modServices : IOTAModuleServices;
  i : integer;
begin
  modServices := BorlandIDEServices as IOTAModuleServices;
  if modServices = nil then
    exit;

  for i := 0 to modServices.ModuleCount - 1 do
  begin
    //Each module is saved independently - one that refuses (read only file,
    //a designer that will not validate) must not stop the rest being saved.
    try
      //(false, true) = don't ask, save even if the IDE thinks it can skip.
      modServices.Modules[i].Save(false, true);
    except
      //Swallow deliberately. The IDE's own shutdown will prompt for anything
      //still unsaved, so the user is not silently losing work.
      on Exception do
        continue;
    end;
  end;
end;

class procedure TToolsApiUtils.CloseIDE;
begin
  if Application.MainForm = nil then
    exit;
  //Post, not Send - the caller is typically inside a click handler and must be
  //allowed to unwind before the IDE starts tearing itself down.
  PostMessage(Application.MainForm.Handle, WM_CLOSE, 0, 0);
end;

class function TToolsApiUtils.GetMainProjectGroup: IOTAProjectGroup;
var
  ModServices: IOTAModuleServices;
begin
  ModServices := BorlandIDEServices as IOTAModuleServices;
  result := ModServices.MainProjectGroup;
end;

class function TToolsApiUtils.IsProjectAvailable: boolean;
var
  ModServices: IOTAModuleServices;
begin
  ModServices := BorlandIDEServices as IOTAModuleServices;
  result := ModServices.GetActiveProject <> nil;
end;

class procedure TToolsApiUtils.RegisterFormClassForTheming(const AFormClass: TCustomFormClass; const Component: TComponent);

{$IF CompilerVersion >= 32.0} //10.2
Var
  {$IF CompilerVersion = 34.0}  //10.4
  // Breaking change to the Open Tools API - They fixed the wrongly defined interface
  ITS : IOTAIDEThemingServices;
  {$ELSE}
  ITS : IOTAIDEThemingServices250;
  {$IFEND}
{$IFEND}

Begin
{$IF CompilerVersion >= 32.0} //10.2
  {$IF CompilerVersion = 34.0}  //10.4
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
  {$ELSE}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
  {$IFEND}
    If ITS.IDEThemingEnabled Then
      Begin
        ITS.RegisterFormClass(AFormClass);
        If Assigned(Component) Then
          ITS.ApplyTheme(Component);
      End;
{$IFEND}
end;

end.
