unit DPM.IDE.ToolsAPI;

interface

uses
  ToolsApi,
  System.Classes,
  Vcl.Forms;

type
  TToolsApiUtils = class
    class procedure RegisterFormClassForTheming(const AFormClass : TCustomFormClass; const Component : TComponent = nil);static;
    class function FindProjectInGroup(const projectGroup : IOTAProjectGroup; const projectName : string) : IOTAProject;
  end;

implementation

uses
  System.SysUtils;

{ TToolsApiUtils }

{$IF CompilerVersion >= 24.0} //XE3
{$LEGACYIFEND ON}
{$IFEND}

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
