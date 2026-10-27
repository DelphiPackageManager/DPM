unit DPM.IDE.Utils;

interface

uses
  System.classes,
  Vcl.Controls;

//  Find a control on one of the IDE's forms (or a child of one) - the main form, or a floating window such as an
//  undocked Project Manager.
function FindIDEControl(const className : string; const controlName : string) : TControl;

implementation

uses
  System.SysUtils,
  Vcl.Forms;

function DoFindIDEControl(const parentControl : TControl; const className : string; const controlName : string) : TControl;
var
  i : integer;
begin
  result := nil;
  if SameText(parentControl.ClassName, className) and SameText(parentControl.Name, controlName) then
    exit(parentControl);

  if not (parentControl is TWinControl) then
    exit;

  //TWinControls have children, so recurse in to check those too.
  for i := 0 to TWinControl(parentControl).ControlCount - 1 do
  begin
    result := DoFindIDEControl(TWinControl(parentControl).Controls[i], className, controlName);
    if result <> nil then
      break;
  end;

end;

function FindIDEControl(const className : string; const controlName : string) : TControl;
var
  i : integer;
begin
  result := nil;
  //the main form first - where the Project Manager usually is, docked.
  if Application.MainForm <> nil then
    result := DoFindIDEControl(Application.MainForm, className, controlName);
  if result <> nil then
    exit;
  for i := 0 to Screen.CustomFormCount - 1 do
  begin
    if Screen.CustomForms[i] = Application.MainForm then
      continue;
    result := DoFindIDEControl(Screen.CustomForms[i], className, controlName);
    if result <> nil then
      exit;
  end;
end;

end.
