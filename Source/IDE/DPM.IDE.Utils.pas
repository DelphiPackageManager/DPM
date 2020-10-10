unit DPM.IDE.Utils;

interface

uses
  System.classes,
  Vcl.Controls;

//  Find a control that is a child of Application.Mainform or it's children
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
begin
  result := DoFindIDEControl(Application.MainForm, className, controlName);
end;

end.
