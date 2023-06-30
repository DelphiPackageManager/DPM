program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  DPM.Controls.LogMemo in '..\DPM.Controls.LogMemo.pas',
  DPM.Core.Utils.Strings in '..\..\Core\Utils\DPM.Core.Utils.Strings.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Glossy');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
