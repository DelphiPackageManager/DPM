program TestProject;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Unit1 in 'Unit1.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    WriteLn(Foo);
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
