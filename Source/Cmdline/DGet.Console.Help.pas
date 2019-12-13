unit DGet.Console.Help;

interface

uses
  DGet.Console.Types,
  DGet.Console;


procedure ShowCommandHelp(console : IConsole; const command : TDelphiGetCommand);

implementation

uses
  VSoft.CommandLine.Options;


procedure ShowCommandHelp(console : IConsole; const command : TDelphiGetCommand);
//var
//  cmd : ICommandDefinition;
begin
  case command of
    TDelphiGetCommand.Help :
    begin



    end;
  else


    TOptionsRegistry.PrintUsage(CommandString[command],
      procedure (const value : string)
      begin
          console.WriteLine(value);
      end);

  end;


end;

end.
