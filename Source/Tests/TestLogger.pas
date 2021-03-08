unit TestLogger;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging;

type
  TTestLogger = class(TInterfacedObject, ILogger)
  protected
    procedure Debug(const data: string);
    procedure Error(const data: string);
    procedure Information(const data: string; const important : boolean = false);
    procedure Success(const data: string; const important : boolean = false);
    procedure Verbose(const data: string; const important : boolean = false);
    procedure Warning(const data: string; const important : boolean = false);
    procedure Clear;
    procedure NewLine;

    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);

  end;

implementation

{ TTestLogger }

procedure TTestLogger.Clear;
begin

end;

procedure TTestLogger.Debug(const data: string);
begin

end;

procedure TTestLogger.Error(const data: string);
begin

end;

function TTestLogger.GetVerbosity: TVerbosity;
begin
  result := TVerbosity.Normal; //just to shut the compiler up.
end;

procedure TTestLogger.Information(const data: string; const important : boolean);
begin

end;

procedure TTestLogger.NewLine;
begin
///
end;

procedure TTestLogger.SetVerbosity(const value: TVerbosity);
begin

end;

procedure TTestLogger.Success(const data: string;  const important: boolean);
begin

end;

procedure TTestLogger.Verbose(const data: string; const important : boolean);
begin

end;

procedure TTestLogger.Warning(const data: string; const important : boolean);
begin

end;

end.
