unit DPM.Core.Options.Info;

interface

uses
  DPM.Core.Options.Base;

type
  TInfoOptions = class(TOptionsBase)
  private
    class var
      FDefault : TInfoOptions;
  public
    class constructor CreateDefault;
    class destructor DestroyDefault;
    class property Default : TInfoOptions read FDefault;

  end;

implementation

{ TInfoOptions }

class constructor TInfoOptions.CreateDefault;
begin
  FDefault := TInfoOptions.Create;
end;

//the singleton goes with the unit - it was never freed, so every run reported it as a leak.
class destructor TInfoOptions.DestroyDefault;
begin
  FDefault.Free;
end;

end.
