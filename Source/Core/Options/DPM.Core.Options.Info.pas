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
    class property Default : TInfoOptions read FDefault;

  end;

implementation

{ TInfoOptions }

class constructor TInfoOptions.CreateDefault;
begin
  FDefault := TInfoOptions.Create;
end;

end.
