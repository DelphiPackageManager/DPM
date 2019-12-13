unit DPM.Core.Spec.Writer;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Spec,
  DPM.Core.Spec.Interfaces;

type
  TPackageSpecWriter = class(TInterfacedObject, IPackageSpecWriter)
  private
    FLogger : ILogger;
  protected
    function CreateSpecFile(const options: TSpecOptions): Boolean;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  DPM.Core.Project.Interfaces;

{ TPackageSpecWriter }

constructor TPackageSpecWriter.Create(const logger: ILogger);
begin
  FLogger := logger;
end;

function TPackageSpecWriter.CreateSpecFile(const options: TSpecOptions): Boolean;
begin
  result := false;

end;

end.
