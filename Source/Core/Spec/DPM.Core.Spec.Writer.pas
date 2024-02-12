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
    FSpec : IPackageSpec;
  protected
  public
    constructor Create(const logger : ILogger; spec: IPackageSpec);
    procedure SaveToFile(filename: string);
  end;

implementation

uses
  System.IOUtils,
  JSONDataObjects,
  DPM.Core.Project.Interfaces;

{ TPackageSpecWriter }

constructor TPackageSpecWriter.Create(const logger : ILogger; spec: IPackageSpec);
begin
  FLogger := logger;
  FSpec := spec;
end;

procedure TPackageSpecWriter.SaveToFile(filename: string);
begin
  JsonSerializationConfig.IndentChar := ' ';
  TFile.WriteAllText(Filename, Fspec.ToJson);
end;

end.

