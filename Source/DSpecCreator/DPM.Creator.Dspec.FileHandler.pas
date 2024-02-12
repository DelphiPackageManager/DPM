unit DPM.Creator.Dspec.FileHandler;

interface

uses
  System.JSON,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Spec.Writer
  ;


type
  TDSpecFile = class
  private
    FLogger: ILogger;
    FReader: IPackageSpecReader;
    FFilename : string;
    FLoadedSpec : IPackageSpec;
  public
    spec : IPackageSpec;
    procedure DeleteTemplate(const templateName: string);
    function DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;
    function DoesTemplateExist(const templateName: string): Boolean;
    function GetNewTemplateName(const sourceName : string) : string;
    function GetTemplate(const templateName: string): ISpecTemplate;
    function GetPlatform(const compiler: string): ISpecTargetPlatform;
    function AddCompiler(const compiler: string): ISpecTargetPlatform;
    procedure DeleteCompiler(const compiler: string);
    procedure LoadFromFile(const filename: string);
    procedure SaveToFile(const filename: string);
    function WorkingDir: string;
    function IsModified: Boolean;
    function AsString: string;
    constructor Create(logger: ILogger);
    destructor Destroy; override;
  end;


implementation

uses
  System.IOUtils,
  System.Classes,
  System.SysUtils,
  System.JSON.Writers,
  REST.Json,
  DPM.Core.Spec,
  DPM.Core.Spec.Template,
  DPM.Core.Spec.TargetPlatform,
  DPM.Core.Types
  ;

{ TDSpecFile }

function TDSpecFile.AddCompiler(const compiler: string): ISpecTargetPlatform;
var
  vplatform : ISpecTargetPlatform;
begin
  if Assigned(GetPlatform(compiler)) then
    raise Exception.Create('Platform already exists in file');

  vplatform := TSpecTargetPlatform.Create(FLogger);
  vplatform.Compiler := StringToCompilerVersion(compiler);

  spec.TargetPlatforms.Add(vplatform);
end;

function TDSpecFile.AsString: string;
begin
  Result := spec.ToJSON;
end;

constructor TDSpecFile.Create(logger: ILogger);
begin
  FLogger := logger;
  spec := TSpec.Create(FLogger, '');
  FLoadedSpec := TSpec.Create(FLogger, '');
end;

procedure TDSpecFile.DeleteTemplate(const templateName: string);
begin
  spec.DeleteTemplate(templateName);
end;

procedure TDSpecFile.DeleteCompiler(const compiler: string);
var
  i : Integer;
begin
  for i := 0 to spec.TargetPlatforms.Count - 1 do
  begin
    if SameText(CompilerToString(spec.TargetPlatforms[i].compiler), compiler) then
    begin
      spec.TargetPlatforms.Delete(i);
      Exit;
    end;
  end;
end;


destructor TDSpecFile.Destroy;
begin
  inherited;
end;

function TDSpecFile.DoesTemplateExist(const templateName: string): Boolean;
begin
  Result := Assigned(spec.FindTemplate(templateName));
end;

function TDSpecFile.DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;
begin
  Result := spec.DuplicateTemplate(sourceTemplate, NewTemplateName);
end;

function TDSpecFile.GetTemplate(const templateName: string): ISpecTemplate;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to spec.templates.Count - 1 do
  begin
    if spec.templates[i].name = templateName then
    begin
      Result := spec.templates[i];
      Exit;
    end;
  end;
end;

function TDSpecFile.GetNewTemplateName(const sourceName: string): string;
begin
  result := sourceName + Random(100).ToString;
  while DoesTemplateExist(Result) do
    result := sourceName + Random(100).ToString;
end;

function TDSpecFile.GetPlatform(const compiler: string): ISpecTargetPlatform;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to spec.targetPlatforms.Count - 1 do
  begin
    if spec.targetPlatforms[i].compiler = StringToCompilerVersion(compiler) then
    begin
      Result := spec.targetPlatforms[i];
      Exit;
    end;
  end;
end;

function TDSpecFile.IsModified: Boolean;
begin
  Result := not SameText(spec.ToJSON, FLoadedSpec.ToJSON);
end;

procedure TDSpecFile.LoadFromFile(const filename: string);
begin
  FReader := TPackageSpecReader.Create(FLogger);
  spec := FReader.ReadSpec(filename);
  FLoadedSpec := FReader.ReadSpec(filename);
  FFilename := Filename;
end;

procedure TDSpecFile.SaveToFile(const filename: string);
var
  writer : IPackageSpecWriter;
begin
  writer := TPackageSpecWriter.Create(FLogger, spec);
  writer.SaveToFile(filename);
  FFilename := Filename;
end;

function TDSpecFile.WorkingDir: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(FFilename));
end;

end.
