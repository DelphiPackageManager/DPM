unit DPM.Creator.Dspec.FileHandler;

interface

uses
  System.JSON,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader
  ;


type
  TDSpecFile = class
  private
    FLogger: ILogger;
    FReader: IPackageSpecReader;
    FFilename : string;
    FLoadedSpec : IPackageSpec;
    FPackageSpec : IPackageSpec;

  public
    procedure DeleteTemplate(const templateName: string);
    function DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;
    function DoesTemplateExist(const templateName: string): Boolean;
    function GetNewTemplateName(const sourceName : string) : string;
    function GetTemplate(const templateName: string): ISpecTemplate;
    function GetPlatform(const compiler: string): ISpecTargetPlatform;
    function AddCompiler(const compiler: string): ISpecTargetPlatform;
    procedure DeleteCompiler(const compiler: string);
    procedure ClearCompilers;
    function LoadFromFile(const filename: string; var errorMessage : string) : boolean;
    procedure SaveToFile(const filename: string);
    function WorkingDir: string;
    function IsModified: Boolean;
    function AsString: string;
    constructor Create(logger: ILogger);
    destructor Destroy; override;
    property FileName : string read FFileName;
    property PackageSpec : IPackageSpec read FPackageSpec;
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
begin
  if Assigned(GetPlatform(compiler)) then
    raise Exception.Create('Platform already exists in file');

  result := TSpecTargetPlatform.Create(FLogger);
  result.Compiler := StringToCompilerVersion(compiler);
  result.Platforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];

  FPackageSpec.TargetPlatforms.Add(result);
end;

function TDSpecFile.AsString: string;
begin
  Result := FPackageSpec.ToJSON;
end;

procedure TDSpecFile.ClearCompilers;
begin
  FPackageSpec.TargetPlatforms.Clear;
end;

constructor TDSpecFile.Create(logger: ILogger);
begin
  FLogger := logger;
  FPackageSpec := TSpec.Create(FLogger, '');
  FLoadedSpec := TSpec.Create(FLogger, '');
end;

procedure TDSpecFile.DeleteTemplate(const templateName: string);
begin
  FPackageSpec.DeleteTemplate(templateName);
end;

procedure TDSpecFile.DeleteCompiler(const compiler: string);
var
  i : Integer;
begin
  for i := 0 to FPackageSpec.TargetPlatforms.Count - 1 do
  begin
    if SameText(CompilerToString(FPackageSpec.TargetPlatforms[i].compiler), compiler) then
    begin
      FPackageSpec.TargetPlatforms.Delete(i);
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
  Result := Assigned(FPackageSpec.FindTemplate(templateName));
end;

function TDSpecFile.DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;
begin
  Result := FPackageSpec.DuplicateTemplate(sourceTemplate, NewTemplateName);
end;

function TDSpecFile.GetTemplate(const templateName: string): ISpecTemplate;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to FPackageSpec.templates.Count - 1 do
  begin
    if FPackageSpec.templates[i].name = templateName then
    begin
      Result := FPackageSpec.templates[i];
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
  for i := 0 to FPackageSpec.targetPlatforms.Count - 1 do
  begin
    if FPackageSpec.targetPlatforms[i].compiler = StringToCompilerVersion(compiler) then
    begin
      Result := FPackageSpec.targetPlatforms[i];
      Exit;
    end;
  end;
end;

function TDSpecFile.IsModified: Boolean;
begin
//TODO : This is wasteful - implement a modified flag.

  if (FPackageSpec <> nil) and (FLoadedSpec <> nil) then
    Result := not SameText(FPackageSpec.ToJSON, FLoadedSpec.ToJSON)
  else
    result := false;
end;

function TDSpecFile.LoadFromFile(const filename: string; var errorMessage : string) : boolean;
begin
  result := true;
  FReader := TPackageSpecReader.Create(FLogger);
  try
    FPackageSpec := FReader.ReadSpec(filename);
    if (FPackageSpec = nil) then
      raise Exception.Create('Failed to load dspec');
    FLoadedSpec := FReader.ReadSpec(filename);
  except
    on E : Exception do
    begin
      errorMessage := e.Message;
      result := false;
    end;
  end;
  FFilename := Filename;
end;

procedure TDSpecFile.SaveToFile(const filename: string);
//var
//  writer : IPackageSpecWriter;
begin
  raise ENotImplemented.Create('Error Message');

  //this is the only place the writer is used - will just use the spec class

//  writer := TPackageSpecWriter.Create(FLogger, FPackageSpec);
//  writer.SaveToFile(filename);
//  FFilename := Filename;
//  //TODO : This is terrible - find a better way to handle modified
//  FLoadedSpec := FReader.ReadSpec(filename);
end;

function TDSpecFile.WorkingDir: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(FFilename));
end;

end.
