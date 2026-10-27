unit DPM.Creator.Dspec.FileHandler;

interface

uses
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
    //the spec as it was loaded, saved, or set up (collapsed) - IsModified compares with it.
    FBaselineYAML : string;
    FPackageSpec : IPackageSpec;

    //The UI edits one compiler per row, so we expand ranges/lists into single-compiler entries on
    //load and collapse them back on output. CollapsedYAML/SaveCollapsed run an action against the
    //spec with its targetPlatforms temporarily swapped to the collapsed form, then restore them so
    //the live (expanded) objects the UI holds are untouched.
    function CollapsedYAML(const spec : IPackageSpec) : string;
    procedure SaveCollapsed(const spec : IPackageSpec; const filename : string);
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
    //what the spec is now is what IsModified compares with - called once a new spec has been set up.
    procedure MarkUnmodified;
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
  Spring.Collections,
  DPM.Core.Spec,
  DPM.Core.Spec.Template,
  DPM.Core.Spec.TargetPlatform,
  DPM.Creator.TargetPlatform.Collapse,
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
  //preview/output reflects the collapsed (to-be-saved) form, not the expanded editing form.
  Result := CollapsedYAML(FPackageSpec);
end;

function TDSpecFile.CollapsedYAML(const spec: IPackageSpec): string;
var
  expanded : IList<ISpecTargetPlatform>;
  collapsed : IList<ISpecTargetPlatform>;
begin
  expanded := TCollections.CreateList<ISpecTargetPlatform>;
  expanded.AddRange(spec.TargetPlatforms);
  //collapse before clearing - CollapseTargetPlatforms reads spec.TargetPlatforms.
  collapsed := CollapseTargetPlatforms(spec);
  spec.TargetPlatforms.Clear;
  spec.TargetPlatforms.AddRange(collapsed);
  try
    result := spec.GenerateDspecYAML(spec.MetaData.Version);
  finally
    spec.TargetPlatforms.Clear;
    spec.TargetPlatforms.AddRange(expanded);
  end;
end;

procedure TDSpecFile.SaveCollapsed(const spec: IPackageSpec; const filename: string);
var
  expanded : IList<ISpecTargetPlatform>;
  collapsed : IList<ISpecTargetPlatform>;
begin
  expanded := TCollections.CreateList<ISpecTargetPlatform>;
  expanded.AddRange(spec.TargetPlatforms);
  //collapse before clearing - CollapseTargetPlatforms reads spec.TargetPlatforms.
  collapsed := CollapseTargetPlatforms(spec);
  spec.TargetPlatforms.Clear;
  spec.TargetPlatforms.AddRange(collapsed);
  try
    spec.ToYAMLFile(filename);
  finally
    spec.TargetPlatforms.Clear;
    spec.TargetPlatforms.AddRange(expanded);
  end;
end;

procedure TDSpecFile.ClearCompilers;
begin
  FPackageSpec.TargetPlatforms.Clear;
end;

constructor TDSpecFile.Create(logger: ILogger);
begin
  FLogger := logger;
  FPackageSpec := TSpec.Create(FLogger, '');
  MarkUnmodified;
end;

procedure TDSpecFile.MarkUnmodified;
begin
  if FPackageSpec <> nil then
    FBaselineYAML := CollapsedYAML(FPackageSpec)
  else
    FBaselineYAML := '';
end;

procedure TDSpecFile.DeleteTemplate(const templateName: string);
begin
  FPackageSpec.DeleteTemplate(templateName);
end;

procedure TDSpecFile.DeleteCompiler(const compiler: string);
var
  i : Integer;
  cv : TCompilerVersion;
begin
  cv := StringToCompilerVersion(compiler);
  for i := 0 to FPackageSpec.TargetPlatforms.Count - 1 do
  begin
    if FPackageSpec.TargetPlatforms[i].IsForCompiler(cv) then
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
  cv : TCompilerVersion;
begin
  Result := nil;
  cv := StringToCompilerVersion(compiler);
  //match via IsForCompiler so all three authoring forms (single compiler, compilers list,
  //compiler from/to range) resolve to the entry that covers this compiler.
  for i := 0 to FPackageSpec.targetPlatforms.Count - 1 do
  begin
    if FPackageSpec.targetPlatforms[i].IsForCompiler(cv) then
    begin
      Result := FPackageSpec.targetPlatforms[i];
      Exit;
    end;
  end;
end;

function TDSpecFile.IsModified: Boolean;
begin
  //compare the collapsed form - it is canonically ordered, so reordering during editing (e.g. appending a newly
  //added compiler) does not register as a change on its own. Exactly - a change of case is a change. The baseline
  //was an empty spec until a file was loaded, so a new one was 'modified' before anything was typed.
  if FPackageSpec <> nil then
    Result := CollapsedYAML(FPackageSpec) <> FBaselineYAML
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
    //expand compiler ranges/lists into one entry per compiler so the UI can edit each
    //independently. The baseline is expanded too so the modified check compares like with like.
    ExpandTargetPlatforms(FPackageSpec);
    MarkUnmodified;
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
begin
  //collapse the per-compiler editing entries back into ranges/lists for the on-disk file.
  SaveCollapsed(FPackageSpec, filename);
  FFilename := filename;
  //what was just written is the new baseline.
  MarkUnmodified;
end;

function TDSpecFile.WorkingDir: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(FFilename));
end;

end.
