{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Core.Spec;

interface

uses
  System.SysUtils,
  System.Classes,
  Spring.Collections,
  System.RegularExpressions,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  VSoft.SemanticVersion,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node,
  JsonDataObjects;


type
  TSpec = class;
  IPackageSpecPrivate = interface(IPackageSpec)
    ['{4236039A-8FD4-4061-A942-01C6DF605163}']
    function AsObject : TSpec;
  end;

  TSpec = class(TSpecNode, IPackageSpec, IPackageSpecPrivate)
  private
    FPackageKind : TDPMPackageKind;
    FMetaData : ISpecMetaData;
    FTargetPlatforms : IList<ISpecTargetPlatform>;
    FTemplates : IList<ISpecTemplate>;
    FVariables : IVariables;
    FIsValid : boolean;
    FCurrentTokens : TStringList;
    FFileName : string;

    // targetPlatforms, templates and variables collections are not backed by an object, so nowhere else to store comments.
    FTargetPlatformsComments : TStringList;
    FTemplatesComments : TStringList;
    FVariablesComments : TStringList;

  public
    function AsObject : TSpec;
    function ApplyDependencies(const targetPlatform : ISpecTargetPlatform; const dependencies : IList<ISpecDependency>) : boolean;
    function ApplySource(const targetPlatform : ISpecTargetPlatform; const sourceFiles : IList<ISpecSourceEntry>) : boolean;
    function ApplyDesign(const targetPlatform : ISpecTargetPlatform; const designFiles : IList<ISpecDesignEntry>) : boolean;
    function ApplyBuild(const targetPlatform : ISpecTargetPlatform; const buildEntries : IList<ISpecBuildEntry>) : boolean;

    function ReplaceTokens(const version : TPackageVersion; const properties : TStringList) : boolean;

    procedure GetTokensForTargetPlatform(const targetPlatform : ISpecTargetPlatform; const version : TPackageVersion; const list : TStringList; const externalProps : TStringList);

    function TokenMatchEvaluator(const match : TMatch) : string;

    function GenerateManifestJson(const version : TSemanticVersion; const targetPlatform : ISpecTargetPlatform) : string;
    function GenerateManifestYAML(const version : TSemanticVersion; const targetPlatform : ISpecTargetPlatform) : string;

    //
    function GenerateManifest(const version : TPackageVersion; const targetPlatform : ISpecTargetPlatform) : IPackageSpec;

    //we call this when generating the manifest so the manifest works with an unprocessed spec
    function Clone : IPackageSpec;


    function GetFileName : string;
    function GetIsValid : boolean;
    function GetMetaData : ISpecMetaData;
    function GetTargetPlatform : ISpecTargetPlatform;
    function GetTargetPlatforms : IList<ISpecTargetPlatform>;
    function GetTemplates : IList<ISpecTemplate>;
    function GetPackageKind : TDPMPackageKind;
    procedure SetPackageKind(const value : TDPMPackageKind);
    function GetVariables : IVariables;
    function GetTargetPlatformsComments : TStrings;
    function GetTemplatesComments : TStrings;
    function GetVariablesComments : TStrings;


    function LoadTemplateFromJson(const templateObj : TJsonObject; const templateNo : integer) : boolean;
    function LoadTemplateFromYAML(const templateObj : IYAMLMapping; const templateNo : integer) : boolean;

    function LoadTemplatesFromJson(const templatesArray : TJsonArray) : boolean;
    function LoadTemplatesFromYAML(const templatesSeq : IYAMLSequence) : boolean;

    function LoadTargetPlatformsFromJson(const targetPlatformsArray : TJsonArray) : boolean;
    function LoadTargetPlatformsFromYAML(const targetPlatformsSeq : IYAMLSequence) : boolean;

    function LoadVariablesFromYAML(const variablesObj : IYAMLMapping) : boolean;

    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    function ToJSON: string; override;

    procedure ToYAML(const parent : IYAMLValue; const packageKind : TDPMPackageKind);override;

    procedure ToYAMLFile(const fileName : string);


    // Template functions
    function FindTemplate(const name : string) : ISpecTemplate;
    function NewTemplate(const name : string) : ISpecTemplate;
    procedure RenameTemplate(const currentTemplateName: string; const NewTemplateName:string);
    procedure DeleteTemplate(const templateName: string);
    function DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;

    constructor CreateClone(const logger : ILogger; const source : IPackageSpecPrivate);
  public
    constructor Create(const logger : ILogger; const fileName : string); reintroduce;
    destructor Destroy;override;
  end;


implementation

uses
  System.Generics.Defaults,
  DPM.Core.Constants,
  DPM.Core.Dependency.Version,
  DPM.Core.Spec.MetaData,
  DPM.Core.Spec.Template,
  DPM.Core.Spec.SourceEntry,
  DPM.Core.Spec.TargetPlatform,
  DPM.Core.Utils.Strings;

{ TSpec }



function TSpec.ApplyBuild(const targetPlatform : ISpecTargetPlatform; const buildEntries : IList<ISpecBuildEntry>) : boolean;
//var
//  existing : ISpecBuildEntry;
//  newBuildEntry : ISpecBuildEntry;
begin
  raise ENotImplemented.Create('will be removed');
//  result := true;
//  for newBuildEntry in buildEntries do
//  begin
//    existing := targetPlatform.FindBuildEntry(newBuildEntry.Project);
//    if existing <> nil then
//    begin
//      result := false;
//      Logger.Error('Duplicate Build entry ' + newBuildEntry.Project + '] found in targetPlatform and template');
//    end
//    else
//    begin
//      targetPlatform.BuildEntries.Add(newBuildEntry.Clone);
//    end;
//  end;
end;

function TSpec.ApplyDependencies(const targetPlatform : ISpecTargetPlatform; const dependencies : IList<ISpecDependency>) : boolean;
//var
//  newDependency : ISpecDependency;
//  existingDependency : ISpecDependency;
begin
  raise ENotImplemented.Create('will be removed');
//  result := true;
//  if not dependencies.Any then
//    exit;
//
//  for newDependency in dependencies do
//  begin
//    existingDependency := targetPlatform.FindDependency(newDependency.Id);
//    if existingDependency <> nil then
//    begin
//      result := false;
//      Logger.Error('Duplicate dependency [' + newDependency.Id + '] found in targetPlatform and template');
//    end
//    else
//    begin
//      existingDependency := newDependency.Clone;
//      targetPlatform.Dependencies.Add(existingDependency);
//    end;
//
//  end;



end;

function TSpec.ApplyDesign(const targetPlatform : ISpecTargetPlatform; const designFiles : IList<ISpecDesignEntry>) : boolean;
//var
//  existing : ISpecDesignEntry;
//  newBPL : ISpecDesignEntry;
begin
  raise ENotImplemented.Create('will be removed');

//s  result := true;
//  for newBPL in designFiles do
//  begin
//    existing := targetPlatform.FindDesignEntry(newBPL.Project);
//    if existing <> nil then
//    begin
//      result := false;
//      Logger.Error('Duplicate Design project ' + newBPL.Project + '] found in targetPlatform and template');
//    end
//    else
//    begin
//      existing := newBPL.Clone;
//      targetPlatform.DesignEntries.Add(existing.Clone);
//    end;
//  end;
end;


function TSpec.ApplySource(const targetPlatform : ISpecTargetPlatform; const sourceFiles : IList<ISpecSourceEntry>) : boolean;
//var
//  existing : ISpecSourceEntry;
//  newEntry : ISpecSourceEntry;
begin
  raise ENotImplemented.Create('will be removed');

//  result := true;
//  for newEntry in sourceFiles do
//  begin
//    existing := targetPlatform.FindSourceEntry(newEntry.Source);
//    if existing <> nil then
//    begin
//      result := false;
//      Logger.Error('Duplicate Source entry ' + newEntry.Source + '] found in targetPlatform and template');
//    end
//    else
//    begin
//      existing := newEntry.Clone;
//      targetPlatform.SourceEntries.Add(existing.Clone);
//    end;
//  end;
end;


function TSpec.AsObject: TSpec;
begin
  result := Self;
end;

function TSpec.Clone: IPackageSpec;
begin
  result := TSpec.CreateClone(Logger, self);
end;

constructor TSpec.Create(const logger : ILogger; const fileName : string);
begin
  inherited Create(logger);
  FPackageKind := TDPMPackageKind.dpm;
  FFileName := fileName;
  FTargetPlatformsComments := nil;
  FTemplatesComments := nil;

  FMetaData := TSpecMetaData.Create(logger, FPackageKind);
  FTargetPlatforms := TCollections.CreateList<ISpecTargetPlatform>;
  FTemplates := TCollections.CreateSortedList<ISpecTemplate>(
   function(const Left, Right: ISpecTemplate): Integer
   begin
      result := CompareText(Left.Name, Right.Name);
   end
  );
  FVariables := TCollections.CreateDictionary<string,string>;
end;

constructor TSpec.CreateClone(const logger: ILogger; const source: IPackageSpecPrivate);
var
  newTargetPlatform : ISpecTargetPlatform;
  newTemplate : ISpecTemplate;
  i : integer;
begin
  inherited Create(logger);
  FPackageKind := source.PackageKind;
  FFileName := source.FileName;
  if source.AsObject.FTargetPlatformsComments <> nil then
  begin
    FTargetPlatformsComments := TStringList.Create;
    FTargetPlatformsComments.Assign(source.AsObject.FTargetPlatformsComments)
  end;

  if source.AsObject.FTemplatesComments <> nil then
  begin
    FTemplatesComments := TStringList.Create;
    FTemplatesComments.Assign(source.AsObject.FTemplatesComments);
  end;

  FMetaData := source.MetaData.Clone;
  FTargetPlatforms := TCollections.CreateList<ISpecTargetPlatform>;
  for i := 0 to source.TargetPlatforms.Count -1 do
  begin
    newTargetPlatform := source.TargetPlatforms[i].Clone;
    FTargetPlatforms.Add(newTargetPlatform);
  end;

  FTemplates := TCollections.CreateSortedList<ISpecTemplate>(
   function(const Left, Right: ISpecTemplate): Integer
   begin
      result := CompareText(Left.Name, Right.Name);
   end
  );

  for i := 0 to source.Templates.Count -1 do
  begin
    newTemplate := source.Templates[i].Clone;
    FTemplates.Add(newTemplate);
  end;

  FVariables := TCollections.CreateDictionary<string,string>;
  FVariables.AddRange(source.Variables);
end;



procedure TSpec.DeleteTemplate(const templateName: string);
var
  i: Integer;
begin
  for i := 0 to FTemplates.Count - 1 do
  begin
    if SameText(FTemplates[i].Name, templateName) then
    begin
      FTemplates.Delete(i);
      Exit;
    end;
  end;
end;

destructor TSpec.Destroy;
begin
  FreeAndNil(FTemplatesComments);
  FreeAndNil(FTargetPlatformsComments);
  FreeAndNil(FVariablesComments);
  inherited;
end;


function TSpec.DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;
var
  I: Integer;
begin
  result := TSpecTemplate.Create(Logger);
  for I := 0 to sourceTemplate.Dependencies.Count - 1 do
  begin
    result.Dependencies.Add(sourceTemplate.Dependencies[i].Clone);
  end;
  for I := 0 to sourceTemplate.DesignEntries.Count - 1 do
  begin
    result.DesignEntries.Add(sourceTemplate.DesignEntries[i].Clone);
  end;
  for I := 0 to sourceTemplate.SourceEntries.Count - 1 do
  begin
    result.SourceEntries.Add(sourceTemplate.SourceEntries[i].Clone);
  end;
  result.Name := newTemplateName;
  FTemplates.Add(result);
end;

procedure TSpec.GetTokensForTargetPlatform(const targetPlatform : ISpecTargetPlatform; const version : TPackageVersion; const list : TStringList; const externalProps : TStringList);
var
  i: Integer;
  regEx : TRegEx;
  evaluator : TMatchEvaluator;
begin
  list.Clear;
  if not version.IsEmpty then
    list.Add('version=' + version.ToString)
  else
    list.Add('version=' + FMetaData.Version.ToString);
  list.Add('target=' + CompilerToString(targetPlatform.Compiler));
  list.Add('compiler=' + CompilerToString(targetPlatform.Compiler));
  list.Add('compilerNoPoint=' + CompilerToStringNoPoint(targetPlatform.Compiler));
  list.Add('compilerCodeName=' + CompilerCodeName(targetPlatform.Compiler));
  list.Add('compilerWithCodeName=' + CompilerWithCodeName(targetPlatform.Compiler));
  list.Add('platform=' + DPMPlatformToString(targetPlatform.Platforms[0]));
  list.Add('compilerVersion=' + CompilerToCompilerVersionIntStr(targetPlatform.Compiler));
  list.Add('libSuffix=' + CompilerToLibSuffix(targetPlatform.Compiler));
  list.Add('bdsVersion=' + CompilerToBDSVersion(targetPlatform.Compiler));
  list.Add('bitness=' + DPMPlatformBitness(targetPlatform.Platforms[0]));
  if DPMPlatformBitness(targetPlatform.Platforms[0]) = '64' then
    list.Add('bitness64Only=' + DPMPlatformBitness(targetPlatform.Platforms[0]))
  else
    list.Add('bitness64Only=');

  if targetPlatform.Variables.Count = 0 then
    exit;

  //override the values with values from the template.
  for i := 0 to targetPlatform.Variables.Count -1 do
  begin
    //setting a value to '' removes it from the list!!!!!
    list.Values[targetPlatform.Variables.Items[i].Key] := '';
    list.Add(targetPlatform.Variables.Items[i].Key + '=' + targetPlatform.Variables.Items[i].Value);
  end;

  //fix up some variable overrides
  if list.Values['compilerCodeName'] = '' then
  begin
    list.Values['compilerWithCodeName'] := '';
    list.Add('compilerWithCodeName=' + CompilerToString(targetPlatform.Compiler));
  end;


  //apply external props passed in on command line.
  if externalProps.Count > 0 then
  begin
    for i := 0 to externalProps.Count -1 do
      list.Values[externalProps.Names[i]] := externalProps.ValueFromIndex[i];
  end;

  regEx := TRegEx.Create('\$(\w+)\$');
  evaluator := TokenMatchEvaluator;

  //variables from the spec and external may reference existing variables.
  for i := 0 to list.Count -1 do
  begin
    if TStringUtils.Contains(list.ValueFromIndex[i], '$') then
      list.ValueFromIndex[i] := regEx.Replace(list.ValueFromIndex[i], evaluator);
  end;


end;

function TSpec.GetVariables: IVariables;
begin
  result := FVariables;
end;

function TSpec.GetVariablesComments: TStrings;
begin
  if FVariablesComments = nil then
    FVariablesComments := TStringList.Create;
  result := FVariablesComments;
end;

function TSpec.FindTemplate(const name : string) : ISpecTemplate;
begin
  result := FTemplates.FirstOrDefault(
    function(const item : ISpecTemplate) : boolean
    begin
      result := SameText(name, item.Name);
    end);
end;

function TSpec.GetTargetPlatform: ISpecTargetPlatform;
begin
  if FTargetPlatforms.Any then
    result := FTargetPlatforms[0]
  else
    result := nil;
end;

function TSpec.GetTargetPlatforms : IList<ISpecTargetPlatform>;
begin
  result := FTargetPlatforms;
end;

function TSpec.GetTargetPlatformsComments: TStrings;
begin
  if FTargetPlatformsComments = nil then
    FTargetPlatformsComments := TStringList.Create;
  result := FTargetPlatformsComments;
end;

function TSpec.GetTemplates : IList<ISpecTemplate>;
begin
  result := FTemplates;
end;

function TSpec.GetTemplatesComments: TStrings;
begin
  if FTemplatesComments = nil then
    FTemplatesComments := TStringList.Create;
  result := FTemplatesComments;
end;

function TSpec.GenerateManifest(const version: TPackageVersion; const targetPlatform: ISpecTargetPlatform): IPackageSpec;
var
  newSpec : IPackageSpec;
begin
//  newSpec := Self.Clone


end;

function TSpec.GenerateManifestJson(const version : TSemanticVersion; const targetPlatform : ISpecTargetPlatform) : string;
//var
//  Obj : TJsonObject;
//  dependency : ISpecDependency;
//  searchPath : ISpecSearchPath;
//  buildEntry : ISpecBuildEntry;
//  bplEntry : ISpecBPLEntry;
//  metaDataObj : TJsonObject;
//  targetPlatformObject : TJDOJsonObject;
////  variablesObj : TJsonObject;
//  dependencyObj : TJsonObject;
//  seachPathObj : TJsonObject;
//  buildEntryObj : TJsonObject;
//  runtimeEntryObj : TJsonObject;
//  designEntryObj : TJsonObject;
//  copyFileObj : TJsonObject;
//  i: Integer;
begin
//  result := '';
//
//  Obj := TJsonObject.Create;
//  try
//    Obj.S['min client version'] := cDPMClientVersion;
//    metaDataObj := Obj.O['metadata'];
//    metaDataObj['id'] := FMetaData.Id;
//    metaDataObj['version'] := version.ToStringNoMeta;
//    metaDataObj['description'] := FMetaData.Description;
//    metaDataObj['authors'] := FMetaData.Authors;
//
//    //optional metadata
//    if FMetaData.ProjectUrl <> '' then
//      metaDataObj['projectUrl'] := FMetaData.ProjectUrl;
//
//    if FMetaData.RepositoryUrl <> '' then
//      metaDataObj['repositoryUrl'] := FMetaData.RepositoryUrl;
//    if FMetaData.RepositoryType <> '' then
//      metaDataObj['repositoryType'] := FMetaData.RepositoryType;
//    if FMetaData.RepositoryBranch <> '' then
//      metaDataObj['repositoryBranch'] := FMetaData.RepositoryBranch;
//    if FMetaData.RepositoryCommit <> '' then
//      metaDataObj['repositoryCommit'] := FMetaData.RepositoryCommit;
//
//    if FMetaData.License <> '' then
//      metaDataObj['license'] := FMetaData.License;
//
//    metaDataObj['licenseType'] := LicenseTypeTypeToString(FMetaData.LicenseType);
//
//
//    if FMetaData.Icon <> '' then
//    begin
//      //ensure consistent icon file name to make it easier to extract later.
//      if ExtractFileExt(FMetaData.Icon) = '.svg' then
//        metaDataObj['icon'] := cIconFileSVG
//      else
//        metaDataObj['icon'] := cIconFilePNG
//    end;
//    if FMetaData.Copyright <> '' then
//      metaDataObj['copyright'] := FMetaData.Copyright;
//
//    if FMetaData.Tags.Count > 0 then
//    begin
//      for i := 0 to FMetaData.Tags.Count -1 do
//        metaDataObj.A['tags'].Add(FMetaData.Tags[i]);
//    end;
//
//
//    if FMetaData.ReadMe <> '' then
//      metaDataObj['readme'] := FMetaData.ReadMe;
//
//    if FMetaData.ReleaseNotes <> '' then
//      metaDataObj['releaseNotes'] := FMetaData.ReleaseNotes;
//
//    metaDataObj['isTrial'] := LowerCase(BoolToStr(FMetaData.IsTrial, true));
//    metaDataObj['isCommercial'] := LowerCase(BoolToStr(FMetaData.IsCommercial, true));
//    metaDataObj['uiFramework'] := UIFrameworkTypeToString(FMetaData.UIFrameworkType);
//
//    targetPlatformObject := Obj.A['targetPlatforms'].AddObject;
//    targetPlatformObject['compiler'] := CompilerToString(targetPlatform.Compiler);
//    targetPlatformObject['platforms'] := DPMPlatformToString(targetPlatform.Platforms[0]);
//
////    if targetPlatform.Variables.Count > 0 then
////    begin
////      variablesObj := targetPlatformObject.O['variables'];
////      for i := 0 to targetPlatform.Variables.Count -1 do
////        variablesObj.S[targetPlatform.Variables.Names[i]] := targetPlatform.Variables.ValueFromIndex[i];
////    end;
//
//    if targetPlatform.Dependencies.Any then
//    begin
//      for dependency in targetPlatform.Dependencies do
//      begin
//        dependencyObj := targetPlatformObject.A['dependencies'].AddObject;
//        dependencyObj['id'] := dependency.Id;
//        dependencyObj['version'] := dependency.Version.ToString;
//      end;
//    end;
//
//    if targetPlatform.SearchPaths.Any then
//    begin
//      for searchPath in targetPlatform.SearchPaths do
//      begin
//        seachPathObj := targetPlatformObject.A['searchPaths'].AddObject;
//        seachPathObj['path'] := searchPath.Path;
//      end;
//    end;
//
//
//    if targetPlatform.RuntimeFiles.Any then
//    begin
//      for bplEntry in targetPlatform.RuntimeFiles do
//      begin
//        runtimeEntryObj := targetPlatformObject.A['runtime'].AddObject;
//        if bplEntry.BuildId <> '' then
//          runtimeEntryObj['buildId'] := bplEntry.BuildId;
//        runtimeEntryObj['src'] := bplEntry.Source; //TODO : check this is expanded with variables
//        runtimeEntryObj['copyLocal'] := bplEntry.CopyLocal;
//      end;
//    end;
//
//    if targetPlatform.DesignFiles.Any then
//    begin
//      for bplEntry in targetPlatform.DesignFiles do
//      begin
//        designEntryObj := targetPlatformObject.A['design'].AddObject;
//        if bplEntry.BuildId <> '' then
//          designEntryObj['buildId'] := bplEntry.BuildId;
//        designEntryObj['src'] := bplEntry.Source; //TODO : check this is expanded with variables
//        designEntryObj['install'] := bplEntry.Install;
//      end;
//    end;
//
//
//    if targetPlatform.BuildEntries.Any then
//    begin
//      for buildEntry in targetPlatform.BuildEntries do
//      begin
//        buildEntryObj := targetPlatformObject.A['build'].AddObject;
//        buildEntryObj['id'] := buildEntry.Id;
//        buildEntryObj['project'] := buildEntry.Project;
//        buildEntryObj['config'] := buildEntry.Config;
//        buildEntryObj['bplOutputDir'] := buildEntry.BplOutputDir;
//        buildEntryObj['libOutputDir'] := buildEntry.LibOutputDir;
//        buildEntryObj['designOnly']   := buildEntry.DesignOnly;
//        buildEntryObj['buildForDesign']   := buildEntry.BuildForDesign;
//        if buildEntry.CopyFiles.Any then
//        begin
//          for i := 0 to buildEntry.CopyFiles.Count -1 do
//          begin
//            copyFileObj := buildEntryObj.A['copyFiles'].AddObject;
//            copyFileObj['src'] := buildEntry.CopyFiles[i].Source;
//            copyFileObj.B['flatten'] := buildEntry.CopyFiles[i].Flatten;
//          end;
//        end;
//      end;
//    end;
//
//    result := Obj.ToJSON(False);
//  finally
//    Obj.Free;
//  end;
end;


function TSpec.GenerateManifestYAML(const version: TSemanticVersion; const targetPlatform: ISpecTargetPlatform): string;
begin
  raise ENotImplemented.Create('Error Message');
end;

function TSpec.GetFileName : string;
begin
  result := FFileName;
end;

function TSpec.GetIsValid : boolean;
begin
  result := FIsValid;
end;

function TSpec.GetMetaData : ISpecMetaData;
begin
  result := FMetaData;
end;

function TSpec.GetPackageKind: TDPMPackageKind;
begin
  result := FPackageKind;
end;

function TSpec.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  metaDataObj : TJsonObject;
  templatesArray : TJsonArray;
  targetPlatformsArray : TJsonArray;
begin
  FIsValid := false;
  //Logger.Debug('Reading spec metadata');
  if not jsonObject.Contains('metadata') then
  begin
    Logger.Error('Required element [metadata] not found!');
    result := false;
  end
  else
  begin
    metaDataObj := jsonObject.O['metadata'];
    result := FMetaData.LoadFromJson(metaDataObj)
  end;

  if jsonObject.Contains('templates') then
  begin
    //Logger.Debug('Reading spec templates');
    templatesArray := jsonObject.A['templates'];
    result := LoadTemplatesFromJson(templatesArray) and result;
  end;

  if not jsonObject.Contains('targetPlatforms') then
  begin
    Logger.Error('Required element [targetPlatforms] not found!');
    result := false;
  end
  else
  begin
    //Logger.Debug('Reading spec targetPlatforms');
    targetPlatformsArray := jsonObject.A['targetPlatforms'];
    result := LoadTargetPlatformsFromJson(targetPlatformsArray) and result;
  end;

  FIsValid := result;
end;



function TSpec.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  metaData : IYAMLValue;
  templates : IYAMLSequence;
  targetPlatforms : IYAMLSequence;
  sPackageKind : string;
  sMinClient : string;
  minClient : TPackageVersion;
  currentVersion : TPackageVersion;
  error : string;
  variablesObj : IYAMLMapping;
begin
  result := true;
  //preserve comments
  LoadComments(yamlObject);

  sMinClient := yamlObject.S['min dpm client version'];
  if sMinClient <> '' then
  begin
    currentVersion := TPackageVersion.Parse(cDPMClientVersion);
    if not TPackageVersion.TryParseWithError(sMinClient, minClient, error) then
    begin
      Logger.Error('Invalid "min dpm client version" in dspec : ' + sMinClient);
    end
    else
    begin
      if minClient > currentVersion then
        raise Exception.Create('Package spec requires newer client version : ' + sMinClient + #13#10 + 'Update your dpm client');
    end;
  end;

  sPackageKind := yamlObject.S['packageKind'];
  if sPackageKind <> '' then
    FPackageKind := StringToPackageKind(sPackageKind)
  else
    FPackageKind := TDPMPackageKind.dpm;

  if yamlObject.Contains('metadata') then
  begin
    metaData := yamlObject.Values['metadata'];
    FMetaData.LoadFromYAML(metaData.AsMapping);
  end
  else
  begin
    Logger.Error('Required element [metadata] not found!');
    result := false;
  end;

  variablesObj := yamlObject.O['variables'];
  result := LoadVariablesFromYAML(variablesObj) and result;

  if not yamlObject.Contains('targetPlatforms') then
  begin
    Logger.Error('Required element [targetPlatforms] not found!');
    result := false;
  end
  else
  begin
    targetPlatforms := yamlObject.A['targetPlatforms'];
    result := LoadTargetPlatformsFromYAML(targetPlatforms) and result;
  end;


  if yamlObject.Contains('templates') then
  begin
    templates := yamlObject.A['templates'];
    result := LoadTemplatesFromYAML(templates) and result;
  end
  else
  begin
    Logger.Error('Required element [templates] not found!');
    result := false;

  end;



  FIsValid := result;

end;

function TSpec.LoadTargetPlatformsFromJson(const targetPlatformsArray : TJsonArray) : boolean;
var
  i : integer;
  targetPlatform : ISpecTargetPlatform;
begin
  result := true;
  if targetPlatformsArray.Count = 0 then
  begin
    Logger.Error('No targetPlatforms found, at least 1 is required');
    exit(false);
  end;

  for i := 0 to targetPlatformsArray.Count - 1 do
  begin
    targetPlatform := TSpecTargetPlatform.Create(Logger);
    FTargetPlatforms.Add(targetPlatform);
    result := targetPlatform.LoadFromJson(targetPlatformsArray.O[i]) and result;
  end;

end;


function TSpec.LoadTargetPlatformsFromYAML(const targetPlatformsSeq: IYAMLSequence): boolean;
var
  i : integer;
  targetPlatform : ISpecTargetPlatform;
begin
  if targetPlatformsSeq.HasComments then
  begin
    FTargetPlatformsComments := TStringList.Create;
    FTargetPlatformsComments.Assign(targetPlatformsSeq.Comments);
  end;

  result := true;
  if targetPlatformsSeq.Count > 0 then
  begin
    for i := 0 to targetPlatformsSeq.Count - 1 do
    begin
      targetPlatform := TSpecTargetPlatform.Create(Logger);
      FTargetPlatforms.Add(targetPlatform);
      result := targetPlatform.LoadFromYAML(targetPlatformsSeq.O[i]) and result;
    end;
  end
  else
  begin
    Logger.Error('No targetPlatforms found, at least 1 is required');
    exit(false);
  end;
end;

function TSpec.LoadTemplateFromJson(const templateObj : TJsonObject; const templateNo : integer) : boolean;
var
  template : ISpecTemplate;
begin
  result := true;
  if not templateObj.Contains('name') then
  begin
    result := false;
    Logger.Error('Template #' + IntToStr(templateNo) + ' is missing the required name field!');
  end;
  template := TSpecTemplate.Create(Self.Logger);
  result := result and template.LoadFromJson(templateObj);
  FTemplates.Add(template);
end;


function TSpec.LoadTemplateFromYAML(const templateObj: IYAMLMapping; const templateNo: integer): boolean;
var
  template : ISpecTemplate;
begin
  result := true;
  if not templateObj.Contains('name') then
  begin
    result := false;
    Logger.Error('Template #' + IntToStr(templateNo) + ' is missing the required name field!');
  end;
  template := TSpecTemplate.Create(Self.Logger);
  result := result and template.LoadFromYAML(templateObj);
  FTemplates.Add(template);

end;

function TSpec.LoadTemplatesFromJson(const templatesArray : TJsonArray) : boolean;
var
  i : integer;
begin
  result := true;
  if templatesArray.Count > 0 then
  begin
    for i := 0 to templatesArray.Count - 1 do
      result := LoadTemplateFromJson(templatesArray.O[i], i + 1) and result;
  end;
end;


function TSpec.LoadTemplatesFromYAML(const templatesSeq: IYAMLSequence): boolean;
var
  i : integer;
begin
  result := true;
  if templatesSeq.HasComments then
  begin
    FTemplatesComments := TStringList.Create;
    FTemplatesComments.Assign(templatesSeq.Comments);
  end;
  if templatesSeq.Count > 0 then
  begin
    for i := 0 to templatesSeq.Count - 1 do
      result := LoadTemplateFromYAML(templatesSeq.O[i], i + 1) and result;
  end
  else
  begin
    result := false;
    Logger.Error('At least 1 template is required');
  end;
end;

function TSpec.LoadVariablesFromYAML(const variablesObj: IYAMLMapping): boolean;
var
  i : integer;
begin
  result := true;
  if variablesObj.HasComments then
  begin
    FVariablesComments := TStringList.Create;
    FVariablesComments.Assign(variablesObj.Comments);
  end;
  if variablesObj.Count > 0 then
  begin
    for i := 0 to variablesObj.Count -1 do
    begin
      try
        FVariables[LowerCase(variablesObj.Keys[i])] := variablesObj.Items[variablesObj.Keys[i]].AsString;
      except
        on e : Exception do
        begin
          result := false;
          Logger.Error('Error loading variable : ' + e.Message);
        end;
      end;
    end;
  end;
end;

function TSpec.NewTemplate(const name: string): ISpecTemplate;
begin
  if FindTemplate(name) <> nil then
    raise Exception.Create('Template name already exists');

  result := TSpecTemplate.Create(Logger);
  result.Name := name;

  GetTemplates.Add(result);
end;


procedure TSpec.RenameTemplate(const currentTemplateName, NewTemplateName: string);
var
  currentTemplate : ISpecTemplate;
begin
  currentTemplate := FindTemplate(currentTemplateName);
  if not Assigned(currentTemplate) then
    raise Exception.Create('Template not found');

  currentTemplate.Name := NewTemplateName;
end;

function TSpec.ReplaceTokens(const version : TPackageVersion; const properties : TStringList) : boolean;
//var
//  tokenList : TStringList;
//  targetPlatform : ISpecTargetPlatform;
//  fileEntry : ISpecSourceEntry;
//  buildEntry : ISpecBuildEntry;
//  designEntry : ISpecDesignEntry;
//  dependency : ISpecDependency;
//  regEx : TRegEx;
//  evaluator : TMatchEvaluator;

begin

  result := true;
//  Logger.Information('Replacing tokens..');
//  tokenList := TStringList.Create;
//  FCurrentTokens := tokenList;
//  try
//    try
//      regEx := TRegEx.Create('\$(\w+)\$');
//      evaluator := TokenMatchEvaluator; //work around for compiler overload resolution issue.
//      for targetPlatform in FTargetPlatforms do
//      begin
//        GetTokensForTargetPlatform(targetPlatform, version, tokenList, properties);
//        FMetaData.Id := regEx.Replace(FMetaData.Id, evaluator);
//        FMetaData.Description := regEx.Replace(FMetaData.Description, evaluator);
//        FMetaData.Authors := regEx.Replace(FMetaData.Authors, evaluator);
//        if FMetaData.ProjectUrl <> '' then
//          FMetaData.ProjectUrl := regEx.Replace(FMetaData.ProjectUrl, evaluator);
//        if FMetaData.RepositoryUrl <> '' then
//          FMetaData.RepositoryUrl := regEx.Replace(FMetaData.RepositoryUrl, evaluator);
//        if FMetaData.RepositoryType <> '' then
//          FMetaData.RepositoryType := regEx.Replace(FMetaData.RepositoryType, evaluator);
//        if FMetaData.RepositoryBranch <> '' then
//          FMetaData.RepositoryBranch := regEx.Replace(FMetaData.RepositoryBranch, evaluator);
//        if FMetaData.RepositoryCommit <> '' then
//          FMetaData.RepositoryCommit := regEx.Replace(FMetaData.RepositoryCommit, evaluator);
//        FMetaData.License := regEx.Replace(FMetaData.License, evaluator);
//        if FMetaData.Icon <> '' then
//          FMetaData.Icon := regEx.Replace(FMetaData.Icon, evaluator);
//        if FMetaData.Copyright <> '' then
//          FMetaData.Copyright := regEx.Replace(FMetaData.Copyright, evaluator);
//
//        if FMetaData.Tags.Count > 0 then
//          FMetaData.Tags.Text := regEx.Replace(FMetaData.Tags.Text, evaluator);
//
//        for fileEntry in targetPlatform.SourceEntries do
//        begin
//          fileEntry.Source := regEx.Replace(fileEntry.Source, evaluator);
//          fileEntry.Destination := regEx.Replace(fileEntry.Destination, evaluator);
//        end;
//
//        for designEntry in targetPlatform.DesignEntries do
//        begin
//          designEntry.Project := Trim(regEx.Replace(designEntry.Project, evaluator));
//          designEntry.Defines := Trim(regEx.Replace(designEntry.Defines, evaluator));
//        end;
//
//        for buildEntry in targetPlatform.BuildEntries do
//        begin
//          buildEntry.Project := regEx.Replace(buildEntry.Project, evaluator);
//
//        end;
//        for dependency in targetPlatform.Dependencies do
//        begin
//          if dependency.VersionString = '$version$' then
//            dependency.Version := TVersionRange.Create(FMetaData.Version);
//        end;
//      end;
//
//    finally
//      tokenList.Free;
//      FCurrentTokens := nil;
//    end;
//  except
//    on e : Exception do
//    begin
//      Logger.Error('Error replacing tokens : ' + e.Message);
//      result := false;
//    end;
//
//  end;

end;

procedure TSpec.SetPackageKind(const value: TDPMPackageKind);
begin
  FPackageKind := value;
end;


function TSpec.ToJSON: string;
var
  json : TJsonObject;
begin
  FTargetPlatforms.Sort(TComparer<ISpecTargetPlatform>.Construct(
  function(const Left, Right: ISpecTargetPlatform): Integer
   begin
      if Ord(left.Compiler) = Ord(right.Compiler) then
        result := 0
      else if Ord(Left.Compiler) > Ord(Right.Compiler) then
        result := 1
      else
        result := -1;
   end));
  json := TJsonObject.Create;
  try
    json.O['metadata'] := TJsonObject.Parse(FMetaData.ToJSON) as TJsonObject;
    json.A['targetPlatforms'] := LoadObjectList(FTargetPlatforms as IList<ISpecNode>);
    json.A['templates'] := LoadObjectList(FTemplates as IList<ISpecNode>);

    Result := json.ToJSON(False);
  finally
    FreeAndNil(json);
  end;
end;

function TSpec.TokenMatchEvaluator(const match : TMatch) : string;
begin
  if match.Success and (match.Groups.Count = 2) then
  begin
    //  Logger.Debug('Replacing ' + match.Groups.Item[1].Value);
    if FCurrentTokens.IndexOfName(match.Groups.Item[1].Value) <> -1 then
      result := FCurrentTokens.Values[match.Groups.Item[1].Value]
    else
      raise Exception.Create('Unknown token [' + match.Groups.Item[1].Value + ']');
  end
  else
    result := match.Value;
end;

procedure TSpec.ToYAML(const parent: IYAMLValue; const packageKind : TDPMPackageKind);
var
  root : IYAMLMapping;
  targetPlatforms : IYAMLSequence;
  templates : IYAMLSequence;
  variables : IYAMLMapping;
  i : integer;
begin
  root := parent.AsMapping;

  if Self.HasComments then
    root.Comments.Assign(Self.Comments)
  else
    root.Comments.Add('# DPM package spec file');

  root.S['min dpm client version'] := cDPMClientVersion;
  if packageKind <> TDPMPackageKind.dpm then
    root.S['packageKind'] := PackageKindToString(FPackageKind);

  FMetaData.ToYAML(root, packageKind);
  targetPlatforms := root.A['targetPlatforms'];

  //attempting to preserve comments
  if FTargetPlatformsComments <> nil then
    targetPlatforms.Comments.Assign(FTargetPlatformsComments);

  for i := 0 to FTargetPlatforms.Count -1 do
    FTargetPlatforms[i].ToYAML(targetPlatforms, packageKind);

  templates := root.A['templates'];

  if FTemplatesComments <> nil then
    templates.Comments.Assign(FTemplatesComments);

  for i := 0 to FTemplates.Count -1 do
    FTemplates[i].ToYAML(templates, packageKind);

  if FVariables.Count > 0 then
  begin
    variables := root.O['variables'];
    if FVariablesComments <> nil then
      variables.Comments.Assign(FVariablesComments);
  end;



end;

procedure TSpec.ToYAMLFile(const fileName: string);
var
  doc : IYAMLDocument;
  root : IYAMLMapping;

begin
  FTargetPlatforms.Sort(TComparer<ISpecTargetPlatform>.Construct(
  function(const Left, Right: ISpecTargetPlatform): Integer
   begin
      if Ord(left.Compiler) = Ord(right.Compiler) then
        result := 0
      else if Ord(Left.Compiler) > Ord(Right.Compiler) then
        result := 1
      else
        result := -1;
   end));

  doc := TYAML.CreateMapping;
  root := doc.AsMapping;
  ToYAML(doc.AsMapping, FPackageKind);
  TYAML.WriteToFile(doc, fileName);
end;

end.

