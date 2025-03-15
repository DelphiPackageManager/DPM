{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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
  DPM.Core.Types,
  DPM.Core.Logging,
  VSoft.SemanticVersion,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node,
  JsonDataObjects;


type
  TSpec = class(TSpecNode, IPackageSpec)
  private
    FMetaData : ISpecMetaData;
    FTargetPlatforms : IList<ISpecTargetPlatform>;
    FTemplates : IList<ISpecTemplate>;
    FIsValid : boolean;
    FCurrentTokens : TStringList;
    FFileName : string;
  public
    function ApplyDependencies(const targetPlatform : ISpecTargetPlatform; const dependencies : IList<ISpecDependency>) : boolean;
    function ApplySearchPaths(const targetPlatform : ISpecTargetPlatform; const searchPaths : IList<ISpecSearchPath>) : boolean;

    function ApplyLibrary(const targetPlatform : ISpecTargetPlatform; const libs : IList<ISpecFileEntry>) : boolean;
    function ApplySource(const targetPlatform : ISpecTargetPlatform; const sourceFiles : IList<ISpecFileEntry>) : boolean;
    function ApplyOtherFiles(const targetPlatform : ISpecTargetPlatform; const files : IList<ISpecFileEntry>) : boolean;
    function ApplyDesign(const targetPlatform : ISpecTargetPlatform; const designFiles : IList<ISpecBPLEntry>) : boolean;
    function ApplyRuntime(const targetPlatform : ISpecTargetPlatform; const runtimeFiles : IList<ISpecBPLEntry>) : boolean;
    function ApplyBuild(const targetPlatform : ISpecTargetPlatform; const buildEntries : IList<ISpecBuildEntry>) : boolean;

    function ApplyTemplates : Boolean;
    function ExpandTargetPlatforms : boolean;
    function ReplaceTokens(const version : TPackageVersion; const properties : TStringList) : boolean;

    procedure GetTokensForTargetPlatform(const targetPlatform : ISpecTargetPlatform; const version : TPackageVersion; const list : TStringList; const externalProps : TStringList);

    function TokenMatchEvaluator(const match : TMatch) : string;
    function PreProcess(const version : TPackageVersion; const properties : TStringList) : boolean;
    function GenerateManifestJson(const version : TSemanticVersion; const targetPlatform : ISpecTargetPlatform) : string;

    function GetFileName : string;
    function GetIsValid : boolean;
    function GetMetaData : ISpecMetaData;
    function GetTargetPlatform : ISpecTargetPlatform;
    function GetTargetPlatforms : IList<ISpecTargetPlatform>;
    function GetTemplates : IList<ISpecTemplate>;
    function LoadTemplateFromJson(const templateObj : TJsonObject; const templateNo : integer) : boolean;

    function LoadTemplatesFromJson(const templatesArray : TJsonArray) : boolean;

    function LoadTargetPlatformsFromJson(const targetPlatformsArray : TJsonArray) : boolean;
    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function ToJSON: string; override;

    // Template functions
    function FindTemplate(const name : string) : ISpecTemplate;
    function NewTemplate(const name : string) : ISpecTemplate;
    procedure RenameTemplate(const currentTemplateName: string; const NewTemplateName:string);
    procedure DeleteTemplate(const templateName: string);
    function DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;
  public
    constructor Create(const logger : ILogger; const fileName : string); reintroduce;
  end;


implementation

uses
  System.Generics.Defaults,
  DPM.Core.Constants,
  DPM.Core.Dependency.Version,
  DPM.Core.Spec.MetaData,
  DPM.Core.Spec.Template,
  DPM.Core.Spec.FileEntry,
  DPM.Core.Spec.TargetPlatform,
  DPM.Core.Utils.Strings;

{ TSpec }



function TSpec.ApplyBuild(const targetPlatform : ISpecTargetPlatform; const buildEntries : IList<ISpecBuildEntry>) : boolean;
var
  existing : ISpecBuildEntry;
  newBuildEntry : ISpecBuildEntry;
begin
  result := true;
  for newBuildEntry in buildEntries do
  begin
    existing := targetPlatform.FindBuildEntryById(newBuildEntry.id);
    if existing <> nil then
    begin
      result := false;
      Logger.Error('Duplicate Build entry ' + newBuildEntry.Id + '] found in targetPlatform and template');
    end
    else
    begin
      targetPlatform.BuildEntries.Add(newBuildEntry.Clone);
    end;
  end;
end;

function TSpec.ApplyDependencies(const targetPlatform : ISpecTargetPlatform; const dependencies : IList<ISpecDependency>) : boolean;
var
  newDependency : ISpecDependency;
  existingDependency : ISpecDependency;
  depGroup : ISpecDependencyGroup;
begin
  result := true;
  if not dependencies.Any then
    exit;

  //first see if there is a group for this target platform, if there is then that is all we will use.

  depGroup := dependencies.FirstOrDefault(
    function(const dependency : ISpecDependency) : boolean
    var
      group : ISpecDependencyGroup;
    begin
      result := dependency.IsGroup;
      if result then
      begin
        group := dependency as ISpecDependencyGroup;
        result := (group.TargetPlatform.Compiler = targetPlatform.Compiler)
        and (group.TargetPlatform.Platform = targetPlatform.Platforms[0]);
      end;

    end) as ISpecDependencyGroup;

  //group replaces existing.
  if depGroup <> nil then
  begin
    targetPlatform.Dependencies.Clear;
    for existingDependency in depGroup.dependencies do
    begin
      newDependency := existingDependency.Clone;
      targetPlatform.Dependencies.Add(newDependency);
    end;
    exit;
  end;

  for newDependency in dependencies do
  begin
    existingDependency := targetPlatform.FindDependencyById(newDependency.Id);
    if existingDependency <> nil then
    begin
      result := false;
      Logger.Error('Duplicate dependency [' + newDependency.Id + '] found in targetPlatform and template');
    end
    else
    begin
      existingDependency := newDependency.Clone;
      targetPlatform.Dependencies.Add(existingDependency);
    end;

  end;



end;

function TSpec.ApplyDesign(const targetPlatform : ISpecTargetPlatform; const designFiles : IList<ISpecBPLEntry>) : boolean;
var
  existing : ISpecBPLEntry;
  newBPL : ISpecBPLEntry;
begin
  result := true;
  for newBPL in designFiles do
  begin
    existing := targetPlatform.FindDesignBplBySrc(newBPL.Source);
    if existing <> nil then
    begin
      result := false;
      Logger.Error('Duplicate Design bpl ' + newBPL.Source + '] found in targetPlatform and template');
    end
    else
    begin
      existing := newBPL.Clone;
      targetPlatform.DesignFiles.Add(existing.Clone);
    end;
  end;
end;

function TSpec.ApplyLibrary(const targetPlatform : ISpecTargetPlatform; const libs : IList<ISpecFileEntry>) : boolean;
var
  existing : ISpecFileEntry;
  newEntry : ISpecFileEntry;
begin
  result := true;
  for newEntry in libs do
  begin
    existing := targetPlatform.FindLibFileBySrc(newEntry.Source);
    if existing <> nil then
    begin
      result := false;
      Logger.Error('Duplicate Lib entry ' + newEntry.Source + '] found in targetPlatform and template');
    end
    else
    begin
      existing := newEntry.Clone;
      targetPlatform.LibFiles.Add(existing.Clone);
    end;
  end;

end;

function TSpec.ApplyOtherFiles(const targetPlatform : ISpecTargetPlatform; const files : IList<ISpecFileEntry>) : boolean;
var
  existing : ISpecFileEntry;
  newEntry : ISpecFileEntry;
begin
  result := true;
  for newEntry in files do
  begin
    existing := targetPlatform.FindOtherFileBySrc(newEntry.Source);
    if existing <> nil then
    begin
      result := false;
      Logger.Error('Duplicate files entry ' + newEntry.Source + '] found in targetPlatform and template');
    end
    else
    begin
      existing := newEntry.Clone;
      targetPlatform.Files.Add(existing.Clone);
    end;
  end;

end;

function TSpec.ApplyRuntime(const targetPlatform : ISpecTargetPlatform; const runtimeFiles : IList<ISpecBPLEntry>) : boolean;
var
  existing : ISpecBPLEntry;
  newBPL : ISpecBPLEntry;
begin
  result := true;
  for newBPL in runtimeFiles do
  begin
    existing := targetPlatform.FindRuntimeBplBySrc(newBPL.Source);
    if existing <> nil then
    begin
      result := false;
      Logger.Error('Duplicate Runtime bpl ' + newBPL.Source + '] found in targetPlatform and template');
    end
    else
    begin
      existing := newBPL.Clone;
      targetPlatform.RuntimeFiles.Add(existing.Clone);
    end;
  end;

end;

function TSpec.ApplySearchPaths(const targetPlatform : ISpecTargetPlatform; const searchPaths : IList<ISpecSearchPath>) : boolean;
var
  newSearchPath : ISpecSearchPath;
  existingSearchPath : ISpecSearchPath;
  searchPathGroup : ISpecSearchPathGroup;
begin
  result := true;
  if not searchPaths.Any then
    exit;

  //first see if there is a group for this target platform, if there is then that is all we will use.

  searchPathGroup := searchPaths.FirstOrDefault(
    function(const searchPath : ISpecSearchPath) : boolean
    var
      group : ISpecSearchPathGroup;
    begin
      result := searchPath.IsGroup;
      if result then
      begin
        group := searchPath as ISpecSearchPathGroup;
        result := (group.TargetPlatform.Compiler = targetPlatform.Compiler)
        and (group.TargetPlatform.Platform = targetPlatform.Platforms[0]);
      end;

    end) as ISpecSearchPathGroup;

  //if we have a group that matches the targetPlatform then we replace the searchPaths with it's searchPaths
  if searchPathGroup <> nil then
  begin
    targetPlatform.SearchPaths.Clear;
    for newSearchPath in searchPathGroup.SearchPaths do
    begin
      existingSearchPath := newSearchPath.Clone;
      targetPlatform.SearchPaths.Add(existingSearchPath);
    end;
    exit;
  end;

  for newSearchPath in searchPaths do
  begin
    existingSearchPath := targetPlatform.FindSearchPathByPath(newSearchPath.Path);
    if existingSearchPath <> nil then
    begin
      result := false;
      Logger.Error('Duplicate searchPath [' + existingSearchPath.Path + '] found in targetPlatform and template');
    end
    else
    begin
      existingSearchPath := newSearchPath.Clone;
      targetPlatform.SearchPaths.Add(existingSearchPath);
    end;
  end;
end;

function TSpec.ApplySource(const targetPlatform : ISpecTargetPlatform; const sourceFiles : IList<ISpecFileEntry>) : boolean;
var
  existing : ISpecFileEntry;
  newEntry : ISpecFileEntry;
begin
  result := true;
  for newEntry in sourceFiles do
  begin
    existing := targetPlatform.FindSourceFileBySrc(newEntry.Source);
    if existing <> nil then
    begin
      result := false;
      Logger.Error('Duplicate Source entry ' + newEntry.Source + '] found in targetPlatform and template');
    end
    else
    begin
      existing := newEntry.Clone;
      targetPlatform.SourceFiles.Add(existing.Clone);
    end;
  end;
end;

function TSpec.ApplyTemplates : Boolean;
var
  template : ISpecTemplate;
  targetPlatform : ISpecTargetPlatform;
  error : boolean;
begin
  result := true;
  error := false;
  Logger.Information('Applying templates..');
  //if any targetPlatforms are missing a template then exit
  FTargetPlatforms.ForEach(
    procedure(const item : ISpecTargetPlatform)
    begin
      if item.TemplateName = '' then
      begin
        error := true;
        Logger.Error('TargetPlatform ' + item.ToString);
      end;
    end);

  if error then
    exit(false);

  if not FTargetPlatforms.Any(function(const item : ISpecTargetPlatform) : boolean
    begin
      result := item.TemplateName <> '';
    end) then
    exit;

  //if we don't have templates then the spec is not valid.
  if not FTemplates.Any then
  begin
    Logger.Error('No templates were found but targetPlatforms reference a template.');
    exit(false);
  end;


  for targetPlatform in FTargetPlatforms do
  begin
    if SameText(targetPlatform.TemplateName, cUnset) then
      continue;

    template := FindTemplate(targetPlatform.TemplateName);
    if template <> nil then
    begin
      result := ApplyDependencies(targetPlatform, template.Dependencies) and result;
      result := ApplySearchPaths(targetPlatform, template.SearchPaths) and result;
      result := ApplyLibrary(targetPlatform, template.LibFiles) and result;
      result := ApplySource(targetPlatform, template.SourceFiles) and result;
      result := ApplyOtherFiles(targetPlatform, template.Files) and result;
      result := ApplyRuntime(targetPlatform, template.RuntimeFiles) and result;
      result := ApplyDesign(targetPlatform, template.DesignFiles) and result;
      result := ApplyBuild(targetPlatform, template.BuildEntries) and result;
    end
    else
    begin
      Logger.Error('A referenced template [' + targetPlatform.TemplateName + '] was not found.');
      result := false
    end;
  end;

end;

constructor TSpec.Create(const logger : ILogger; const fileName : string);
begin
  inherited Create(logger);
  FFileName := fileName;
  FMetaData := TSpecMetaData.Create(logger);
  FTargetPlatforms := TCollections.CreateSortedList<ISpecTargetPlatform>(
   function(const Left, Right: ISpecTargetPlatform): Integer
   begin
      if left.Compiler = right.Compiler then
        result := 0
      else if Left.Compiler > Right.Compiler then
        result := 1
      else
        result := -1;
   end
  );
  FTemplates := TCollections.CreateSortedList<ISpecTemplate>(
   function(const Left, Right: ISpecTemplate): Integer
   begin
      result := CompareText(Left.Name, Right.Name);
   end
  );
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

function TSpec.DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;
var
  I: Integer;
begin
  result := TSpecTemplate.Create(Logger);
  for I := 0 to sourceTemplate.Dependencies.Count - 1 do
  begin
    result.Dependencies.Add(sourceTemplate.Dependencies[i].Clone);
  end;
  for I := 0 to sourceTemplate.DesignFiles.Count - 1 do
  begin
    result.DesignFiles.Add(sourceTemplate.DesignFiles[i].Clone);
  end;
  for I := 0 to sourceTemplate.Files.Count - 1 do
  begin
    result.Files.Add(sourceTemplate.Files[i].Clone);
  end;
  for I := 0 to sourceTemplate.LibFiles.Count - 1 do
  begin
    result.LibFiles.Add(sourceTemplate.LibFiles[i].Clone);
  end;
  for I := 0 to sourceTemplate.RuntimeFiles.Count - 1 do
  begin
    result.RuntimeFiles.Add(sourceTemplate.RuntimeFiles[i].Clone);
  end;
  for I := 0 to sourceTemplate.SourceFiles.Count - 1 do
  begin
    result.SourceFiles.Add(sourceTemplate.SourceFiles[i].Clone);
  end;
  for I := 0 to sourceTemplate.SearchPaths.Count - 1 do
  begin
    result.SearchPaths.Add(sourceTemplate.SearchPaths[i].Clone);
  end;
  for I := 0 to sourceTemplate.SearchPaths.Count - 1 do
  begin
    result.SearchPaths.Add(sourceTemplate.SearchPaths[i].Clone);
  end;
  result.Name := newTemplateName;
  FTemplates.Add(result);
end;

function TSpec.ExpandTargetPlatforms : boolean;
var
  newTargetPlatforms : IList<ISpecTargetPlatform>;
  toRemoveTargetPlatforms : IList<ISpecTargetPlatform>;
  toProcessTargetPlatforms : TArray<ISpecTargetPlatform>;
  targetPlatform : ISpecTargetPlatform;
  newTargetPlatform : ISpecTargetPlatform;
  platform : TDPMPlatform;
begin
  result := true;
  Logger.Information('Expanding targetPlatforms');

  toProcessTargetPlatforms := FTargetPlatforms.Where(
    function(const tp : ISpecTargetPlatform) : boolean
    begin
      result := Length(tp.Platforms) > 1;
    end).ToArray;

  if length(toProcessTargetPlatforms) = 0 then
    exit;

  newTargetPlatforms := TCollections.CreateList<ISpecTargetPlatform>;
  toRemoveTargetPlatforms := TCollections.CreateList<ISpecTargetPlatform>;

  try
    for targetPlatform in toProcessTargetPlatforms do
    begin
      for platform in targetPlatform.Platforms do
      begin
        newTargetPlatform := targetPlatform.CloneForPlatform(platform);
        newTargetPlatforms.Add(newTargetPlatform);
        //Logger.Debug('Expanded ' + CompilerToString(newTargetPlatform.Compiler) + '.' + DPMPlatformToString(newTargetPlatform.Platforms[0]));
      end;
      toRemoveTargetPlatforms.Add(targetPlatform);
    end;
    FTargetPlatforms.RemoveRange(toRemoveTargetPlatforms);
    FTargetPlatforms.AddRange(newTargetPlatforms);
  except
    on e : Exception do
    begin
      Logger.Error('Error expanding TargetPlatforms : ' + e.Message);
      result := false;
    end;
  end;

  //TODO : Sort targetPlatforms by compiler then platform
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
    list.Values[targetPlatform.Variables.Names[i]] := '';
    list.Add(targetPlatform.Variables.Names[i] + '=' + targetPlatform.Variables.ValueFromIndex[i]);
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

function TSpec.GetTemplates : IList<ISpecTemplate>;
begin
  result := FTemplates;
end;

function TSpec.GenerateManifestJson(const version : TSemanticVersion; const targetPlatform : ISpecTargetPlatform) : string;
var
  Obj : TJsonObject;
  dependency : ISpecDependency;
  searchPath : ISpecSearchPath;
  buildEntry : ISpecBuildEntry;
  bplEntry : ISpecBPLEntry;
  metaDataObj : TJsonObject;
  targetPlatformObject : TJDOJsonObject;
//  variablesObj : TJsonObject;
  dependencyObj : TJsonObject;
  seachPathObj : TJsonObject;
  buildEntryObj : TJsonObject;
  runtimeEntryObj : TJsonObject;
  designEntryObj : TJsonObject;
  copyFileObj : TJsonObject;
  i: Integer;
begin
  result := '';

  Obj := TJsonObject.Create;
  try
    metaDataObj := Obj.O['metadata'];
    metaDataObj['id'] := FMetaData.Id;
    metaDataObj['version'] := version.ToStringNoMeta;
    metaDataObj['description'] := FMetaData.Description;
    metaDataObj['authors'] := FMetaData.Authors;
    
    //optional metadata
    if FMetaData.ProjectUrl <> '' then
      metaDataObj['projectUrl'] := FMetaData.ProjectUrl;

    if FMetaData.RepositoryUrl <> '' then
      metaDataObj['repositoryUrl'] := FMetaData.RepositoryUrl;
    if FMetaData.RepositoryType <> '' then
      metaDataObj['repositoryType'] := FMetaData.RepositoryType;
    if FMetaData.RepositoryBranch <> '' then
      metaDataObj['repositoryBranch'] := FMetaData.RepositoryBranch;
    if FMetaData.RepositoryCommit <> '' then
      metaDataObj['repositoryCommit'] := FMetaData.RepositoryCommit;

    if FMetaData.License <> '' then
      metaDataObj['license'] := FMetaData.License;

    metaDataObj['licenseType'] := LicenseTypeTypeToString(FMetaData.LicenseType);


    if FMetaData.Icon <> '' then
    begin
      //ensure consistent icon file name to make it easier to extract later.
      if ExtractFileExt(FMetaData.Icon) = '.svg' then
        metaDataObj['icon'] := cIconFileSVG
      else
        metaDataObj['icon'] := cIconFilePNG
    end;
    if FMetaData.Copyright <> '' then
      metaDataObj['copyright'] := FMetaData.Copyright;
    if FMetaData.Tags <> '' then
      metaDataObj['tags'] := FMetaData.tags;

    if FMetaData.ReadMe <> '' then
      metaDataObj['readme'] := FMetaData.ReadMe;

    if FMetaData.ReleaseNotes <> '' then
      metaDataObj['releaseNotes'] := FMetaData.ReleaseNotes;

    metaDataObj['isTrial'] := LowerCase(BoolToStr(FMetaData.IsTrial, true));
    metaDataObj['isCommercial'] := LowerCase(BoolToStr(FMetaData.IsCommercial, true));
    metaDataObj['uiFramework'] := UIFrameworkTypeToString(FMetaData.UIFrameworkType);

    targetPlatformObject := Obj.A['targetPlatforms'].AddObject;
    targetPlatformObject['compiler'] := CompilerToString(targetPlatform.Compiler);
    targetPlatformObject['platforms'] := DPMPlatformToString(targetPlatform.Platforms[0]);

//    if targetPlatform.Variables.Count > 0 then
//    begin
//      variablesObj := targetPlatformObject.O['variables'];
//      for i := 0 to targetPlatform.Variables.Count -1 do
//        variablesObj.S[targetPlatform.Variables.Names[i]] := targetPlatform.Variables.ValueFromIndex[i];
//    end;

    if targetPlatform.Dependencies.Any then
    begin
      for dependency in targetPlatform.Dependencies do
      begin
        dependencyObj := targetPlatformObject.A['dependencies'].AddObject;
        dependencyObj['id'] := dependency.Id;
        dependencyObj['version'] := dependency.Version.ToString;
      end;
    end;

    if targetPlatform.SearchPaths.Any then
    begin
      for searchPath in targetPlatform.SearchPaths do
      begin
        seachPathObj := targetPlatformObject.A['searchPaths'].AddObject;
        seachPathObj['path'] := searchPath.Path;
      end;
    end;


    if targetPlatform.RuntimeFiles.Any then
    begin
      for bplEntry in targetPlatform.RuntimeFiles do
      begin
        runtimeEntryObj := targetPlatformObject.A['runtime'].AddObject;
        if bplEntry.BuildId <> '' then
          runtimeEntryObj['buildId'] := bplEntry.BuildId;
        runtimeEntryObj['src'] := bplEntry.Source; //TODO : check this is expanded with variables
        runtimeEntryObj['copyLocal'] := bplEntry.CopyLocal;
      end;
    end;

    if targetPlatform.DesignFiles.Any then
    begin
      for bplEntry in targetPlatform.DesignFiles do
      begin
        designEntryObj := targetPlatformObject.A['design'].AddObject;
        if bplEntry.BuildId <> '' then
          designEntryObj['buildId'] := bplEntry.BuildId;
        designEntryObj['src'] := bplEntry.Source; //TODO : check this is expanded with variables
        designEntryObj['install'] := bplEntry.Install;
      end;
    end;


    if targetPlatform.BuildEntries.Any then
    begin
      for buildEntry in targetPlatform.BuildEntries do
      begin
        buildEntryObj := targetPlatformObject.A['build'].AddObject;
        buildEntryObj['id'] := buildEntry.Id;
        buildEntryObj['project'] := buildEntry.Project;
        buildEntryObj['config'] := buildEntry.Config;
        buildEntryObj['bplOutputDir'] := buildEntry.BplOutputDir;
        buildEntryObj['libOutputDir'] := buildEntry.LibOutputDir;
        buildEntryObj['designOnly']   := buildEntry.DesignOnly;
        buildEntryObj['buildForDesign']   := buildEntry.BuildForDesign;
        if buildEntry.CopyFiles.Any then
        begin
          for i := 0 to buildEntry.CopyFiles.Count -1 do
          begin
            copyFileObj := buildEntryObj.A['copyFiles'].AddObject;
            copyFileObj['src'] := buildEntry.CopyFiles[i].Source;
            copyFileObj.B['flatten'] := buildEntry.CopyFiles[i].Flatten;
          end;
        end;
      end;
    end;

    result := Obj.ToJSON(False);
  finally
    Obj.Free;
  end;
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


function TSpec.NewTemplate(const name: string): ISpecTemplate;
begin
  if FindTemplate(name) <> nil then
    raise Exception.Create('Template name already exists');

  result := TSpecTemplate.Create(Logger);
  result.Name := name;

  GetTemplates.Add(result);
end;

function TSpec.PreProcess(const version : TPackageVersion; const properties : TStringList) : boolean;
begin
  result := false;
  Logger.Information('Preprocessing spec file..');
  if not ExpandTargetPlatforms then
    exit;

  if not ApplyTemplates then
    exit;

  if not ReplaceTokens(version, properties) then
    exit;

  result := true;
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
var
  tokenList : TStringList;
  targetPlatform : ISpecTargetPlatform;
  fileEntry : ISpecFileEntry;
  bplEntry : ISpecBPLEntry;
  buildEntry : ISpecBuildEntry;
  dependency : ISpecDependency;
  regEx : TRegEx;
  evaluator : TMatchEvaluator;

begin
  result := true;
  Logger.Information('Replacing tokens..');
  tokenList := TStringList.Create;
  FCurrentTokens := tokenList;
  try
    try
      regEx := TRegEx.Create('\$(\w+)\$');
      evaluator := TokenMatchEvaluator; //work around for compiler overload resolution issue.
      for targetPlatform in FTargetPlatforms do
      begin
        GetTokensForTargetPlatform(targetPlatform, version, tokenList, properties);
        FMetaData.Id := regEx.Replace(FMetaData.Id, evaluator);
        FMetaData.Description := regEx.Replace(FMetaData.Description, evaluator);
        FMetaData.Authors := regEx.Replace(FMetaData.Authors, evaluator);
        if FMetaData.ProjectUrl <> '' then
          FMetaData.ProjectUrl := regEx.Replace(FMetaData.ProjectUrl, evaluator);
        if FMetaData.RepositoryUrl <> '' then
          FMetaData.RepositoryUrl := regEx.Replace(FMetaData.RepositoryUrl, evaluator);
        if FMetaData.RepositoryType <> '' then
          FMetaData.RepositoryType := regEx.Replace(FMetaData.RepositoryType, evaluator);
        if FMetaData.RepositoryBranch <> '' then
          FMetaData.RepositoryBranch := regEx.Replace(FMetaData.RepositoryBranch, evaluator);
        if FMetaData.RepositoryCommit <> '' then
          FMetaData.RepositoryCommit := regEx.Replace(FMetaData.RepositoryCommit, evaluator);
        FMetaData.License := regEx.Replace(FMetaData.License, evaluator);
        if FMetaData.Icon <> '' then
          FMetaData.Icon := regEx.Replace(FMetaData.Icon, evaluator);
        if FMetaData.Copyright <> '' then
          FMetaData.Copyright := regEx.Replace(FMetaData.Copyright, evaluator);
        if FMetaData.Tags <> '' then
          FMetaData.Tags := regEx.Replace(FMetaData.Tags, evaluator);

        for fileEntry in targetPlatform.SourceFiles do
        begin
          fileEntry.Source := regEx.Replace(fileEntry.Source, evaluator);
          fileEntry.Destination := regEx.Replace(fileEntry.Destination, evaluator);
        end;

        for fileEntry in targetPlatform.LibFiles do
        begin
          fileEntry.Source := TRim(regEx.Replace(fileEntry.Source, evaluator));
          fileEntry.Destination := Trim(regEx.Replace(fileEntry.Destination, evaluator));
        end;

        for fileEntry in targetPlatform.Files do
        begin
          fileEntry.Source := Trim(regEx.Replace(fileEntry.Source, evaluator));
          fileEntry.Destination := Trim(regEx.Replace(fileEntry.Destination, evaluator));
        end;

        for bplEntry in targetPlatform.RuntimeFiles do
        begin
          bplEntry.Source := Trim(regEx.Replace(bplEntry.Source, evaluator));
          bplEntry.BuildId := Trim(regEx.Replace(bplEntry.BuildId, evaluator));
        end;

        for bplEntry in targetPlatform.DesignFiles do
        begin
          bplEntry.Source := Trim(regEx.Replace(bplEntry.Source, evaluator));
          bplEntry.BuildId := Trim(regEx.Replace(bplEntry.BuildId, evaluator));
        end;

        for buildEntry in targetPlatform.BuildEntries do
        begin
          buildEntry.Id := regEx.Replace(buildEntry.Id, evaluator);
          buildEntry.Project := regEx.Replace(buildEntry.Project, evaluator);
          buildEntry.BplOutputDir := regEx.Replace(buildEntry.BplOutputDir, evaluator);
          buildEntry.LibOutputDir := regEx.Replace(buildEntry.LibOutputDir, evaluator);
        end;
        for dependency in targetPlatform.Dependencies do
        begin
          if dependency.VersionString = '$version$' then
            dependency.Version := TVersionRange.Create(FMetaData.Version);
        end;

      end;

    finally
      tokenList.Free;
      FCurrentTokens := nil;
    end;
  except
    on e : Exception do
    begin
      Logger.Error('Error replacing tokens : ' + e.Message);
      result := false;
    end;

  end;

end;

function TSpec.ToJSON: string;
var
  json : TJsonObject;
begin
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

end.

