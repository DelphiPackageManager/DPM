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
  DPM.Core.Spec.Node;


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
    
    procedure GetTokensForTargetPlatform(const targetPlatform : ISpecTargetPlatform; const version : TPackageVersion; const list : TStringList; const externalProps : TStringList);

    function TokenMatchEvaluator(const match : TMatch) : string;

    function GenerateManifestYAML(const version : TSemanticVersion) : string;

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


    function LoadTemplateFromYAML(const templateObj : IYAMLMapping; const templateNo : integer) : boolean;
    function LoadTemplatesFromYAML(const templatesSeq : IYAMLSequence) : boolean;
    function LoadTargetPlatformsFromYAML(const targetPlatformsSeq : IYAMLSequence) : boolean;
    function LoadVariablesFromYAML(const variablesObj : IYAMLMapping) : boolean;

    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

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


function TSpec.GenerateManifestYAML(const version: TSemanticVersion): string;
var
  yamlDoc : IYAMLDocument;
begin
  FMetaData.Version := version;
  yamlDoc := TYAML.CreateMapping;
  Self.ToYAML(yamlDoc.Root, FPackageKind);
  yamlDoc.Options.Format := TYAMLOutputFormat.yofMixed;
  result := TYAML.WriteToString(yamlDoc);
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


procedure TSpec.SetPackageKind(const value: TDPMPackageKind);
begin
  FPackageKind := value;
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

