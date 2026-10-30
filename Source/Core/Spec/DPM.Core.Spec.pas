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
    FMinClientVersion : string;
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
    
//    procedure GetTokensForTargetPlatform(const targetPlatform : ISpecTargetPlatform; const version : TPackageVersion; const list : TStringList; const externalProps : TStringList);

    function TokenMatchEvaluator(const match : TMatch) : string;

    function GenerateDspecYAML(const version : TSemanticVersion) : string;

    //we call this when generating the dspec so it works with an unprocessed spec
    function Clone : IPackageSpec;


    function GetFileName : string;
    function GetIsValid : boolean;
    function GetMetaData : ISpecMetaData;
    function GetTargetPlatform : ISpecTargetPlatform;
    function GetTargetPlatforms : IList<ISpecTargetPlatform>;
    function GetTemplates : IList<ISpecTemplate>;
    function GetPackageKind : TDPMPackageKind;
    procedure SetPackageKind(const value : TDPMPackageKind);
    function GetMinClientVersion : string;
    procedure SetMinClientVersion(const value : string);
    function GetVariables : IVariables;
    function GetTargetPlatformsComments : TStrings;
    function GetTemplatesComments : TStrings;
    function GetVariablesComments : TStrings;


    function LoadTemplateFromYAML(const templateObj : IYAMLMapping; const templateNo : integer) : boolean;
    function LoadTemplatesFromYAML(const templatesSeq : IYAMLSequence) : boolean;
    function LoadTargetPlatformsFromYAML(const targetPlatformsSeq : IYAMLSequence) : boolean;
    function LoadVariablesFromYAML(const variablesObj : IYAMLMapping) : boolean;
    function ValidateNoCompilerOverlap : boolean;

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
  DPM.Core.Utils.Strings,
  DPM.Core.Diagnostics;

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
  FVariables := TCollections.CreateDictionary<string,string>(TIStringComparer.Ordinal); //see LoadVariablesFromYAML
end;

constructor TSpec.CreateClone(const logger: ILogger; const source: IPackageSpecPrivate);
var
  newTargetPlatform : ISpecTargetPlatform;
  newTemplate : ISpecTemplate;
  i : integer;
begin
  inherited Create(logger);
  FPackageKind := source.PackageKind;
  FMinClientVersion := source.MinClientVersion;
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

  FVariables := TCollections.CreateDictionary<string,string>(TIStringComparer.Ordinal); //see LoadVariablesFromYAML
  FVariables.AddRange(source.Variables);
end;



procedure TSpec.DeleteTemplate(const templateName: string);
var
  i: Integer;
  targetPlatform : ISpecTargetPlatform;
begin
  for i := 0 to FTemplates.Count - 1 do
  begin
    if SameText(FTemplates[i].Name, templateName) then
    begin
      FTemplates.Delete(i);
      //the platforms that used it have none now - not a name that leads nowhere. The spec then says it isn't
      //valid until they are given one.
      for targetPlatform in FTargetPlatforms do
        if SameText(targetPlatform.TemplateName, templateName) then
          targetPlatform.TemplateName := '';
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
begin
  //a full clone, so nothing the template carries (build entries, package defs, env vars...) is lost.
  result := sourceTemplate.Clone;
  result.Name := newTemplateName;
  FTemplates.Add(result);
end;

//procedure TSpec.GetTokensForTargetPlatform(const targetPlatform : ISpecTargetPlatform; const version : TPackageVersion; const list : TStringList; const externalProps : TStringList);
//var
//  i: Integer;
//  regEx : TRegEx;
//  evaluator : TMatchEvaluator;
//begin
//  list.Clear;
//  if not version.IsEmpty then
//    list.Add('version=' + version.ToString)
//  else
//    list.Add('version=' + FMetaData.Version.ToString);
//  list.Add('target=' + CompilerToString(targetPlatform.Compiler));
//  list.Add('compiler=' + CompilerToString(targetPlatform.Compiler));
//  list.Add('compilerNoPoint=' + CompilerToStringNoPoint(targetPlatform.Compiler));
//  list.Add('compilerCodeName=' + CompilerCodeName(targetPlatform.Compiler));
//  list.Add('compilerWithCodeName=' + CompilerWithCodeName(targetPlatform.Compiler));
////  list.Add('platform=' + DPMPlatformToString(targetPlatform.Platforms[0]));
//  list.Add('compilerVersion=' + CompilerToCompilerVersionIntStr(targetPlatform.Compiler));
//  list.Add('libSuffix=' + CompilerToLibSuffix(targetPlatform.Compiler));
//  list.Add('bdsVersion=' + CompilerToBDSVersion(targetPlatform.Compiler));
//  list.Add('bitness=' + DPMPlatformBitness(targetPlatform.Platforms[0]));
//  if DPMPlatformBitness(targetPlatform.Platforms[0]) = '64' then
//    list.Add('bitness64Only=' + DPMPlatformBitness(targetPlatform.Platforms[0]))
//  else
//    list.Add('bitness64Only=');
//
//  if targetPlatform.Variables.Count = 0 then
//    exit;
//
//  //override the values with values from the template.
//  for i := 0 to targetPlatform.Variables.Count -1 do
//  begin
//    //setting a value to '' removes it from the list!!!!!
//    list.Values[targetPlatform.Variables.Items[i].Key] := '';
//    list.Add(targetPlatform.Variables.Items[i].Key + '=' + targetPlatform.Variables.Items[i].Value);
//  end;
//
//  //fix up some variable overrides
//  if list.Values['compilerCodeName'] = '' then
//  begin
//    list.Values['compilerWithCodeName'] := '';
//    list.Add('compilerWithCodeName=' + CompilerToString(targetPlatform.Compiler));
//  end;
//
//
//  //apply external props passed in on command line.
//  if externalProps.Count > 0 then
//  begin
//    for i := 0 to externalProps.Count -1 do
//      list.Values[externalProps.Names[i]] := externalProps.ValueFromIndex[i];
//  end;
//
//  regEx := TRegEx.Create('\$(\w+)\$');
//  evaluator := TokenMatchEvaluator;
//
//  //variables from the spec and external may reference existing variables.
//  for i := 0 to list.Count -1 do
//  begin
//    if TStringUtils.Contains(list.ValueFromIndex[i], '$') then
//      list.ValueFromIndex[i] := regEx.Replace(list.ValueFromIndex[i], evaluator);
//  end;
//
//
//end;

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


function TSpec.GenerateDspecYAML(const version: TSemanticVersion): string;
var
  yamlDoc : IYAMLDocument;
begin
  FMetaData.Version := version;
  yamlDoc := TYAML.CreateMapping;
  Self.ToYAML(yamlDoc.Root, FPackageKind);
  yamlDoc.Options.Format := TYAMLOutputFormat.yofBlock;
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
      Logger.Error(TErrorCode.E6004_SpecValueInvalid, 'The [min dpm client version] [' + sMinClient + '] is not a valid version.');
    end
    else
    begin
      if minClient > currentVersion then
        raise EDPMError.Create(TErrorCode.E6005_SpecNeedsNewerDPM, 'The spec needs DPM ' + sMinClient + ' or later - update dpm.');
      //kept, so it is written back as declared.
      FMinClientVersion := minClient.ToStringNoMeta;
    end;
  end;

  sPackageKind := yamlObject.S['packageKind'];
  if sPackageKind <> '' then
    FPackageKind := StringToPackageKind(sPackageKind)
  else
    FPackageKind := TDPMPackageKind.dpm;
  //before the metadata is read - what it requires depends on it (a git package's version is optional).
  FMetaData.PackageKind := FPackageKind;

  if yamlObject.Contains('metadata') then
  begin
    metaData := yamlObject.Values['metadata'];
    //the metadata result must feed into the spec's validity - an invalid id (or any
    //other metadata failure) has to make the whole spec invalid, otherwise pack and
    //install just log the error and carry on regardless.
    if metaData.IsMapping then
      result := FMetaData.LoadFromYAML(metaData.AsMapping) and result
    else
    begin
      Logger.Error(TErrorCode.E6002_SpecUnreadable, 'The [metadata] of the spec must be a set of key: value lines.');
      result := false;
    end;
  end
  else
  begin
    Logger.Error(TErrorCode.E6003_SpecElementMissing, 'The required element [metadata] was not found.');
    result := false;
  end;

  variablesObj := yamlObject.O['variables'];
  result := LoadVariablesFromYAML(variablesObj) and result;

  if not yamlObject.Contains('targetPlatforms') then
  begin
    Logger.Error(TErrorCode.E6003_SpecElementMissing, 'The required element [targetPlatforms] was not found.');
    result := false;
  end
  else
  begin
    if not SequenceOf(yamlObject, 'targetPlatforms', 'the spec', targetPlatforms) then
      result := false;
    result := LoadTargetPlatformsFromYAML(targetPlatforms) and result;
    //Cross-targetPlatform invariant: each compiler must appear in at most one targetPlatform.
    //Done here (rather than in pack) so the IDE and any other consumer of TSpec also see the
    //failure as part of spec validation, and the "Spec is valid..." log never lies.
    if result then
      result := ValidateNoCompilerOverlap and result;
  end;


  if yamlObject.Contains('templates') then
  begin
    if not SequenceOf(yamlObject, 'templates', 'the spec', templates) then
      result := false;
    result := LoadTemplatesFromYAML(templates) and result;
  end
  else
  begin
    Logger.Error(TErrorCode.E6003_SpecElementMissing, 'The required element [templates] was not found.');
    result := false;

  end;

  FIsValid := result;

end;


function TSpec.ValidateNoCompilerOverlap : boolean;
var
  targetPlatform : ISpecTargetPlatform;
  expandedCompilers : TCompilerVersions;
  duplicates : TCompilerVersions;
  seenCompilers : TCompilerVersions;
  c : TCompilerVersion;
  duplicateList : string;
begin
  result := true;
  seenCompilers := [];
  for targetPlatform in FTargetPlatforms do
  begin
    expandedCompilers := ExpandedCompilersOf(targetPlatform);
    duplicates := expandedCompilers * seenCompilers;
    if duplicates <> [] then
    begin
      duplicateList := '';
      for c in duplicates do
      begin
        if duplicateList <> '' then
          duplicateList := duplicateList + ', ';
        duplicateList := duplicateList + CompilerToString(c);
      end;
      Logger.Error(TErrorCode.E6004_SpecValueInvalid,
        'targetPlatform overlap: compiler(s) [' + duplicateList + '] are covered by more than one targetPlatform. ' +
        'Each compiler must appear in exactly one targetPlatform - overlapping ranges produce conflicting packages ' +
        'and silently mask per-version variable overrides. Narrow the broader range so the two no longer overlap.');
      result := false;
    end;
    seenCompilers := seenCompilers + expandedCompilers;
  end;
end;

function TSpec.LoadTargetPlatformsFromYAML(const targetPlatformsSeq: IYAMLSequence): boolean;
var
  i : integer;
  targetPlatform : ISpecTargetPlatform;
  targetPlatformObj : IYAMLMapping;
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
      if not MappingAt(targetPlatformsSeq, i, '[targetPlatforms]', targetPlatformObj) then
      begin
        result := false;
        continue;
      end;
      targetPlatform := TSpecTargetPlatform.Create(Logger);
      FTargetPlatforms.Add(targetPlatform);
      result := targetPlatform.LoadFromYAML(targetPlatformObj) and result;
    end;
  end
  else
  begin
    Logger.Error(TErrorCode.E6003_SpecElementMissing, 'No targetPlatforms were found - at least one is required.');
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
    Logger.Error(TErrorCode.E6003_SpecElementMissing, 'Template #' + IntToStr(templateNo) + ' is missing the required [name].');
  end;
  template := TSpecTemplate.Create(Self.Logger);
  result := result and template.LoadFromYAML(templateObj);
  FTemplates.Add(template);

end;

function TSpec.LoadTemplatesFromYAML(const templatesSeq: IYAMLSequence): boolean;
var
  i : integer;
  templateObj : IYAMLMapping;
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
    begin
      if MappingAt(templatesSeq, i, '[templates]', templateObj) then
        result := LoadTemplateFromYAML(templateObj, i + 1) and result
      else
        result := false;
    end;
  end
  else
  begin
    result := false;
    Logger.Error(TErrorCode.E6003_SpecElementMissing, 'No templates were found - at least one is required.');
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
    //Variable names are case insensitive - FVariables compares them that way - but kept as written, so a load and save
    //(DSpecCreator) doesn't rewrite them. They used to be lowercased here.
    for i := 0 to variablesObj.Count -1 do
    begin
      try
        FVariables[variablesObj.Keys[i]] := variablesObj.Items[variablesObj.Keys[i]].AsString;
      except
        on e : Exception do
        begin
          result := false;
          Logger.Error(TErrorCode.E6004_SpecValueInvalid, 'The variable [' + variablesObj.Keys[i] + '] is not valid : ' + e.Message);
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
  existing : ISpecTemplate;
  targetPlatform : ISpecTargetPlatform;
begin
  currentTemplate := FindTemplate(currentTemplateName);
  if not Assigned(currentTemplate) then
    raise Exception.Create('Template not found');
  //two templates of one name - which one a platform means can't be told.
  existing := FindTemplate(NewTemplateName);
  if (existing <> nil) and (existing <> currentTemplate) then
    raise Exception.Create('There is already a template named [' + NewTemplateName + ']');

  currentTemplate.Name := NewTemplateName;
  //and the platforms that name it - they went on naming one that wasn't there.
  for targetPlatform in FTargetPlatforms do
    if SameText(targetPlatform.TemplateName, currentTemplateName) then
      targetPlatform.TemplateName := NewTemplateName;
end;


function TSpec.GetMinClientVersion : string;
begin
  result := FMinClientVersion;
end;

procedure TSpec.SetMinClientVersion(const value : string);
begin
  FMinClientVersion := value;
end;

procedure TSpec.SetPackageKind(const value: TDPMPackageKind);
begin
  FPackageKind := value;
  FMetaData.PackageKind := value;
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
    //the writer adds the leading '# ' marker itself - including it here produces '# # ...' and the
    //extra '#' then accumulates on every load/save round-trip.
    root.Comments.Add('DPM package spec file');

  if FMinClientVersion <> '' then
    root.S['min dpm client version'] := FMinClientVersion;
  if packageKind <> TDPMPackageKind.dpm then
    root.S['packageKind'] := PackageKindToString(FPackageKind);

  FMetaData.ToYAML(root, packageKind);

  //variables come straight after metadata (insertion order is preserved in the output).
  if FVariables.Count > 0 then
  begin
    variables := root.O['variables'];
    if FVariablesComments <> nil then
      variables.Comments.Assign(FVariablesComments);
    //write the actual variable values - pack never reaches here with variables (it clears them
    //after resolving tokens), so this only affects the DSpecCreator save/preview path.
    for i := 0 to FVariables.Count - 1 do
      variables.S[FVariables.Items[i].Key] := FVariables.Items[i].Value;
  end;

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

end;

procedure TSpec.ToYAMLFile(const fileName: string);
var
  doc : IYAMLDocument;
  root : IYAMLMapping;

begin
  FTargetPlatforms.Sort(TComparer<ISpecTargetPlatform>.Construct(
  function(const Left, Right: ISpecTargetPlatform): Integer
   begin
      //sort by the lowest compiler each entry covers - using Compiler alone would sort range and
      //list entries (whose Compiler is UnknownVersion) to the front.
      result := Ord(MinCompilerOf(Left)) - Ord(MinCompilerOf(Right));
   end));

  doc := TYAML.CreateMapping;
  root := doc.AsMapping;
  ToYAML(doc.AsMapping, FPackageKind);
  TYAML.WriteToFile(doc, fileName);
end;

end.

