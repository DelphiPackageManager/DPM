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


unit DPM.Core.Spec.TargetPlatform;

interface

uses
  System.Classes,
  Spring.Collections,
  JsonDataObjects,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Node,
  DPM.Core.Spec.Interfaces;


//NOTE: TSpecTargetPlatform descends from TSpecTemplateBase because we need this when we apply the templates during pack.
// and also when we load the package spec in the IDE for dependencies.
type
  TSpecTargetPlatform = class(TSpecNode, ISpecTargetPlatform)
  private
    FTemplateName : string;
    FPlatforms : TArray<TDPMPlatform>;

    // Single version
    FCompiler : TCompilerVersion;

    // various versions
    FCompilers : TArray<TCompilerVersion>;

    //a range of versions
    FMinCompilerVersion : TCompilerVersion;
    FMaxCompilerVersion : TCompilerVersion;


    FVariables : TStringList;
  protected

    function GetPlatforms : TArray<TDPMPlatform>;
    procedure SetPlatforms(const platforms: TArray<TDPMPlatform>);
    function GetTemplateName : string;
    procedure SetTemplateName(const name: string);

    function GetCompiler : TCompilerVersion;
    procedure SetCompiler(compiler: TCompilerVersion);
    function GetCompilers : TArray<TCompilerVersion>;
    procedure SetCompilers(compilers: TArray<TCompilerVersion>);

    function GetMinCompiler : TCompilerVersion;
    procedure SetMinCompiler(value : TCompilerVersion);
    function GetMaxCompiler : TCompilerVersion;
    procedure SetMaxCompiler(value : TCompilerVersion);


    function GetVariables : TStrings;

    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    function CloneForCompilerVersion(const compilerVersion : TCompilerVersion) : ISpecTargetPlatform;
    function CloneForPlatform(const platform : TDPMPlatform) : ISpecTargetPlatform;
    function ToJSON: string; override;
    procedure ToYAML(const parent : IYAMLValue; const packageKind : TDPMPackageKind);override;
  public
    constructor Create(const logger : ILogger); override;
    constructor CreateReducedClone(const logger : ILogger; const targetPlatform : ISpecTargetPlatform; const platform : TDPMPlatform; const variables : TStrings);
    destructor Destroy;override;
    function ToString : string;override;
    function PlatformContains(const platformName:string): Boolean;
  end;


implementation

uses
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Constants,
  DPM.Core.TargetPlatform,
  DPM.Core.Utils.Strings,
  DPM.Core.Spec.Dependency;


{ TSpecTargetPlatform }

function TSpecTargetPlatform.CloneForPlatform(const platform : TDPMPlatform) : ISpecTargetPlatform;
begin
  raise ENotImplemented.Create('Error Message');
end;


function TSpecTargetPlatform.CloneForCompilerVersion(const compilerVersion : TCompilerVersion) : ISpecTargetPlatform;
begin
  raise ENotImplemented.Create('Error Message');
end;


constructor TSpecTargetPlatform.Create(const logger : ILogger);
begin
  inherited Create(logger);
  SetLength(FPlatforms, 0);
  FCompiler := TCompilerVersion.UnknownVersion;
  FCompilers := [];
  FMinCompilerVersion := TCompilerVersion.UnknownVersion;
  FMaxCompilerVersion := TCompilerVersion.UnknownVersion;



  FTemplateName := cUnset;
  FVariables := TStringList.Create;
  FTemplateName := 'default';
end;


constructor TSpecTargetPlatform.CreateReducedClone(const logger : ILogger; const targetPlatform : ISpecTargetPlatform; const platform : TDPMPlatform; const variables : TStrings);
//var
//  deps : IList<ISpecDependency>;
//  dep, newDep : ISpecDependency;
//  designFiles, runtimeFiles : IList<ISpecBPLEntry>;
//  bpl, newBpl : ISpecBPLEntry;
//  sourceFiles, libFiles, otherFiles : IList<ISpecFileEntry>;
//  fileEntry, newFileEntry : ISpecFileEntry;
//  searchPaths : IList<ISpecSearchPath>;
//  path, newPath : ISpecSearchPath;
begin
//  deps := TCollections.CreateList<ISpecDependency>;
//  for dep in targetPlatform.Dependencies do
//  begin
//    newDep := dep.Clone;
//    deps.Add(newDep);
//  end;
//
//  designFiles := TCollections.CreateList<ISpecBPLEntry>;
//  for bpl in targetPlatform.DesignFiles do
//  begin
//    newBpl := bpl.Clone;
//    designFiles.Add(newBpl)
//  end;
//
//  runtimeFiles := TCollections.CreateList<ISpecBPLEntry>;
//  for bpl in targetPlatform.RuntimeFiles do
//  begin
//    newBpl := bpl.Clone;
//    runtimeFiles.Add(newBpl)
//  end;
//
//  sourceFiles := TCollections.CreateList<ISpecFileEntry>;
//  for fileEntry in targetPlatform.SourceFiles do
//  begin
//    newFileEntry := fileEntry.Clone;
//    sourceFiles.Add(newFileEntry)
//  end;
//
//  libFiles := TCollections.CreateList<ISpecFileEntry>;
//  for fileEntry in targetPlatform.LibFiles do
//  begin
//    newFileEntry := fileEntry.Clone;
//    libFiles.Add(newFileEntry)
//  end;
//
//  otherFiles := TCollections.CreateList<ISpecFileEntry>;
//  for fileEntry in targetPlatform.Files do
//  begin
//    newFileEntry := fileEntry.Clone;
//    otherFiles.Add(newFileEntry)
//  end;
//
//  searchPaths := TCollections.CreateList<ISpecSearchPath>;
//  for path in targetPlatform.SearchPaths do
//  begin
//    newPath := path.Clone;
//    searchPaths.Add(newPath);
//  end;
//
//  inherited CreateClone(logger, deps, designFiles, runtimeFiles, sourceFiles, libFiles, otherFiles, searchPaths);
//
//  FVariables.Assign(variables);
//
//  FTemplateName := targetPlatform.TemplateName;
//  FCompiler := targetPlatform.Compiler;
//  FPlatforms := TArray<TDPMPlatform>.Create(platform);
end;

destructor TSpecTargetPlatform.Destroy;
begin
  FVariables.Free;
  inherited;
end;

function TSpecTargetPlatform.GetPlatforms : TArray<TDPMPlatform>;
begin
  result := FPlatforms;
end;

function TSpecTargetPlatform.GetCompiler : TCompilerVersion;
begin
  result := FCompiler;
end;


function TSpecTargetPlatform.GetCompilers: TArray<TCompilerVersion>;
begin
  result := FCompilers;
end;

function TSpecTargetPlatform.GetMaxCompiler: TCompilerVersion;
begin
  result := FMaxCompilerVersion;
end;

function TSpecTargetPlatform.GetMinCompiler: TCompilerVersion;
begin
  result := FMinCompilerVersion;
end;

function TSpecTargetPlatform.GetTemplateName : string;
begin
  result := FTemplateName;
end;

function TSpecTargetPlatform.GetVariables: TStrings;
begin
  result := FVariables;
end;

function TSpecTargetPlatform.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  sValue : string;
  platformStrings : TArray<string>;
  platform : TDPMPlatform;
  platformList : IList<TDPMPlatform>;
  sCompiler : string;

  variablesObj : TJsonObject;
  i: Integer;

begin
  result := true;
  sCompiler := jsonObject.S['compiler'];
  if sCompiler = '' then
  begin
    Logger.Error('Required property [compiler] is missing)');
    result := false;
  end
  else
  begin
    FCompiler := StringToCompilerVersion(sCompiler);
    if FCompiler = TCompilerVersion.UnknownVersion then
    begin
      result := false;
      Logger.Error('Invalid compiler value [' + sCompiler + ']');
    end;
  end;

  sValue := jsonObject.S['platforms'];
  if sValue = '' then
  begin
    Logger.Error('Required property [platforms] is missing)');
    result := false;
  end
  else
  begin
    sValue := StringReplace(sValue, ' ', '', [rfReplaceAll]);
    //Logger.Debug('[targetPlatform] platforms : ' + sValue);
    platformStrings := TStringUtils.SplitStr(sValue, ',');

    if Length(platformStrings) > 0 then
    begin
      platformList := TCollections.CreateList<TDPMPlatform>;
      for sValue in platformStrings do
      begin
        platform := StringToDPMPlatform(sValue);
        if platform <> TDPMPlatform.UnknownPlatform then
        begin
          platformList.Add(platform);
          if not ValidatePlatform(FCompiler, platform) then
          begin
            Logger.Error('Invalid platform value [' + sValue + '] for compiler version [' + sCompiler + ']');
            result := false;
          end;
        end
        else
        begin
          Logger.Error('Invalid platform value [' + sValue + ']');
          result := false;
        end;
      end;
      FPlatforms := platformList.ToArray();
    end
    else
    begin
      Logger.Error('At least 1 platform must be specified.');
      result := false;
    end;
  end;
  FTemplateName := jsonObject.S['template'];
  variablesObj := jsonObject.ExtractObject('variables');
  if variablesObj <> nil then
  begin
    for i := 0 to variablesObj.Count -1 do
      FVariables.Add(variablesObj.Names[i] + '=' + variablesObj.Values[variablesObj.Names[i]].Value );
    variablesObj.Free;
  end;
  result := inherited LoadFromJson(jsonObject) and result;
end;

function TSpecTargetPlatform.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  sValue : string;
  platform : TDPMPlatform;
  platformList : IList<TDPMPlatform>;
  sCompiler : string;
  compiler : TCompilerVersion;
  sMinCompiler : string;
  sMaxCompiler : string;
  compilersSeq : IYAMLSequence;
  variablesObj : IYAMLMapping;
  i: Integer;
  platformsSeq : IYAMLSequence;
begin
  result := true;
  FCompilers := [];
  //order - compiler, from to, compilers
  sCompiler := yamlObject.S['compiler'];
  if sCompiler <> '' then
  begin
    FMinCompilerVersion := TCompilerVersion.UnknownVersion;
    FMaxCompilerVersion := TCompilerVersion.UnknownVersion;
    FCompiler := StringToCompilerVersion(sCompiler);
    if FCompiler = TCompilerVersion.UnknownVersion then
    begin
      result := false;
      Logger.Error('Invalid compiler value [' + sCompiler + ']');
    end;
  end
  else
  begin
    sMinCompiler := yamlObject.S['compiler from'];
    sMaxCompiler := yamlObject.S['compiler to'];
    if (sMinCompiler <> '') and (sMaxCompiler <> '') then
    begin
      FMinCompilerVersion := StringToCompilerVersion(sMinCompiler);
      if FMinCompilerVersion = TCompilerVersion.UnknownVersion then
      begin
        result := false;
        Logger.Error('Invalid compiler from value [' + sMinCompiler + ']');
      end;
      FMaxCompilerVersion := StringToCompilerVersion(sMaxCompiler);
      if FMaxCompilerVersion = TCompilerVersion.UnknownVersion then
      begin
        result := false;
        Logger.Error('Invalid compiler to value [' + sMaxCompiler + ']');
      end;
    end
    else
    begin
      compilersSeq := yamlObject.A['compilers'];
      if compilersSeq.Count > 0 then
      begin
        for i := 0 to compilersSeq.Count -1 do
        begin
          sCompiler := compilersSeq.S[i];
          compiler := StringToCompilerVersion(sCompiler);
          if compiler = TCompilerVersion.UnknownVersion then
          begin
            result := false;
            Logger.Error('Invalid compiler value [' + sCompiler + ']');
          end
          else
            AddToArray<TCompilerVersion>(FCompilers, compiler);
        end;
      end;
      if Length(FCompilers) = 0 then
      begin
        result := false;
        Logger.Error('No compiler versions supplied for targetFramework');
      end;
    end;
  end;

  platformsSeq := yamlObject.A['platforms'];
  if platformsSeq.Count > 0 then
  begin
    platformList := TCollections.CreateList<TDPMPlatform>;
    for i := 0 to platformsSeq.Count -1 do
    begin
      sValue := platformsSeq.S[i];
      platform := StringToDPMPlatform(sValue);
      if platform = TDPMPlatform.UnknownPlatform then
      begin
        result := false;
        Logger.Error('No compiler versions supplied for targetFramework');
      end
      else
        platformList.Add(platform);
    end;
    FPlatforms := platformList.Distinct.ToArray;
  end
  else
  begin
    Logger.Error('Required property [platforms] is missing)');
    result := false;
  end;
  FTemplateName := yamlObject.S['template'];
  if FTemplateName = '' then
    FTemplateName := 'default';

  if yamlObject.ContainsKey('variables') then
  begin
    variablesObj := yamlObject.O['variables'];
    if variablesObj <> nil then
    begin
      for i := 0 to variablesObj.Count -1 do
        //TODO : update vsoftyaml to make accessing values by index easier
        FVariables.Add(variablesObj.Keys[i] + '=' + variablesObj.Items[variablesObj.Keys[i]].AsString);
    end;
  end;

  result := inherited LoadFromYAML(yamlObject) and result;
end;

function TSpecTargetPlatform.PlatformContains(const platformName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FPlatforms) do
  begin
    if SameText(DPMPlatformToString(FPlatforms[i]), platformName) then
      Exit(True);
  end;
end;

procedure TSpecTargetPlatform.SetCompiler(compiler: TCompilerVersion);
begin
  FCompiler := compiler;
end;

procedure TSpecTargetPlatform.SetCompilers(compilers: TArray<TCompilerVersion>);
begin
  FCompilers := compilers;
end;

procedure TSpecTargetPlatform.SetMaxCompiler(value : TCompilerVersion);
begin
  FMaxCompilerVersion := value;
end;

procedure TSpecTargetPlatform.SetMinCompiler(value : TCompilerVersion);
begin
  FMinCompilerVersion := value;
end;

procedure TSpecTargetPlatform.SetPlatforms(const platforms: TArray<TDPMPlatform>);
begin
  FPlatforms := platforms;
end;

procedure TSpecTargetPlatform.SetTemplateName(const name: string);
begin
  FTemplateName := name;
end;

function TSpecTargetPlatform.ToJSON: string;
var
  json : TJSONObject;
  jsonVariables : TJSONObject;
  platformList : string;
  i: Integer;
  j: Integer;
begin
  json := TJSONObject.Create;
  try
    json.S['compiler'] := CompilerToString(FCompiler);
    platformList := '';
    for i := 0 to High(FPlatforms) do
    begin
      if platformList = '' then
        platformList := DPMPlatformToString(FPlatforms[i])
      else
        platformList := platformList + ', ' + DPMPlatformToString(FPlatforms[i]);
    end;
    json.S['platforms'] := platformList;
    json.S['template'] := FTemplateName;
    if FVariables.Count > 0 then
    begin
      jsonVariables := TJSONObject.Create;
      for j := 0 to FVariables.Count - 1 do
      begin
        jsonVariables.S[FVariables.Names[j]] := FVariables.ValueFromIndex[j];
      end;
      json.O['variables'] := jsonVariables;
    end;
    Result := json.ToJSON;
  finally
    FreeAndNil(json);
  end;
end;

function TSpecTargetPlatform.ToString: string;
begin
  result := CompilerToString(FCompiler);
end;

procedure TSpecTargetPlatform.ToYAML(const parent: IYAMLValue; const packageKind : TDPMPackageKind);
var
  mapping : IYAMLMapping;
  variables : IYAMLMapping;
  platforms : IYAMLSequence;
  sPlatform : string;
  i: Integer;
  j: Integer;
begin
  mapping := parent.AsSequence.AddMapping;
  mapping.S['compiler'] := CompilerToString(FCompiler);
  platforms := mapping.A['platforms'];

  for i := 0 to High(FPlatforms) do
  begin
    sPlatform := DPMPlatformToString(FPlatforms[i]);
    platforms.AddValue(sPlatform);
  end;

  if FTemplateName <> 'default' then
    mapping.S['template'] := FTemplateName;

  if FVariables.Count > 0 then
  begin
    variables := mapping.O['variables'];
    for j := 0 to FVariables.Count - 1 do
      variables.S[FVariables.Names[j]] := FVariables.ValueFromIndex[j];
  end;
end;

end.

