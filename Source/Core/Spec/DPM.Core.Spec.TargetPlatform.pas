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
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.TemplateBase,
  DPM.Core.Spec.Interfaces;


//NOTE: TSpecTargetPlatform descends from TSpecTemplateBase because we need this when we apply the templates during pack.
// and also when we load the package spec in the IDE for dependencies.
type
  TSpecTargetPlatform = class(TSpecTemplateBase, ISpecTargetPlatform)
  private
    FTemplateName : string;
    FPlatforms : TArray<TDPMPlatform>;
    FCompiler : TCompilerVersion;
    FVariables : TStringList;

  protected
    function GetPlatforms : TArray<TDPMPlatform>;
    procedure SetPlatforms(platforms: TArray<TDPMPlatform>);
    function GetTemplateName : string;
    procedure SetTemplateName(name: string);
    function GetCompiler : TCompilerVersion;
    procedure SetCompiler(compiler: TCompilerVersion);
    function GetVariables : TStrings;

    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function CloneForPlatform(const platform : TDPMPlatform) : ISpecTargetPlatform;
    function ToJSON: string; override;
  public
    constructor Create(const logger : ILogger); override;
    constructor CreateReducedClone(const logger : ILogger; const targetPlatform : ISpecTargetPlatform; const platform : TDPMPlatform; const variables : TStrings);
    destructor Destroy;override;
    function ToString : string;override;
    function PlatformContains(platformName:string): Boolean;
  end;


implementation

uses
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Constants,
  DPM.Core.TargetPlatform,
  DPM.Core.Utils.Strings,
  DPM.Core.Spec.Dependency,
  DPM.Core.Spec.DependencyGroup;


{ TSpecTargetPlatform }

function TSpecTargetPlatform.CloneForPlatform(const platform : TDPMPlatform) : ISpecTargetPlatform;
begin
  result := TSpecTargetPlatform.CreateReducedClone(logger, self, platform, FVariables);
end;

constructor TSpecTargetPlatform.Create(const logger : ILogger);
begin
  inherited Create(logger);
  SetLength(FPlatforms, 0);
  FCompiler := TCompilerVersion.UnknownVersion;
  FTemplateName := cUnset;
  FVariables := TStringList.Create;
end;


constructor TSpecTargetPlatform.CreateReducedClone(const logger : ILogger; const targetPlatform : ISpecTargetPlatform; const platform : TDPMPlatform; const variables : TStrings);
var
  deps : IList<ISpecDependency>;
  dep, newDep : ISpecDependency;
  designFiles, runtimeFiles : IList<ISpecBPLEntry>;
  bpl, newBpl : ISpecBPLEntry;
  sourceFiles, libFiles, otherFiles : IList<ISpecFileEntry>;
  fileEntry, newFileEntry : ISpecFileEntry;
  searchPaths : IList<ISpecSearchPath>;
  path, newPath : ISpecSearchPath;
begin
  deps := TCollections.CreateList<ISpecDependency>;
  for dep in targetPlatform.Dependencies do
  begin
    newDep := dep.Clone;
    deps.Add(newDep);
  end;

  designFiles := TCollections.CreateList<ISpecBPLEntry>;
  for bpl in targetPlatform.DesignFiles do
  begin
    newBpl := bpl.Clone;
    designFiles.Add(newBpl)
  end;

  runtimeFiles := TCollections.CreateList<ISpecBPLEntry>;
  for bpl in targetPlatform.RuntimeFiles do
  begin
    newBpl := bpl.Clone;
    runtimeFiles.Add(newBpl)
  end;

  sourceFiles := TCollections.CreateList<ISpecFileEntry>;
  for fileEntry in targetPlatform.SourceFiles do
  begin
    newFileEntry := fileEntry.Clone;
    sourceFiles.Add(newFileEntry)
  end;

  libFiles := TCollections.CreateList<ISpecFileEntry>;
  for fileEntry in targetPlatform.LibFiles do
  begin
    newFileEntry := fileEntry.Clone;
    libFiles.Add(newFileEntry)
  end;

  otherFiles := TCollections.CreateList<ISpecFileEntry>;
  for fileEntry in targetPlatform.Files do
  begin
    newFileEntry := fileEntry.Clone;
    otherFiles.Add(newFileEntry)
  end;

  searchPaths := TCollections.CreateList<ISpecSearchPath>;
  for path in targetPlatform.SearchPaths do
  begin
    newPath := path.Clone;
    searchPaths.Add(newPath);
  end;

  inherited CreateClone(logger, deps, designFiles, runtimeFiles, sourceFiles, libFiles, otherFiles, searchPaths);

  FVariables.Assign(variables);

  FTemplateName := targetPlatform.TemplateName;
  FCompiler := targetPlatform.Compiler;
  FPlatforms := TArray<TDPMPlatform>.Create(platform);
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

function TSpecTargetPlatform.PlatformContains(platformName: string): Boolean;
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

procedure TSpecTargetPlatform.SetPlatforms(platforms: TArray<TDPMPlatform>);
begin
  FPlatforms := platforms;
end;

procedure TSpecTargetPlatform.SetTemplateName(name: string);
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

end.

