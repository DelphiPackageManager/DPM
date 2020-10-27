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
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.TemplateBase,
  DPM.Core.Spec.Interfaces;

type
  TSpecTargetPlatform = class(TSpecTemplateBase, ISpecTargetPlatform)
  private
    FTemplateName : string;
    FPlatforms : TArray<TDPMPlatform>;
    FCompiler : TCompilerVersion;

  protected
    function GetPlatforms : TArray<TDPMPlatform>;
    function GetTemplateName : string;
    function GetCompiler : TCompilerVersion;
    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function CloneForPlatform(const platform : TDPMPlatform) : ISpecTargetPlatform;
  public
    constructor Create(const logger : ILogger); override;
    constructor CreateReducedClone(const logger : ILogger; const targetPlatform : ISpecTargetPlatform; const platform : TDPMPlatform);

  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Constants,
  DPM.Core.TargetPlatform,
  DPM.Core.Utils.Strings,
  DPM.Core.Spec.Dependency,
  DPM.Core.Spec.DependencyGroup;


{ TSpecTargetPlatform }

function TSpecTargetPlatform.CloneForPlatform(const platform : TDPMPlatform) : ISpecTargetPlatform;
begin
  result := TSpecTargetPlatform.CreateReducedClone(logger, self, platform);
end;

constructor TSpecTargetPlatform.Create(const logger : ILogger);
begin
  inherited Create(logger);
  SetLength(FPlatforms, 0);
  FCompiler := TCompilerVersion.UnknownVersion;
  FTemplateName := cUnset;
end;


constructor TSpecTargetPlatform.CreateReducedClone(const logger : ILogger; const targetPlatform : ISpecTargetPlatform; const platform : TDPMPlatform);
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

  FTemplateName := targetPlatform.TemplateName;
  FCompiler := targetPlatform.Compiler;
  FPlatforms := TArray<TDPMPlatform>.Create(platform);
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

function TSpecTargetPlatform.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  sValue : string;
  platformStrings : TArray<string>;
  platform : TDPMPlatform;
  platformList : IList<TDPMPlatform>;
  sCompiler : string;
  sTemplate : string;
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
    Logger.Debug('[targetPlatform] platforms : ' + sValue);
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

  sTemplate := jsonObject.S['template'];
  if sTemplate <> '' then
    FTemplateName := sTemplate;

  result := inherited LoadFromJson(jsonObject) and result;
end;

end.

