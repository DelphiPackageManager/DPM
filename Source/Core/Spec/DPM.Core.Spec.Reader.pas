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

unit DPM.Core.Spec.Reader;

interface

uses
  System.Classes,
  System.SysUtils,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces;

type
  TPackageSpecReader = class(TInterfacedObject, IPackageSpecReader)
  private
    FLogger : ILogger;
    function InternalReadPackageSpecYAML(const fileName : string; const yamlDoc : IYAMLDocument) : IPackageSpec;

  protected
    function ReadSpec(const fileName : string) : IPackageSpec;
    function ReadSpecString(const value : string; const fileName : string = '') : IPackageSpec;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  System.StrUtils,
  DPM.Core.Utils.Path,
  DPM.Core.Spec,
  DPM.Core.Diagnostics;

{ TSpecReader }

function TPackageSpecReader.ReadSpec(const fileName : string) : IPackageSpec;
var
  yamlDoc : IYAMLDocument;
begin
  result := nil;
  if not FileExists(fileName) then
  begin
    FLogger.Error(TErrorCode.E6001_SpecNotFound, 'The spec file [' + filename + '] does not exist.');
    exit;
  end;

  if TPathUtils.IsDspecFile(filename) then
  begin
    try
      yamlDoc := TYAML.LoadFromFile(fileName);
      result := InternalReadPackageSpecYAML(fileName, yamlDoc);
    except
      //a spec that failed to load may have been half built - don't hand it out.
      on e : EDPMError do
      begin
        result := nil;
        FLogger.Error(e.Code, 'Spec [' + fileName + '] : ' + e.Message);
      end;
      on e : Exception do
      begin
        result := nil;
        FLogger.Error(TErrorCode.E6002_SpecUnreadable, 'Unable to read the spec [' + fileName + '] : ' + e.Message);
      end;
    end;
  end
  else
    FLogger.Error(TErrorCode.E6002_SpecUnreadable, 'The spec file [' + filename + '] has an unrecognised extension (expected .dspec or .dspec.yaml).');
end;

function TPackageSpecReader.ReadSpecString(const value : string; const fileName : string = '') : IPackageSpec;
var
  yamlDoc : IYAMLDocument;
begin
  result := nil;
  try
    yamlDoc := TYAML.LoadFromString(value);
    result := InternalReadPackageSpecYAML(fileName, yamlDoc);
  except
    on e : EDPMError do
    begin
      result := nil;
      FLogger.Error(e.Code, e.Message);
    end;
    on e : Exception do
    begin
      result := nil;
      FLogger.Error(TErrorCode.E6002_SpecUnreadable, 'Unable to read the spec : ' + e.Message);
    end;
  end;
end;

constructor TPackageSpecReader.Create(const logger : ILogger);
begin
  FLogger := logger;
end;


function TPackageSpecReader.InternalReadPackageSpecYAML(const fileName: string; const yamlDoc: IYAMLDocument): IPackageSpec;
var
  root : IYAMLMapping;
  specLogger : ILogger;
begin
  result := nil;
  root := yamlDoc.AsMapping;
  if not root.ContainsKey('metadata') then
  begin
    FLogger.Error(TErrorCode.E6002_SpecUnreadable, 'The yaml document [' + fileName + '] has no metadata - it is probably not a dspec file.');
    exit;
  end;
  //the spec's parts don't know which file they came from - say so in what they report.
  if fileName <> '' then
    specLogger := TContextLogger.Create(FLogger, 'Spec [' + fileName + '] : ')
  else
    specLogger := FLogger;
  result := TSpec.Create(specLogger, fileName);
  result.LoadFromYAML(root);
end;

end.

