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

unit DPM.Core.Spec.Reader;

interface

uses
  System.Classes,
  System.SysUtils,
  JsonDataObjects,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces;

type
  TPackageSpecReader = class(TInterfacedObject, IPackageSpecReader)
  private
    FLogger : ILogger;
    function InternalReadPackageSpecJson(const fileName : string; const jsonObject : TJsonObject) : IPackageSpec;
    function InternalReadPackageSpecYAML(const fileName : string; const yamlDoc : IYAMLDocument) : IPackageSpec;

  protected
    function ReadSpec(const fileName : string) : IPackageSpec;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  Winapi.ActiveX,
  DPM.Core.Spec;

{ TSpecReader }

function TPackageSpecReader.ReadSpec(const fileName : string) : IPackageSpec;
var
  jsonObj : TJsonObject;
  yamlDoc : IYAMLDocument;
  ext : string;
begin
  result := nil;
  if not FileExists(fileName) then
  begin
    FLogger.Error('Spec file : [' + filename + '] does not exist');
    exit;
  end;

  ext := ExtractFileExt(fileName);
  if SameText(ext, '.yaml') then
  begin
    try
      yamlDoc := TYAML.LoadFromFile(fileName);
      result := InternalReadPackageSpecYAML(fileName, yamlDoc);
    except
      on e : Exception do
      begin
        FLogger.Error('Error parsing spec yaml : ' + e.Message);
      end;

    end;

  end
  else
  begin
    try
      jsonObj := TJsonObject.ParseFromFile(fileName) as TJsonObject;
      try
        Result := InternalReadPackageSpecJson(fileName, jsonObj);
      finally
        jsonObj.Free;
      end;
    except
      on e : Exception do
      begin
        FLogger.Error('Error parsing spec json : ' + e.Message);
        exit;
      end;
    end;
  end;
end;

constructor TPackageSpecReader.Create(const logger : ILogger);
begin
  FLogger := logger;
end;

function TPackageSpecReader.InternalReadPackageSpecJson(const fileName : string; const jsonObject : TJsonObject) : IPackageSpec;
begin
  result := nil;
  if not jsonObject.Contains('metadata') then
  begin
    FLogger.Error('json document does not have a metadata object, this is probably not a dspec file');
    exit;
  end;
  result := TSpec.Create(FLogger, fileName);
  result.LoadFromJson(jsonObject)

end;


function TPackageSpecReader.InternalReadPackageSpecYAML(const fileName: string; const yamlDoc: IYAMLDocument): IPackageSpec;
var
  root : IYAMLMapping;
begin
  result := nil;
  root := yamlDoc.AsMapping;
  if not root.ContainsKey('metadata') then
  begin
    FLogger.Error('yaml document does not have a metadata object, this is probably not a dspec file');
    exit;
  end;
  result := TSpec.Create(FLogger, fileName);
  result.LoadFromYAML(root);




end;

end.

