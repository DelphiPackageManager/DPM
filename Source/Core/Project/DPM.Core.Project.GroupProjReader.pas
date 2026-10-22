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

unit DPM.Core.Project.GroupProjReader;

interface

uses
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.MSXML,
  DPM.Core.Project.Interfaces;

type
  TGroupProjectReader = class(TInterfacedObject, IGroupProjectReader)
  private
    FLogger : ILogger;
    FXmlDoc : IXMLDOMDocument;
    FGroupProjFile : string;
  protected
    function ExtractProjects(const list : IList<string>) : boolean;
    function LoadGroupProj(const groupProjFile : string) : Boolean;
    procedure Close;
  public
    constructor Create(const logger : ILogger);
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Diagnostics,
  DPM.Core.Utils.XML;

const
  projectXPath = '/x:Project/x:ItemGroup/x:Projects';

  { TGroupProjectReader }

procedure TGroupProjectReader.Close;
begin
  FXmlDoc := nil;
end;

constructor TGroupProjectReader.Create(const logger : ILogger);
begin
  FLogger := logger;

end;

function TGroupProjectReader.ExtractProjects(const list : IList<string> ) : boolean;
var
  projectNodes : IXMLDOMNodeList;
  projectElement : IXMLDOMElement;
  i : integer;
begin
  result := false;
  list.Clear;
  if FXmlDoc = nil then
  begin
    FLogger.Error(TErrorCode.E0003_InternalError, 'ExtractProjects was called before a group project was loaded.');
    exit;
  end;

  projectNodes := FXmlDoc.selectNodes(projectXPath);
  if projectNodes.length = 0 then
  begin
    FLogger.Error(TErrorCode.E4002_NoProjectsFound, 'The group project [' + FGroupProjFile + '] contains no projects.');
    FXmlDoc := nil;
    exit;
  end;
  for i := 0 to projectNodes.length - 1 do
  begin
    projectElement := projectNodes.item[i] as IXMLDOMElement;
    if projectElement.getAttributeNode('Include') <> nil then
      list.Add(projectElement.getAttribute('Include'))
  end;

  if list.Count = 0 then
    FLogger.Error(TErrorCode.E4002_NoProjectsFound, 'The group project [' + FGroupProjFile + '] contains no projects.')
  else
    result := true;
end;

function TGroupProjectReader.LoadGroupProj(const groupProjFile : string) : Boolean;
begin
  result := true;
  FGroupProjFile := groupProjFile;

  FXmlDoc := CoDOMDocument60.Create;
  try
    if not TXMLUtils.LoadXMLFromFile(FXmlDoc, groupProjFile) then
    begin
      FLogger.Error(TErrorCode.E4003_ProjectLoadFailed, 'Unable to load the group project [' + groupProjFile + '] : ' + Trim(FXmlDoc.parseError.reason));
      FXmlDoc := nil;
      exit(false);
    end;
    (FXmlDoc as IXMLDOMDocument2).setProperty('SelectionLanguage', 'XPath');
    (FXmlDoc as IXMLDOMDocument2).setProperty('SelectionNamespaces', 'xmlns:x=''http://schemas.microsoft.com/developer/msbuild/2003''');
  except
    on e : Exception do
    begin
      FLogger.Error(TErrorCode.E4003_ProjectLoadFailed, 'Unable to load the group project [' + groupProjFile + '] : ' + e.Message);
      FXmlDoc := nil;
      result := false;
    end;
  end;
end;

end.

