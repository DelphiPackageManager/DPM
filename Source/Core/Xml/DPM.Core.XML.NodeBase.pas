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

unit DPM.Core.XML.NodeBase;

//TODO : Not used since we switched to json.. remove when we're sure we don't need it

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.MSXML;

type
  IXMLNodeBase = interface
  ['{3716A814-0213-4845-80D2-2BD164E139D8}']
    function LoadFromXML(const xmlElement: IXMLDOMElement): boolean;
  end;


  TXMLNodeBase = class;

  TXMLNodeBaseClass = class of TXMLNodeBase;

  TXMLNodeBase = class(TInterfacedObject,IXMLNodeBase)
  private
    FLogger : ILogger;
  protected
    property Logger : ILogger read FLogger;
    function SafeGetAttribute(const xmlElement :IXMLDOMElement; const attributeName : string) : string;
    function ReadStringValue(const xmlElement : IXMLDOMElement; const nodeName : string; const required : boolean; var field : string) : boolean;
    function ReadBoolValue(const xmlElement : IXMLDOMElement; const nodeName : string; const required : boolean; var field : boolean) : boolean;
    function ReadBoolAttribute(const xmlElement : IXMLDOMElement; const attributeName : string; const required : boolean; var field : boolean) : boolean;
    function LoadFromXML(const xmlElement: IXMLDOMElement): Boolean;virtual;abstract;
    function LoadCollection(const rootElement: IXMLDOMElement; const collectionPath: string; const nodeClass: TXMLNodeBaseClass; const action: TConstProc<IInterface>): boolean;
  public
    constructor Create(const logger : ILogger);virtual;
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

constructor TXMLNodeBase.Create(const logger: ILogger);
begin
  FLogger := logger;
end;

function TXMLNodeBase.ReadBoolAttribute(const xmlElement: IXMLDOMElement; const attributeName: string; const required: boolean; var field: boolean): boolean;
var
  attributeValue : Variant;
  sValue : string;
begin
  result := false;
  attributeValue := xmlElement.getAttribute(attributeName);
  if VarIsNull(attributeValue) then
  begin
    if required  then
      logger.Error('Required attribute [' + attributeName + '] is empty.')
    else
      result := true;
    exit;
  end;
  sValue := attributeValue;
  try
    field := StrToBool(sValue);
    result := true;
  except
    logger.Error('Invalid  value [' + sValue + '] for ' + attributeName );
  end;

end;

function TXMLNodeBase.ReadBoolValue(const xmlElement: IXMLDOMElement; const nodeName: string; const required: boolean; var field: boolean): boolean;
var
  sValue : string;
begin
  result := ReadStringValue(xmlElement,nodeName,required,sValue);
  if result then
  begin
    if sValue = '' then
      exit;
      try
        field := StrToBool(sValue)
      except
        logger.Error('Invalid  value [' + sValue + '] for ' + nodeName );
        result := false;
      end;
  end;
end;

function TXMLNodeBase.ReadStringValue(const xmlElement : IXMLDOMElement; const nodeName: string; const required: boolean; var field: string): boolean;
var
  tmpNode : IXMLDOMElement;
begin
  result := false;
  tmpNode := xmlElement.selectSingleNode(nodeName) as IXMLDOMElement;
  if tmpNode = nil then
  begin
    if required  then
      logger.Error('Required Element [' + nodeName + '] not found.')
    else
      result := true;
    exit;
  end;
  field :=  tmpNode.text;
  if field = '' then
  begin
    if required  then
      logger.Error('Required Element [' + nodeName + '] is empty.');
    exit;
  end
  else
    result := true;
end;


function TXMLNodeBase.SafeGetAttribute(const xmlElement: IXMLDOMElement; const attributeName: string): string;
var
  attributeValue : Variant;
begin
  result := '';
  attributeValue := xmlElement.getAttribute(attributeName);
  if not VarIsNull(attributeValue) then
    result := attributeValue;
end;

function TXMLNodeBase.LoadCollection(const rootElement : IXMLDOMElement; const collectionPath: string; const nodeClass: TXMLNodeBaseClass; const action: TConstProc<IInterface>): boolean;
var
  element : IXMLDOMElement;
  nodeList : IXMLDOMNodeList;
  item : IXMLNodeBase;
  i: Integer;
begin
  result := true;
  nodeList := rootElement.selectNodes(collectionPath);
  if nodeList.length = 0 then
    exit;
  for i := 0 to nodeList.length -1 do
  begin
    element := nodeList.item[i] as IXMLDOMElement;
    item := nodeClass.Create(Logger) as IXMLNodeBase ;
    item.LoadFromXML(element);
    action(item);
  end;
end;


end.
