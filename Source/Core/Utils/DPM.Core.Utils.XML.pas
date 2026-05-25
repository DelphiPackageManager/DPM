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

unit DPM.Core.Utils.XML;

interface

uses
  DPM.Core.MSXML;

type
  TXMLUtils = class
    //we are using this to preserve the formatting of the dproj file.
    class procedure PrettyFormatXML(const node : IXMLDOMNode; const indentSize : integer);
    //Load via TFileStream/IPersistStreamInit so MSXML's URL parser never sees the
    //path. IXMLDOMDocument.load() treats its argument as a URL, which trips up on
    //some valid Windows paths (e.g. paths with spaces inside AppData).
    class function LoadXMLFromFile(const xmlDoc : IXMLDOMDocument; const filename : string) : boolean;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  WinApi.ActiveX;

procedure DoPrettyFormatXML(const node : IXMLDOMNode; const indentSize : integer; var indentLen : integer);
const
  CRLF = #13#10;
  TAB = #9;
var
  i : integer;
  newnode : IXMLDOMNode;
  childNode : IXMLDOMNode;
  siblingNode : IXMLDOMNode;
  stmp : string;
  sDebug : string;

  function SpaceString(stringLen : integer) : string;
  begin
    result := StringOfChar(' ', stringLen);
  end;

  function hasChildElements(const anode : IXMLDOMNode) : boolean;
  var
    j : integer;
    cnode : IXMLDOMNode;
  begin
    result := False;
    for j := 0 to anode.childNodes.length - 1 do
    begin
      cnode := anode.childNodes.item[j];
      if (cnode.nodeType = NODE_ELEMENT) or (cnode.nodeType = NODE_CDATA_SECTION) then
      begin
        result := True;
        break;
      end;
    end;

  end;

  function GetNextElement(const anode : IXMLDOMNode) : IXMLDOMNode;
  begin
    result := anode.nextSibling;
    while result <> nil do
    begin
      if result.nodeType = NODE_ELEMENT then
        break;
      result := result.nextSibling;
    end;
  end;

begin
  if node.nodeType = NODE_TEXT then
    exit;
  if node.childNodes.length > 0 then
  begin
    sDebug := node.nodeName;
    if hasChildElements(node) then
    begin
      Inc(indentLen, indentSize);
      i := 0;
      while i < node.childNodes.length do
      begin
        childNode := node.childNodes.item[i];
        sDebug := childNode.nodeName;
        case childNode.nodeType of //
          NODE_ELEMENT, NODE_CDATA_SECTION :
            begin
              stmp := CRLF + SpaceString(indentLen);
              newnode := node.ownerDocument.createTextNode(stmp);
              node.insertBefore(newnode, childNode);

              if hasChildElements(childNode) then
                DoPrettyFormatXML(childNode, indentSize, indentLen);

              siblingNode := GetNextElement(childNode);
              stmp := CRLF + SpaceString(indentLen);
              newnode := node.ownerDocument.createTextNode(stmp);
              if siblingNode <> nil then
                node.insertBefore(newnode, siblingNode);
              Inc(i);
            end;
          NODE_TEXT :
            begin
              // remove any old formatting nodes.
              node.removeChild(childNode);
              continue;
            end
        end;
        Inc(i);
      end;
      Dec(indentLen, indentSize);
      stmp := CRLF + SpaceString(indentLen);
      newnode := node.ownerDocument.createTextNode(stmp);
      node.appendChild(newnode);
    end;
  end;

end;

class procedure TXMLUtils.PrettyFormatXML(const node : IXMLDOMNode; const indentSize : integer);
var
  indentLen : integer;
begin
  indentLen := 0;
  DoPrettyFormatXML(node, indentSize, indentLen);
end;

class function TXMLUtils.LoadXMLFromFile(const xmlDoc : IXMLDOMDocument; const filename : string) : boolean;
var
  fs : TFileStream;
  stream : IStream;
  persist : IPersistStreamInit;
begin
  result := false;
  if xmlDoc = nil then
    exit;
  if not Supports(xmlDoc, IPersistStreamInit, persist) then
    exit;
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    stream := TStreamAdapter.Create(fs, soReference);
    result := Succeeded(persist.Load(stream));
  finally
    fs.Free;
  end;
end;

end.

