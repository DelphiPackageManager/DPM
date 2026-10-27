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
  //The parts of a file's layout that the DOM does not hold - captured by LoadXMLFromFile so that
  //SaveXMLToFile can put them back.
  TXMLFileFormat = record
    HasBOM : boolean;
    TrailingNewLine : boolean;
    //the file breaks lines with a bare LF - MSXML writes every line break as CRLF.
    LFLineBreaks : boolean;
  end;

  TXMLUtils = class
    //we are using this to preserve the formatting of the dproj file.
    class procedure PrettyFormatXML(const node : IXMLDOMNode; const indentSize : integer);
    //Load via TFileStream/IPersistStreamInit so MSXML's URL parser never sees the
    //path. IXMLDOMDocument.load() treats its argument as a URL, which trips up on
    //some valid Windows paths (e.g. paths with spaces inside AppData).
    class function LoadXMLFromFile(const xmlDoc : IXMLDOMDocument; const filename : string) : boolean; overload;
    class function LoadXMLFromFile(const xmlDoc : IXMLDOMDocument; const filename : string; out fileFormat : TXMLFileFormat) : boolean; overload;
    //IXMLDOMDocument.save never writes a byte order mark, and writes ' and " in text as is where the
    //Delphi IDE writes &apos; and &quot;. This puts back the BOM, line breaks and trailing new line
    //the file was loaded with and escapes quotes in text the way the IDE does, so that a document
    //loaded with preserveWhiteSpace saves byte for byte apart from what was changed in the DOM.
    class procedure SaveXMLToFile(const xmlDoc : IXMLDOMDocument; const filename : string; const fileFormat : TXMLFileFormat);

    //Whitespace aware DOM edits, for documents loaded with preserveWhiteSpace. New elements are put
    //on their own line at the indent their siblings already use, and a removed element takes its
    //line with it, so the rest of the document keeps its layout. A parent with no line breaks
    //between its children (compact or mixed content) is left compact.
    class procedure AppendChildElement(const parent : IXMLDOMNode; const child : IXMLDOMNode);
    class procedure InsertChildElementAfter(const parent : IXMLDOMNode; const child : IXMLDOMNode; const refChild : IXMLDOMNode);
    class procedure RemoveChildElement(const parent : IXMLDOMNode; const child : IXMLDOMNode);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  WinApi.ActiveX,
  DPM.Core.Utils.Files;

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
  fileFormat : TXMLFileFormat;
begin
  result := LoadXMLFromFile(xmlDoc, filename, fileFormat);
end;

class function TXMLUtils.LoadXMLFromFile(const xmlDoc : IXMLDOMDocument; const filename : string; out fileFormat : TXMLFileFormat) : boolean;
var
  fs : TFileStream;
  stream : IStream;
  persist : IPersistStreamInit;
  head : TBytes;
  headSize : integer;
  i : integer;
  lastByte : byte;
begin
  result := false;
  fileFormat.HasBOM := false;
  fileFormat.TrailingNewLine := false;
  fileFormat.LFLineBreaks := false;
  if xmlDoc = nil then
    exit;
  if not Supports(xmlDoc, IPersistStreamInit, persist) then
    exit;
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    //the first line break decides the style - it is well inside the first 64K of any real dproj.
    headSize := 65536;
    if fs.Size < headSize then
      headSize := fs.Size;
    SetLength(head, headSize);
    if headSize > 0 then
      fs.ReadBuffer(head[0], headSize);
    fileFormat.HasBOM := (headSize >= 3) and (head[0] = $EF) and (head[1] = $BB) and (head[2] = $BF);
    for i := 0 to headSize - 1 do
    begin
      if head[i] = 10 then
      begin
        fileFormat.LFLineBreaks := (i = 0) or (head[i - 1] <> 13);
        break;
      end;
    end;
    if fs.Size > 0 then
    begin
      fs.Position := fs.Size - 1;
      fs.ReadBuffer(lastByte, 1);
      fileFormat.TrailingNewLine := lastByte = 10;
    end;
    fs.Position := 0;
    stream := TStreamAdapter.Create(fs, soReference);
    result := Succeeded(persist.Load(stream));
  finally
    fs.Free;
  end;
end;

//MSXML only escapes & < and > in text. The Delphi IDE also writes ' and " in text as &apos; and
//&quot;, so do the same - otherwise every save rewrites values like VerInfo_Keys. Attribute values
//are copied as is (MSXML already writes " there as &quot; and the IDE writes ' unescaped), as are
//CDATA sections, comments and processing instructions. This works on the encoded bytes, which is
//safe for UTF-8 and the single byte encodings - every byte it acts on is ASCII, and multi byte
//UTF-8 sequences never contain an ASCII byte.
procedure WriteEscapingTextQuotes(const data : PByte; const size : integer; const stream : TStream);
type
  TScanState = (InText, InTag, InAttribute, InComment, InCData, InInstruction, InDocType);
const
  cApos : array[0..5] of AnsiChar = '&apos;';
  cQuot : array[0..5] of AnsiChar = '&quot;';
var
  state : TScanState;
  i : integer;
  runStart : integer;
  quote : byte;
  docTypeDepth : integer;

  function Matches(const token : AnsiString) : boolean;
  var
    j : integer;
  begin
    result := false;
    if i + Length(token) > size then
      exit;
    for j := 1 to Length(token) do
    begin
      if data[i + j - 1] <> Ord(token[j]) then
        exit;
    end;
    result := true;
  end;

  procedure WriteEntity(const entity : array of AnsiChar);
  begin
    if i > runStart then
      stream.WriteBuffer(data[runStart], i - runStart);
    stream.WriteBuffer(entity[0], Length(entity));
    runStart := i + 1;
  end;

begin
  state := TScanState.InText;
  runStart := 0;
  quote := 0;
  docTypeDepth := 0;
  i := 0;
  while i < size do
  begin
    case state of
      TScanState.InText :
        begin
          if data[i] = Ord('<') then
          begin
            if Matches('<!--') then
            begin
              state := TScanState.InComment;
              Inc(i, 3);
            end
            else if Matches('<![CDATA[') then
            begin
              state := TScanState.InCData;
              Inc(i, 8);
            end
            else if Matches('<?') then
            begin
              state := TScanState.InInstruction;
              Inc(i, 1);
            end
            else if Matches('<!') then
            begin
              state := TScanState.InDocType;
              docTypeDepth := 0;
            end
            else
              state := TScanState.InTag;
          end
          else if data[i] = Ord('''') then
            WriteEntity(cApos)
          else if data[i] = Ord('"') then
            WriteEntity(cQuot);
        end;
      TScanState.InTag :
        begin
          if (data[i] = Ord('"')) or (data[i] = Ord('''')) then
          begin
            quote := data[i];
            state := TScanState.InAttribute;
          end
          else if data[i] = Ord('>') then
            state := TScanState.InText;
        end;
      TScanState.InAttribute :
        begin
          if data[i] = quote then
            state := TScanState.InTag;
        end;
      TScanState.InComment :
        begin
          if Matches('-->') then
          begin
            state := TScanState.InText;
            Inc(i, 2);
          end;
        end;
      TScanState.InCData :
        begin
          if Matches(']]>') then
          begin
            state := TScanState.InText;
            Inc(i, 2);
          end;
        end;
      TScanState.InInstruction :
        begin
          if Matches('?>') then
          begin
            state := TScanState.InText;
            Inc(i, 1);
          end;
        end;
      TScanState.InDocType :
        begin
          //an internal subset can hold '>' - only the one after the closing ']' ends the doctype.
          if data[i] = Ord('[') then
            Inc(docTypeDepth)
          else if data[i] = Ord(']') then
            Dec(docTypeDepth)
          else if (data[i] = Ord('>')) and (docTypeDepth <= 0) then
            state := TScanState.InText;
        end;
    end;
    Inc(i);
  end;
  if size > runStart then
    stream.WriteBuffer(data[runStart], size - runStart);
end;

//Turns every CRLF into LF, in place. Returns the new size.
function StripCRBeforeLF(const data : PByte; const size : integer) : integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to size - 1 do
  begin
    if (data[i] = 13) and (i + 1 < size) and (data[i + 1] = 10) then
      continue;
    data[result] := data[i];
    Inc(result);
  end;
end;

class procedure TXMLUtils.SaveXMLToFile(const xmlDoc : IXMLDOMDocument; const filename : string; const fileFormat : TXMLFileFormat);
const
  cUTF8BOM : array[0..2] of byte = ($EF, $BB, $BF);
  cCRLF : array[0..1] of byte = (13, 10);
var
  ms : TMemoryStream;
  fs : TMemoryStream;
  stream : IStream;
  destination : OleVariant;
  data : PByte;
  size : integer;
  isUTF16 : boolean;
  hasBOM : boolean;
  bytes : TBytes;
begin
  ms := TMemoryStream.Create;
  try
    //save to a stream rather than letting MSXML write the file, so the bytes can be fixed up first.
    //Unlike the xml property, this keeps the encoding attribute of an xml declaration.
    stream := TStreamAdapter.Create(ms, soReference);
    destination := stream as IUnknown;
    xmlDoc.save(destination);
    destination := Unassigned;
    stream := nil;

    data := ms.Memory;
    size := ms.Size;
    //a document that declares utf-16 comes back with its own BOM - leave those bytes alone.
    isUTF16 := (size >= 2) and (((data[0] = $FF) and (data[1] = $FE)) or ((data[0] = $FE) and (data[1] = $FF)));
    hasBOM := isUTF16 or ((size >= 3) and (data[0] = $EF) and (data[1] = $BB) and (data[2] = $BF));
    if fileFormat.LFLineBreaks and (not isUTF16) then
      size := StripCRBeforeLF(data, size);

    //built in memory, then written in one go - a failure part way (disk full, a crash) mustn't leave a
    //truncated project behind.
    fs := TMemoryStream.Create;
    try
      if fileFormat.HasBOM and (not hasBOM) then
        fs.WriteBuffer(cUTF8BOM, SizeOf(cUTF8BOM));
      if isUTF16 then
        fs.WriteBuffer(data^, size)
      else
      begin
        WriteEscapingTextQuotes(data, size, fs);
        //whitespace after the root element is not part of the DOM, so it never comes back from save.
        if fileFormat.TrailingNewLine and ((size = 0) or (data[size - 1] <> 10)) then
        begin
          if fileFormat.LFLineBreaks then
            fs.WriteBuffer(cCRLF[1], 1)
          else
            fs.WriteBuffer(cCRLF, SizeOf(cCRLF));
        end;
      end;
      SetLength(bytes, fs.Size);
      if fs.Size > 0 then
        Move(fs.Memory^, bytes[0], fs.Size);
      TFileUtils.AtomicWriteAllBytes(filename, bytes);
    finally
      fs.Free;
    end;
  finally
    ms.Free;
  end;
end;

const
  cLineBreak = #13#10;
  cDefaultIndentUnit = '    ';

function IsWhitespaceText(const node : IXMLDOMNode) : boolean;
var
  text : string;
  i : integer;
begin
  result := false;
  if (node = nil) or (node.nodeType <> NODE_TEXT) then
    exit;
  text := VarToStr(node.nodeValue);
  for i := 1 to Length(text) do
  begin
    if not CharInSet(text[i], [' ', #9, #13, #10]) then
      exit;
  end;
  result := true;
end;

//True when the node starts a line - the whitespace before it ends in a line break plus indent.
function TryGetLineIndent(const node : IXMLDOMNode; out indent : string) : boolean;
var
  previous : IXMLDOMNode;
  text : string;
  p : integer;
begin
  result := false;
  indent := '';
  previous := node.previousSibling;
  if not IsWhitespaceText(previous) then
    exit;
  text := VarToStr(previous.nodeValue);
  p := LastDelimiter(#13#10, text);
  if p = 0 then
    exit;
  indent := Copy(text, p + 1, MaxInt);
  result := true;
end;

//One level of indent, taken from the document element's first child so that a tab indented
//document stays tab indented.
function IndentUnit(const doc : IXMLDOMDocument) : string;
var
  child : IXMLDOMNode;
begin
  result := cDefaultIndentUnit;
  if doc.documentElement = nil then
    exit;
  child := doc.documentElement.firstChild;
  while child <> nil do
  begin
    if child.nodeType = NODE_ELEMENT then
    begin
      if TryGetLineIndent(child, result) and (result <> '') then
        exit;
      result := cDefaultIndentUnit;
      exit;
    end;
    child := child.nextSibling;
  end;
end;

function IndentOf(const node : IXMLDOMNode) : string;
var
  parent : IXMLDOMNode;
begin
  parent := node.parentNode;
  if (parent = nil) or (parent.nodeType = NODE_DOCUMENT) then
    result := ''
  else if not TryGetLineIndent(node, result) then
    result := IndentOf(parent) + IndentUnit(node.ownerDocument);
end;

//line up with the last element child that starts a line, else one level in from the parent.
function ChildIndent(const parent : IXMLDOMNode) : string;
var
  child : IXMLDOMNode;
begin
  child := parent.lastChild;
  while child <> nil do
  begin
    if (child.nodeType = NODE_ELEMENT) and TryGetLineIndent(child, result) then
      exit;
    child := child.previousSibling;
  end;
  result := IndentOf(parent) + IndentUnit(parent.ownerDocument);
end;

class procedure TXMLUtils.AppendChildElement(const parent : IXMLDOMNode; const child : IXMLDOMNode);
var
  doc : IXMLDOMDocument;
  lastNode : IXMLDOMNode;
  indent : string;
begin
  doc := parent.ownerDocument;
  lastNode := parent.lastChild;
  if lastNode = nil then
  begin
    //empty (usually an element we just added) - open it up across lines.
    indent := IndentOf(parent);
    parent.appendChild(doc.createTextNode(cLineBreak + indent + IndentUnit(doc)));
    parent.appendChild(child);
    parent.appendChild(doc.createTextNode(cLineBreak + indent));
  end
  else if IsWhitespaceText(lastNode) and (LastDelimiter(#13#10, VarToStr(lastNode.nodeValue)) > 0) then
  begin
    //the last node is the indent of the closing tag - the new child goes on its own line before it.
    indent := ChildIndent(parent);
    parent.insertBefore(doc.createTextNode(cLineBreak + indent), lastNode);
    parent.insertBefore(child, lastNode);
  end
  else
    parent.appendChild(child);
end;

class procedure TXMLUtils.InsertChildElementAfter(const parent : IXMLDOMNode; const child : IXMLDOMNode; const refChild : IXMLDOMNode);
var
  nextNode : IXMLDOMNode;
  indent : string;
  separator : IXMLDOMNode;
begin
  nextNode := refChild.nextSibling;
  separator := nil;
  //refChild's line break + indent goes in front of the new child; whatever followed refChild
  //(normally the whitespace before the next sibling) then follows the new child instead.
  if TryGetLineIndent(refChild, indent) then
    separator := parent.ownerDocument.createTextNode(cLineBreak + indent);
  if nextNode = nil then
  begin
    if separator <> nil then
      parent.appendChild(separator);
    parent.appendChild(child);
  end
  else
  begin
    if separator <> nil then
      parent.insertBefore(separator, nextNode);
    parent.insertBefore(child, nextNode);
  end;
end;

class procedure TXMLUtils.RemoveChildElement(const parent : IXMLDOMNode; const child : IXMLDOMNode);
var
  previous : IXMLDOMNode;
  node : IXMLDOMNode;
  onlyWhitespace : boolean;
begin
  previous := child.previousSibling;
  parent.removeChild(child);
  //the whitespace in front of the element is its line - leaving it would leave a blank line.
  if IsWhitespaceText(previous) then
    parent.removeChild(previous);

  //nothing but whitespace left - collapse to <parent/> rather than leave an empty, indented line.
  onlyWhitespace := true;
  node := parent.firstChild;
  while node <> nil do
  begin
    if not IsWhitespaceText(node) then
    begin
      onlyWhitespace := false;
      break;
    end;
    node := node.nextSibling;
  end;
  if onlyWhitespace then
  begin
    while parent.firstChild <> nil do
      parent.removeChild(parent.firstChild);
  end;
end;

end.

