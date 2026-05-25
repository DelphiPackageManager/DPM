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

unit DPM.Core.Project.Transformer;

interface

uses
  System.RegularExpressions,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.MSXML,
  DPM.Core.Logging;

type
  //Pluggable cleanup steps that the prepare command applies to each .dproj after the
  //built-in version-correction transforms. The first built-in step removes deployment
  //blocks; consumers can register more without subclassing.
  IProjectCleanupStep = interface
    ['{6F8B0AC9-7C19-44E1-A4F2-44E63D43F2A1}']
    function Name : string;
    procedure Apply(const projectXml : IXMLDOMDocument; const compiler : TCompilerVersion);
  end;

  //Mutating editor used by the prepare command. Distinct from TProjectEditor — that
  //class is restore-time and centred on DPM package references / search paths. The
  //transformer focuses on producing per-version-correct .dproj files at authoring time.
  IProjectTransformer = interface
    ['{4E29B0B2-0F92-4F31-9B6D-2C8B6F7E18E0}']
    function LoadFromFile(const filename : string) : boolean;
    function SaveToFile(const filename : string) : boolean;

    procedure SetProjectVersion(const compiler : TCompilerVersion);
    procedure SetLibSuffix(const compiler : TCompilerVersion);
    procedure SetDebugInformation(const compiler : TCompilerVersion);
    procedure SetDPMCompiler(const compiler : TCompilerVersion);
    procedure RemoveDeploymentBlocks;

    procedure RegisterCleanupStep(const step : IProjectCleanupStep);
    //runs all four setters then every registered cleanup step in registration order.
    procedure ApplyForCompiler(const compiler : TCompilerVersion);
  end;

  //Rewrites the {$LIBSUFFIX 'xxx'} directive (case-insensitive) inside a .dpk file.
  //All other lines are preserved verbatim. If no directive is present, the file is
  //copied through unchanged (the author may have deliberately omitted it).
  IDpkTransformer = interface
    ['{2A8E5BC4-3D7A-4D5A-9B3C-1F8D2E4A6B5C}']
    procedure RewriteLibSuffix(const inputFile, outputFile : string; const compiler : TCompilerVersion);
  end;

  TProjectTransformer = class(TInterfacedObject, IProjectTransformer)
  private
    FLogger : ILogger;
    FProjectXml : IXMLDOMDocument;
    FCleanupSteps : IList<IProjectCleanupStep>;
  protected
    function EnsureSinglePropertyGroupChild(const elementName : string) : IXMLDOMElement;
    function FindOrCreateChild(const parent : IXMLDOMElement; const elementName : string) : IXMLDOMElement;
    function SelectMatchingElements(const xpath : string) : IXMLDOMNodeList;
    procedure RemoveElement(const element : IXMLDOMElement);
  public
    constructor Create(const logger : ILogger);

    function LoadFromFile(const filename : string) : boolean;
    function SaveToFile(const filename : string) : boolean;

    procedure SetProjectVersion(const compiler : TCompilerVersion);
    procedure SetLibSuffix(const compiler : TCompilerVersion);
    procedure SetDebugInformation(const compiler : TCompilerVersion);
    procedure SetDPMCompiler(const compiler : TCompilerVersion);
    procedure RemoveDeploymentBlocks;

    procedure RegisterCleanupStep(const step : IProjectCleanupStep);
    procedure ApplyForCompiler(const compiler : TCompilerVersion);
  end;

  TDpkTransformer = class(TInterfacedObject, IDpkTransformer)
  private
    FLogger : ILogger;
    //holds the new suffix value while the regex evaluator is invoked - avoids
    //the XE2 overload-resolution issue with anonymous TMatchEvaluator functions.
    FRewriteSuffix : string;
  protected
    function LibSuffixMatchEvaluator(const match : TMatch) : string;
  public
    constructor Create(const logger : ILogger);
    procedure RewriteLibSuffix(const inputFile, outputFile : string; const compiler : TCompilerVersion);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  DPM.Core.Utils.XML;

const
  msbuildNamespace = 'http://schemas.microsoft.com/developer/msbuild/2003';
  projectVersionXPath = '/x:Project/x:PropertyGroup/x:ProjectVersion';
  dpmCompilerXPath = '/x:Project/x:PropertyGroup/x:DPMCompiler';
  debugInfoXPath = '//x:DCC_DebugInformation';
  //Update both elements - DCC_LibSuffix is the build-time suffix; DllSuffix tells
  //the IDE the BPL output filename has the suffix appended (used when loading the
  //package after compile). Both must match the target compiler.
  libSuffixXPath = '//x:DCC_LibSuffix | //x:DllSuffix';
  deploymentItemsXPath = '//x:DeployFile | //x:DeployClass | //x:Deployment | //x:ProjectRoot';
  rootPropertyGroupXPath = '/x:Project/x:PropertyGroup[not(@Condition)]';

{ TProjectTransformer }

constructor TProjectTransformer.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
  FCleanupSteps := TCollections.CreateList<IProjectCleanupStep>;
end;

function TProjectTransformer.LoadFromFile(const filename : string) : boolean;
begin
  result := false;
  FProjectXml := nil;

  if not FileExists(filename) then
  begin
    FLogger.Error('Project file does not exist : ' + filename);
    exit;
  end;

  FProjectXml := CoDOMDocument60.Create;
  try
    result := TXMLUtils.LoadXMLFromFile(FProjectXml, filename);
    if not result then
    begin
      FLogger.Error('Error loading project file : ' + FProjectXml.parseError.reason);
      exit;
    end;
    (FProjectXml as IXMLDOMDocument2).setProperty('SelectionLanguage', 'XPath');
    (FProjectXml as IXMLDOMDocument2).setProperty('SelectionNamespaces',
      'xmlns:x=''' + msbuildNamespace + '''');
  except
    on e : Exception do
    begin
      FLogger.Error('Error loading project xml : ' + e.Message);
      FProjectXml := nil;
      result := false;
    end;
  end;
end;

function TProjectTransformer.SaveToFile(const filename : string) : boolean;
begin
  result := false;
  if FProjectXml = nil then
  begin
    FLogger.Error('Cannot save project - nothing loaded.');
    exit;
  end;
  try
    TXMLUtils.PrettyFormatXML(FProjectXml.documentElement, 4);
    FProjectXml.save(filename);
    result := true;
  except
    on e : Exception do
    begin
      FLogger.Error('Error saving project [' + filename + '] : ' + e.Message);
    end;
  end;
end;

function TProjectTransformer.SelectMatchingElements(const xpath : string) : IXMLDOMNodeList;
begin
  result := FProjectXml.selectNodes(xpath);
end;

function TProjectTransformer.FindOrCreateChild(const parent : IXMLDOMElement; const elementName : string) : IXMLDOMElement;
var
  existing : IXMLDOMNode;
begin
  existing := parent.selectSingleNode('x:' + elementName);
  if existing <> nil then
  begin
    result := existing as IXMLDOMElement;
    exit;
  end;
  result := FProjectXml.createNode(NODE_ELEMENT, elementName, msbuildNamespace) as IXMLDOMElement;
  parent.appendChild(result);
end;

function TProjectTransformer.EnsureSinglePropertyGroupChild(const elementName : string) : IXMLDOMElement;
var
  propertyGroup : IXMLDOMElement;
  existing : IXMLDOMNode;
begin
  //find the existing element anywhere under any PropertyGroup; prefer the first match.
  existing := FProjectXml.selectSingleNode('/x:Project/x:PropertyGroup/x:' + elementName);
  if existing <> nil then
  begin
    result := existing as IXMLDOMElement;
    exit;
  end;

  //not present - find an unconditional PropertyGroup to host it, or create one.
  propertyGroup := FProjectXml.selectSingleNode(rootPropertyGroupXPath) as IXMLDOMElement;
  if propertyGroup = nil then
  begin
    propertyGroup := FProjectXml.createNode(NODE_ELEMENT, 'PropertyGroup', msbuildNamespace) as IXMLDOMElement;
    FProjectXml.documentElement.insertBefore(propertyGroup, FProjectXml.documentElement.firstChild);
  end;

  result := FProjectXml.createNode(NODE_ELEMENT, elementName, msbuildNamespace) as IXMLDOMElement;
  propertyGroup.appendChild(result);
end;

procedure TProjectTransformer.RemoveElement(const element : IXMLDOMElement);
begin
  if (element <> nil) and (element.parentNode <> nil) then
    element.parentNode.removeChild(element);
end;

procedure TProjectTransformer.SetProjectVersion(const compiler : TCompilerVersion);
var
  element : IXMLDOMElement;
begin
  element := EnsureSinglePropertyGroupChild('ProjectVersion');
  element.text := CompilerVersionToProjectVersion(compiler);
end;

procedure TProjectTransformer.SetLibSuffix(const compiler : TCompilerVersion);
var
  nodes : IXMLDOMNodeList;
  i : integer;
  element : IXMLDOMElement;
  suffix : string;
begin
  suffix := CompilerToDefaultLibSuffix(compiler);
  nodes := SelectMatchingElements(libSuffixXPath);
  if (nodes = nil) or (nodes.length = 0) then
  begin
    //no existing element - don't synthesize one; the author may have deliberately
    //omitted it (e.g. application projects). Only update where it's already declared.
    exit;
  end;
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    element.text := suffix;
  end;
end;

procedure TProjectTransformer.SetDebugInformation(const compiler : TCompilerVersion);
var
  nodes : IXMLDOMNodeList;
  i : integer;
  element : IXMLDOMElement;
  currentValue : string;
  newValue : string;
  usesInteger : boolean;
begin
  usesInteger := CompilerUsesIntegerDebugInformation(compiler);
  nodes := SelectMatchingElements(debugInfoXPath);
  if nodes = nil then
    exit;
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    currentValue := Trim(element.text);
    if usesInteger then
    begin
      //XE5+ : booleans need to become integers. Anything else (already an int) we leave.
      if SameText(currentValue, 'false') then
        newValue := '0'
      else if SameText(currentValue, 'true') then
        newValue := '2'
      else
        newValue := currentValue;
    end
    else
    begin
      //pre-XE5 : integers need to become booleans. Already a boolean? leave it.
      if currentValue = '0' then
        newValue := 'false'
      else if (currentValue = '1') or (currentValue = '2') then
        newValue := 'true'
      else
        newValue := currentValue;
    end;
    if newValue <> currentValue then
      element.text := newValue;
  end;
end;

procedure TProjectTransformer.SetDPMCompiler(const compiler : TCompilerVersion);
var
  element : IXMLDOMNode;
begin
  //we only update if the element exists - we don't synthesize it. Adding DPM state to a
  //dproj that doesn't have it would be misleading.
  element := FProjectXml.selectSingleNode(dpmCompilerXPath);
  if element = nil then
    exit;
  element.text := CompilerToString(compiler);
end;

procedure TProjectTransformer.RemoveDeploymentBlocks;
var
  nodes : IXMLDOMNodeList;
  i : integer;
  element : IXMLDOMElement;
  parent : IXMLDOMNode;
  emptyParents : IList<IXMLDOMNode>;
begin
  nodes := SelectMatchingElements(deploymentItemsXPath);
  if nodes = nil then
    exit;
  emptyParents := TCollections.CreateList<IXMLDOMNode>;
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    parent := element.parentNode;
    RemoveElement(element);
    if parent <> nil then
      emptyParents.Add(parent);
  end;
  //drop ItemGroup parents that are now empty after the deployment items were removed.
  for i := 0 to emptyParents.Count - 1 do
  begin
    parent := emptyParents[i];
    if (parent.nodeName = 'ItemGroup') and (parent.selectNodes('x:*').length = 0) then
    begin
      if parent.parentNode <> nil then
        parent.parentNode.removeChild(parent);
    end;
  end;
end;

procedure TProjectTransformer.RegisterCleanupStep(const step : IProjectCleanupStep);
begin
  if step <> nil then
    FCleanupSteps.Add(step);
end;

procedure TProjectTransformer.ApplyForCompiler(const compiler : TCompilerVersion);
var
  i : integer;
begin
  SetProjectVersion(compiler);
  SetDebugInformation(compiler);
  SetLibSuffix(compiler);
  SetDPMCompiler(compiler);
  RemoveDeploymentBlocks;
  for i := 0 to FCleanupSteps.Count - 1 do
    FCleanupSteps[i].Apply(FProjectXml, compiler);
end;

{ TDpkTransformer }

constructor TDpkTransformer.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

function TDpkTransformer.LibSuffixMatchEvaluator(const match : TMatch) : string;
begin
  //Rebuild the directive using the captured prefix and the new directive body
  //stashed in FRewriteSuffix by RewriteLibSuffix. The body already includes any
  //necessary quotes (or the bare AUTO keyword), so it's a literal drop-in.
  result := match.Groups[1].Value + FRewriteSuffix + match.Groups[3].Value;
end;

procedure TDpkTransformer.RewriteLibSuffix(const inputFile, outputFile : string; const compiler : TCompilerVersion);
var
  inputBytes : TBytes;
  contents : string;
  regex : TRegEx;
  rewritten : string;
  outStream : TFileStream;
  utf8 : TEncoding;
  outBytes : TBytes;
  preamble : TBytes;
  evaluator : TMatchEvaluator;
begin
  //Build the entire directive body (everything between '{$LIBSUFFIX ' and '}'):
  //  - 10.4+ : the bare keyword AUTO
  //  - older : a single-quoted digit suffix like '160'
  //The dpk directive syntax differs from the dproj's <DllSuffix>$(Auto)</DllSuffix>.
  if compiler >= TCompilerVersion.Delphi10_4 then
    FRewriteSuffix := 'AUTO'
  else
    FRewriteSuffix := '''' + CompilerToLibSuffix(compiler) + '''';

  //read raw bytes so we can preserve the file's existing encoding/line endings.
  inputBytes := TFile.ReadAllBytes(inputFile);
  utf8 := TEncoding.UTF8;
  contents := utf8.GetString(inputBytes);

  //Match both forms: {$LIBSUFFIX 'xxx'} (quoted, old) and {$LIBSUFFIX AUTO} (bare keyword).
  //Group 1 = prefix incl. trailing whitespace, Group 2 = the value (quoted or keyword),
  //Group 3 = closing brace. We discard Group 2 and substitute FRewriteSuffix instead.
  regex := TRegEx.Create('(\{\$LIBSUFFIX\s+)(''[^'']*''|"[^"]*"|\w+)(\})', [roIgnoreCase]);
  if not regex.IsMatch(contents) then
  begin
    //no directive present - copy bytes through unchanged.
    if not SameFileName(inputFile, outputFile) then
      TFile.WriteAllBytes(outputFile, inputBytes);
    exit;
  end;

  //use a match evaluator so the new suffix is treated as a literal — otherwise
  //replacement-string substitution would interpret `$(...)` sequences specially.
  //assigning an instance method to a TMatchEvaluator works around a Delphi
  //overload-resolution issue with inline anonymous functions (see TPackageWriter
  //for the same workaround).
  evaluator := LibSuffixMatchEvaluator;
  rewritten := regex.Replace(contents, evaluator);

  //preserve UTF-8 BOM if the original had one; otherwise write without BOM.
  preamble := utf8.GetPreamble;
  outStream := TFileStream.Create(outputFile, fmCreate);
  try
    if (Length(inputBytes) >= Length(preamble)) and (Length(preamble) > 0) and
       CompareMem(@inputBytes[0], @preamble[0], Length(preamble)) then
      outStream.WriteBuffer(preamble[0], Length(preamble));
    outBytes := utf8.GetBytes(rewritten);
    if Length(outBytes) > 0 then
      outStream.WriteBuffer(outBytes[0], Length(outBytes));
  finally
    outStream.Free;
  end;
end;

end.
