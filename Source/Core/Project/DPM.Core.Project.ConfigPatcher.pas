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

unit DPM.Core.Project.ConfigPatcher;

// Install-time repair of a package's .dproj in the package cache. Before DPM hands a project
// to msbuild with /p:Config=X /p:Platform=Y, this makes sure the dproj actually declares that
// (platform, config) pair. A dspec can legitimately target platforms the package's own dproj
// was never configured for, and msbuild applies NONE of the project's settings when the config
// activator group is missing - which fails the compile in a way that is very hard to diagnose.
//
// Distinct from TProjectEditor (the consumer project - package references / search paths) and
// from TProjectTransformer (authoring time, `dpm prepare`, per-compiler correction).
//
// The canonical shapes written here mirror what the Delphi IDE emits, and what
// TPrepareTemplates.ChainStubFor / BuildPlatformsList render for a brand new package project.

interface

uses
  DPM.Core.Types,
  DPM.Core.MSXML,
  DPM.Core.Logging;

{$SCOPEDENUMS ON}

type
  //NotNeeded means the dproj already declared the pair and NOTHING was written - the file's
  //bytes and timestamp are untouched. Failed means we could not inspect or save it; the caller
  //should warn and attempt the build anyway, so this can only ever fix packages, never break
  //one that builds today.
  TProjectPatchResult = (NotNeeded, Patched, Failed);

  //UpdatePlatformList controls whether <Platform value="X">true</Platform> is written into
  //ProjectExtensions/BorlandProject/Platforms. msbuild ignores ProjectExtensions entirely, but
  //DPM reads that list (TProjectEditor.LoadProjectPlatforms) to decide which platforms a design
  //package supports - so design entries must NOT pass this option, or a Win32-only design
  //package would look like it supports every platform we ever built it for.
  TProjectPatchOption = (UpdatePlatformList);
  TProjectPatchOptions = set of TProjectPatchOption;

  IProjectConfigPatcher = interface
    ['{9C3A5E71-6D24-4B8F-9E0C-3F5A1D7B2C48}']
    //Ensures projectFile declares configuration (eg 'Release') and platform. Adds only what is
    //missing; never rewrites the file when nothing is missing.
    function EnsureBuildTarget(const projectFile : string; const platform : TDPMPlatform;
                               const configuration : string; const compiler : TCompilerVersion;
                               const options : TProjectPatchOptions) : TProjectPatchResult;
  end;

  TProjectConfigPatcher = class(TInterfacedObject, IProjectConfigPatcher)
  private
    FLogger : ILogger;
    FProjectXml : IXMLDOMDocument;
    FProjectName : string; //ExtractFileName - for log messages
    FCompiler : TCompilerVersion;
    //Set only when a write actually changed the DOM. EnsureBuildTarget skips the save (and the
    //whole document reflow PrettyFormatXML does) when this stays false.
    FModified : boolean;
  protected
    function LoadFromFile(const filename : string) : boolean;
    function SaveToFile(const filename : string) : boolean;

    //--- dom helpers
    function CreateElement(const elementName : string) : IXMLDOMElement;
    function AddChildText(const parent : IXMLDOMElement; const elementName : string; const value : string) : IXMLDOMElement;
    procedure InsertAfter(const parent : IXMLDOMNode; const newNode : IXMLDOMNode; const afterNode : IXMLDOMNode);
    procedure InsertActivatorGroup(const group : IXMLDOMElement; const afterNode : IXMLDOMNode);
    function ConditionOf(const element : IXMLDOMElement) : string;
    function AttributeOf(const element : IXMLDOMElement; const name : string) : string;
    function PropertyGroups : IXMLDOMNodeList;

    //--- inspection
    function FindConfigKey(const configName : string) : string;
    function NextCfgIndex : integer;
    function FindActivatorGroup(const configName : string; const key : string) : IXMLDOMElement;
    function FindChainGroup(const key : string; const platformName : string) : IXMLDOMElement;
    function FindSettingsGroup(const key : string) : IXMLDOMElement;
    function LastActivatorOrChainGroup : IXMLDOMElement;
    function LastChainGroupFor(const key : string) : IXMLDOMElement;
    function LastPropertyGroup : IXMLDOMElement;
    function UsesBasePlatformStubs : boolean;
    function HasPlatformConfiguration(const configKey : string; const platformName : string) : boolean;

    //--- mutation - each returns true when it changed something
    function EnsureBuildConfigurationItem(const configName : string; const key : string) : boolean;
    function EnsureConfigActivator(const configName : string; const key : string) : boolean;
    function EnsureConfigSettings(const configName : string; const key : string) : boolean;
    function EnsureChainGroup(const key : string; const platformName : string) : boolean;
    function EnsurePlatformListEntry(const platformName : string) : boolean;
    function EnsurePlatformNamespaces(const platform : TDPMPlatform; const platformName : string) : boolean;

    function EnsureBuildTarget(const projectFile : string; const platform : TDPMPlatform;
                               const configuration : string; const compiler : TCompilerVersion;
                               const options : TProjectPatchOptions) : TProjectPatchResult;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  DPM.Core.Utils.XML;

const
  msbuildNamespace = 'http://schemas.microsoft.com/developer/msbuild/2003';
  propertyGroupsXPath = '/x:Project/x:PropertyGroup';
  buildConfigsXPath = '/x:Project/x:ItemGroup/x:BuildConfiguration';
  platformsXPath = '/x:Project/x:ProjectExtensions/x:BorlandProject/x:Platforms';
  borlandProjectXPath = '/x:Project/x:ProjectExtensions/x:BorlandProject';

  //Windows platforms are the only ones we can meaningfully synthesise settings for - the
  //others need SDK configuration we have no way to invent.
  cWindowsPlatforms : TDPMPlatforms = [TDPMPlatform.Win32, TDPMPlatform.Win64,
                                       TDPMPlatform.Win64x, TDPMPlatform.WinARM64EC];

  //Matches what the IDE writes into the Base_<Platform> group. Win32 additionally gets Bde
  //(there is no 64 bit BDE), everything else is identical.
  cWin32Namespaces = 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;$(DCC_Namespace)';
  cWin64Namespaces = 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;$(DCC_Namespace)';

{ TProjectConfigPatcher }

constructor TProjectConfigPatcher.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

function TProjectConfigPatcher.LoadFromFile(const filename : string) : boolean;
begin
  result := false;
  FProjectXml := nil;

  if not FileExists(filename) then
  begin
    FLogger.Warning('Project file does not exist : ' + filename);
    exit;
  end;

  FProjectXml := CoDOMDocument60.Create;
  try
    result := TXMLUtils.LoadXMLFromFile(FProjectXml, filename);
    if not result then
    begin
      FLogger.Warning('Error loading project file [' + FProjectName + '] : ' + FProjectXml.parseError.reason);
      FProjectXml := nil;
      exit;
    end;
    (FProjectXml as IXMLDOMDocument2).setProperty('SelectionLanguage', 'XPath');
    (FProjectXml as IXMLDOMDocument2).setProperty('SelectionNamespaces',
      'xmlns:x=''' + msbuildNamespace + '''');
    //A dproj that isn't an msbuild project at all would give us a document we cannot reason
    //about - bail rather than start bolting property groups onto something unrelated.
    if (FProjectXml.documentElement = nil) or (not SameText(FProjectXml.documentElement.baseName, 'Project')) then
    begin
      FLogger.Warning('Project file [' + FProjectName + '] does not look like an msbuild project.');
      FProjectXml := nil;
      result := false;
    end;
  except
    on e : Exception do
    begin
      FLogger.Warning('Error loading project xml [' + FProjectName + '] : ' + e.Message);
      FProjectXml := nil;
      result := false;
    end;
  end;
end;

function TProjectConfigPatcher.SaveToFile(const filename : string) : boolean;
begin
  result := false;
  if FProjectXml = nil then
    exit;
  try
    TXMLUtils.PrettyFormatXML(FProjectXml.documentElement, 4);
    FProjectXml.save(filename);
    result := true;
  except
    on e : Exception do
      FLogger.Warning('Error saving project [' + FProjectName + '] : ' + e.Message);
  end;
end;

//createNode with the msbuild namespace, NOT createElement - createElement would emit xmlns=""
//on every new node and neither msbuild nor our own xpath queries would see the property.
function TProjectConfigPatcher.CreateElement(const elementName : string) : IXMLDOMElement;
begin
  result := FProjectXml.createNode(NODE_ELEMENT, elementName, msbuildNamespace) as IXMLDOMElement;
end;

function TProjectConfigPatcher.AddChildText(const parent : IXMLDOMElement; const elementName : string; const value : string) : IXMLDOMElement;
begin
  result := CreateElement(elementName);
  result.text := value;
  parent.appendChild(result);
end;

//insertBefore's refChild is an OleVariant - passing a nil interface through it is not safe,
//so branch to appendChild when there is no following sibling.
procedure TProjectConfigPatcher.InsertAfter(const parent : IXMLDOMNode; const newNode : IXMLDOMNode; const afterNode : IXMLDOMNode);
var
  nextNode : IXMLDOMNode;
begin
  if afterNode = nil then
  begin
    parent.appendChild(newNode);
    exit;
  end;
  nextNode := afterNode.nextSibling;
  if nextNode = nil then
    parent.appendChild(newNode)
  else
    parent.insertBefore(newNode, nextNode);
end;

//Activator and chain groups MUST sit above the first settings group - msbuild evaluates
//PropertyGroups top down, so an activator placed after "'$(Base)'!=''" would leave Base empty
//when that group is evaluated and none of the output dirs / namespaces / GenPackage apply.
procedure TProjectConfigPatcher.InsertActivatorGroup(const group : IXMLDOMElement; const afterNode : IXMLDOMNode);
var
  reference : IXMLDOMNode;
begin
  reference := afterNode;
  if reference = nil then
    reference := LastActivatorOrChainGroup;
  if reference = nil then
  begin
    //no activators at all - go in front of everything rather than after the Import of
    //CodeGear.Delphi.Targets at the end of the document.
    if FProjectXml.documentElement.firstChild <> nil then
      FProjectXml.documentElement.insertBefore(group, FProjectXml.documentElement.firstChild)
    else
      FProjectXml.documentElement.appendChild(group);
    exit;
  end;
  InsertAfter(FProjectXml.documentElement, group, reference);
end;

function TProjectConfigPatcher.AttributeOf(const element : IXMLDOMElement; const name : string) : string;
var
  value : OleVariant;
begin
  result := '';
  if element = nil then
    exit;
  value := element.getAttribute(name);
  //getAttribute returns Null when the attribute is absent; VarToStr maps that to ''.
  if not VarIsNull(value) then
    result := VarToStr(value);
end;

function TProjectConfigPatcher.ConditionOf(const element : IXMLDOMElement) : string;
begin
  result := AttributeOf(element, 'Condition');
end;

function TProjectConfigPatcher.PropertyGroups : IXMLDOMNodeList;
begin
  result := FProjectXml.selectNodes(propertyGroupsXPath);
end;

function TProjectConfigPatcher.FindConfigKey(const configName : string) : string;
var
  nodes : IXMLDOMNodeList;
  element : IXMLDOMElement;
  keyElement : IXMLDOMElement;
  i : integer;
begin
  result := '';
  nodes := FProjectXml.selectNodes(buildConfigsXPath);
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    if not SameText(AttributeOf(element, 'Include'), configName) then
      continue;
    keyElement := element.selectSingleNode('x:Key') as IXMLDOMElement;
    if keyElement <> nil then
      result := Trim(keyElement.text);
    exit;
  end;
end;

//Allocate the next free Cfg_<n>. Scan BOTH the BuildConfiguration keys and every PropertyGroup
//condition - a dproj can carry an orphan Cfg_4 group whose ItemGroup entry was deleted, and
//reusing that number would silently merge two configs.
function TProjectConfigPatcher.NextCfgIndex : integer;
var
  highest : integer;
  nodes : IXMLDOMNodeList;
  i : integer;

  procedure ScanText(const text : string);
  var
    rest : string;
    p : integer;
    cursor : integer;
    n : integer;
    digitCount : integer;
  begin
    rest := text;
    p := Pos('Cfg_', rest);
    while p > 0 do
    begin
      cursor := p + 4;
      n := 0;
      digitCount := 0;
      while (cursor <= Length(rest)) and CharInSet(rest[cursor], ['0'..'9']) do
      begin
        n := (n * 10) + (Ord(rest[cursor]) - Ord('0'));
        Inc(cursor);
        Inc(digitCount);
      end;
      if (digitCount > 0) and (n > highest) then
        highest := n;
      rest := Copy(rest, cursor, MaxInt);
      p := Pos('Cfg_', rest);
    end;
  end;

begin
  highest := 0;
  nodes := FProjectXml.selectNodes(buildConfigsXPath + '/x:Key');
  for i := 0 to nodes.length - 1 do
    ScanText(nodes.item[i].text);
  nodes := PropertyGroups;
  for i := 0 to nodes.length - 1 do
    ScanText(ConditionOf(nodes.item[i] as IXMLDOMElement));
  result := highest + 1;
end;

//The activator is the group that makes /p:Config=<name> select anything. It is identified by a
//condition that tests $(Config) against the config name - or, for a dproj that only keys off
//the Cfg_n marker, by a $(Config) test combined with this key.
function TProjectConfigPatcher.FindActivatorGroup(const configName : string; const key : string) : IXMLDOMElement;
var
  nodes : IXMLDOMNodeList;
  element : IXMLDOMElement;
  condition : string;
  i : integer;
begin
  result := nil;
  nodes := PropertyGroups;
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    condition := ConditionOf(element);
    if condition = '' then
      continue;
    if Pos('''$(Config)''==''' + configName + '''', condition) > 0 then
      exit(element);
    if (key <> '') and (Pos('''$(Config)''==', condition) > 0) and
       (Pos('''$(' + key + ')''!=''''', condition) > 0) then
      exit(element);
  end;
end;

//A chain stub tests both the platform and the child marker - that is what distinguishes it from
//a settings group, which tests the child marker alone.
function TProjectConfigPatcher.FindChainGroup(const key : string; const platformName : string) : IXMLDOMElement;
var
  nodes : IXMLDOMNodeList;
  element : IXMLDOMElement;
  condition : string;
  i : integer;
begin
  result := nil;
  nodes := PropertyGroups;
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    condition := ConditionOf(element);
    if condition = '' then
      continue;
    if (Pos('''$(Platform)''==''' + platformName + '''', condition) > 0) and
       (Pos('''$(' + key + '_' + platformName + ')''!=''''', condition) > 0) then
      exit(element);
  end;
end;

function TProjectConfigPatcher.FindSettingsGroup(const key : string) : IXMLDOMElement;
var
  nodes : IXMLDOMNodeList;
  element : IXMLDOMElement;
  i : integer;
begin
  result := nil;
  nodes := PropertyGroups;
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    if Trim(ConditionOf(element)) = '''$(' + key + ')''!=''''' then
      exit(element);
  end;
end;

function TProjectConfigPatcher.LastActivatorOrChainGroup : IXMLDOMElement;
var
  nodes : IXMLDOMNodeList;
  element : IXMLDOMElement;
  condition : string;
  i : integer;
  lastUnconditional : IXMLDOMElement;
begin
  result := nil;
  lastUnconditional := nil;
  nodes := PropertyGroups;
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    condition := ConditionOf(element);
    if condition = '' then
    begin
      //the root property group - only used as a fallback anchor when there are no activators.
      lastUnconditional := element;
      continue;
    end;
    if (Pos('''$(Config)''==', condition) > 0) or (Pos('''$(Platform)''==', condition) > 0) then
      result := element;
  end;
  if result = nil then
    result := lastUnconditional;
end;

function TProjectConfigPatcher.LastChainGroupFor(const key : string) : IXMLDOMElement;
var
  nodes : IXMLDOMNodeList;
  element : IXMLDOMElement;
  condition : string;
  i : integer;
begin
  result := nil;
  nodes := PropertyGroups;
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    condition := ConditionOf(element);
    if condition = '' then
      continue;
    if (Pos('''$(Platform)''==', condition) > 0) and (Pos('''$(' + key + '_', condition) > 0) then
      result := element;
  end;
end;

function TProjectConfigPatcher.LastPropertyGroup : IXMLDOMElement;
var
  nodes : IXMLDOMNodeList;
begin
  result := nil;
  nodes := PropertyGroups;
  if nodes.length > 0 then
    result := nodes.item[nodes.length - 1] as IXMLDOMElement;
end;

//IDE authored dprojs hang the per platform DCC_Namespace off Base_<Platform>. The DPM package
//template instead puts everything under '$(Base)'!='' and needs no Base_ stubs at all - adding
//them there would be noise, so only mirror the layout the project already uses.
function TProjectConfigPatcher.UsesBasePlatformStubs : boolean;
var
  nodes : IXMLDOMNodeList;
  i : integer;
begin
  result := false;
  nodes := PropertyGroups;
  for i := 0 to nodes.length - 1 do
    if Pos('''$(Base_', ConditionOf(nodes.item[i] as IXMLDOMElement)) > 0 then
      exit(true);
end;

//Does the dproj carry ANY settings of its own for this platform? Must be asked BEFORE we start
//adding stubs, or we would only ever be looking at our own work.
//
//The Cfg_N_<Platform> chain stub is not evidence either way - the IDE writes one only when that
//config/platform pair has settings of its own, so a project that is fully configured for Linux64
//(Base_Linux64 chain + '$(Base_Linux64)'!='' settings) legitimately has no Cfg_2_Linux64 group for
//Release. Judging platform support by that stub alone cried wolf on every such project, while the
//real cause of those builds failing was elsewhere entirely.
function TProjectConfigPatcher.HasPlatformConfiguration(const configKey : string; const platformName : string) : boolean;
begin
  result := (FindChainGroup('Base', platformName) <> nil) or
            (FindSettingsGroup('Base_' + platformName) <> nil) or
            (FindChainGroup(configKey, platformName) <> nil) or
            (FindSettingsGroup(configKey + '_' + platformName) <> nil);
end;

//Not read by msbuild, but DPM's own readers depend on it - TDPMProjectSettingsLoader resolves
//the config key from here to build the /p:DCC_UnitSearchPath argument, and
//TProjectEditor.LoadConfigurations errors outright when there are no BuildConfiguration items.
function TProjectConfigPatcher.EnsureBuildConfigurationItem(const configName : string; const key : string) : boolean;
var
  nodes : IXMLDOMNodeList;
  itemGroup : IXMLDOMElement;

  function AddEntry(const includeName : string; const keyName : string; const parentName : string) : IXMLDOMElement;
  begin
    result := CreateElement('BuildConfiguration');
    result.setAttribute('Include', includeName);
    itemGroup.appendChild(result);
    AddChildText(result, 'Key', keyName);
    if parentName <> '' then
      AddChildText(result, 'CfgParent', parentName);
  end;

begin
  nodes := FProjectXml.selectNodes(buildConfigsXPath);
  if nodes.length > 0 then
    itemGroup := nodes.item[0].parentNode as IXMLDOMElement
  else
  begin
    itemGroup := CreateElement('ItemGroup');
    InsertAfter(FProjectXml.documentElement, itemGroup, LastPropertyGroup);
  end;

  //Every config chains up to Base, so the Base entry has to be there for the chain to resolve.
  if FindConfigKey('Base') = '' then
    AddEntry('Base', 'Base', '');

  AddEntry(configName, key, 'Base');
  FModified := true;
  result := true;
end;

function TProjectConfigPatcher.EnsureConfigActivator(const configName : string; const key : string) : boolean;
var
  group : IXMLDOMElement;
begin
  result := false;
  if FindActivatorGroup(configName, key) <> nil then
    exit;

  group := CreateElement('PropertyGroup');
  //exactly the shape the IDE emits.
  group.setAttribute('Condition', '''$(Config)''==''' + configName + ''' or ''$(' + key + ')''!=''''');
  AddChildText(group, key, 'true');
  AddChildText(group, 'CfgParent', 'Base');
  AddChildText(group, 'Base', 'true');

  InsertActivatorGroup(group, LastActivatorOrChainGroup);
  FModified := true;
  result := true;
end;

//Only called for a config WE invented. If the author declared the config but wrote no settings
//group that is their choice and we leave it alone.
function TProjectConfigPatcher.EnsureConfigSettings(const configName : string; const key : string) : boolean;
var
  group : IXMLDOMElement;
  isDebug : boolean;
begin
  result := false;
  if FindSettingsGroup(key) <> nil then
    exit;

  isDebug := SameText(configName, 'Debug');

  group := CreateElement('PropertyGroup');
  group.setAttribute('Condition', '''$(' + key + ')''!=''''');
  if isDebug then
  begin
    AddChildText(group, 'DCC_Define', 'DEBUG;$(DCC_Define)');
    AddChildText(group, 'DCC_Optimize', 'false');
    AddChildText(group, 'DCC_GenerateStackFrames', 'true');
    AddChildText(group, 'DCC_DebugInfoInExe', 'true');
    AddChildText(group, 'DCC_RemoteDebug', 'true');
  end
  else
  begin
    AddChildText(group, 'DCC_Define', 'RELEASE;$(DCC_Define)');
    AddChildText(group, 'DCC_LocalDebugSymbols', 'false');
    AddChildText(group, 'DCC_SymbolReferenceInfo', '0');
    //DCC_DebugInformation went from boolean to integer in XE5.
    if CompilerUsesIntegerDebugInformation(FCompiler) then
      AddChildText(group, 'DCC_DebugInformation', '0')
    else
      AddChildText(group, 'DCC_DebugInformation', 'false');
  end;

  //settings groups go after every other property group so they cannot shadow an activator.
  InsertAfter(FProjectXml.documentElement, group, LastPropertyGroup);
  FModified := true;
  result := true;
end;

//One shape serves both Base_<Platform> and Cfg_N_<Platform>; the only difference is that the
//Base stub has no redundant restatement of its own key.
function TProjectConfigPatcher.EnsureChainGroup(const key : string; const platformName : string) : boolean;
var
  group : IXMLDOMElement;
  childKey : string;
  anchor : IXMLDOMElement;
begin
  result := false;
  if FindChainGroup(key, platformName) <> nil then
    exit;

  childKey := key + '_' + platformName;
  group := CreateElement('PropertyGroup');
  group.setAttribute('Condition',
    '(''$(Platform)''==''' + platformName + ''' and ''$(' + key + ')''==''true'') or ''$(' + childKey + ')''!=''''');
  AddChildText(group, childKey, 'true');
  AddChildText(group, 'CfgParent', key);
  if not SameText(key, 'Base') then
    AddChildText(group, key, 'true');
  AddChildText(group, 'Base', 'true');

  //directly after the last stub already hanging off this key, else after the key's activator.
  anchor := LastChainGroupFor(key);
  if anchor = nil then
    anchor := FindActivatorGroup(key, key);
  InsertActivatorGroup(group, anchor);
  FModified := true;
  result := true;
end;

function TProjectConfigPatcher.EnsurePlatformListEntry(const platformName : string) : boolean;
var
  platformsNode : IXMLDOMElement;
  borlandNode : IXMLDOMElement;
  nodes : IXMLDOMNodeList;
  element : IXMLDOMElement;
  i : integer;
begin
  result := false;
  platformsNode := FProjectXml.selectSingleNode(platformsXPath) as IXMLDOMElement;
  if platformsNode = nil then
  begin
    //Don't synthesize the whole ProjectExtensions tree - if the project has no BorlandProject
    //element it isn't IDE managed and the list would mean nothing.
    borlandNode := FProjectXml.selectSingleNode(borlandProjectXPath) as IXMLDOMElement;
    if borlandNode = nil then
    begin
      FLogger.Debug('Project [' + FProjectName + '] has no ProjectExtensions platform list - leaving it alone.');
      exit;
    end;
    platformsNode := CreateElement('Platforms');
    borlandNode.appendChild(platformsNode);
    FModified := true;
  end;

  nodes := platformsNode.selectNodes('x:Platform');
  for i := 0 to nodes.length - 1 do
  begin
    element := nodes.item[i] as IXMLDOMElement;
    if not SameText(AttributeOf(element, 'value'), platformName) then
      continue;
    if StrToBoolDef(element.text, false) then
      exit;
    element.text := 'true';
    FModified := true;
    FLogger.Information('Project [' + FProjectName + '] has platform [' + platformName + '] disabled - enabling it.');
    exit(true);
  end;

  element := CreateElement('Platform');
  element.setAttribute('value', platformName);
  element.text := 'true';
  platformsNode.appendChild(element);
  FModified := true;
  FLogger.Information('Project [' + FProjectName + '] does not declare platform [' + platformName + '] - adding it.');
  result := true;
end;

//Only called when we just created a Base_<Platform> chain stub in a project that uses that
//layout - without the matching settings group the platform would compile with no Windows unit
//scope names and fail on 'Windows', 'Messages' etc.
function TProjectConfigPatcher.EnsurePlatformNamespaces(const platform : TDPMPlatform; const platformName : string) : boolean;
var
  group : IXMLDOMElement;
begin
  result := false;
  if not (platform in cWindowsPlatforms) then
    exit;
  if FindSettingsGroup('Base_' + platformName) <> nil then
    exit;

  group := CreateElement('PropertyGroup');
  group.setAttribute('Condition', '''$(Base_' + platformName + ')''!=''''');
  if platform = TDPMPlatform.Win32 then
    AddChildText(group, 'DCC_Namespace', cWin32Namespaces)
  else
    AddChildText(group, 'DCC_Namespace', cWin64Namespaces);

  InsertAfter(FProjectXml.documentElement, group, LastPropertyGroup);
  FModified := true;
  result := true;
end;

function TProjectConfigPatcher.EnsureBuildTarget(const projectFile : string; const platform : TDPMPlatform;
                                                 const configuration : string; const compiler : TCompilerVersion;
                                                 const options : TProjectPatchOptions) : TProjectPatchResult;
var
  configKey : string;
  platformName : string;
  createdConfig : boolean;
  platformConfigured : boolean;
begin
  result := TProjectPatchResult.Failed;
  FCompiler := compiler;
  FModified := false;
  FProjectName := ExtractFileName(projectFile);

  if not LoadFromFile(projectFile) then
    exit;

  platformName := DPMPlatformToBDString(platform);
  try
    //1. Which Cfg_N key does this dproj use for the config we are about to pass to msbuild?
    //   No <BuildConfiguration Include="Release"> means the config does not exist at all.
    configKey := FindConfigKey(configuration);
    createdConfig := configKey = '';
    if createdConfig then
    begin
      configKey := 'Cfg_' + IntToStr(NextCfgIndex);
      FLogger.Information('Project [' + FProjectName + '] does not declare the [' + configuration +
                          '] configuration - adding it as [' + configKey + '].');
      EnsureBuildConfigurationItem(configuration, configKey);
    end;

    //2. The activator is what actually makes /p:Config=<name> select anything, and what sets
    //   Base=true so the '$(Base)'!='' settings group applies. Without it msbuild silently uses
    //   none of the project's settings.
    if EnsureConfigActivator(configuration, configKey) and (not createdConfig) then
      FLogger.Information('Project [' + FProjectName + '] declares config [' + configuration +
                          '] but has no property group for it - adding one.');

    //3. Only stamp canonical Debug/Release settings on a config we invented.
    if createdConfig then
      EnsureConfigSettings(configuration, configKey);

    //4. The <Platform value=..> list is IDE metadata that msbuild ignores, but DPM reads it -
    //   see the design entry handling in TPackageInstaller.CompilePackage for why it is opt in.
    if TProjectPatchOption.UpdatePlatformList in options then
      EnsurePlatformListEntry(platformName);

    //5. Ask about platform support before step 6 starts writing stubs.
    platformConfigured := HasPlatformConfiguration(configKey, platformName);

    if UsesBasePlatformStubs then
    begin
      if EnsureChainGroup('Base', platformName) then
        EnsurePlatformNamespaces(platform, platformName);
    end;
    EnsureChainGroup(configKey, platformName);

    if (not platformConfigured) and (not (platform in cWindowsPlatforms)) then
      FLogger.Warning('Project [' + FProjectName + '] was not configured for [' + platformName +
                      '] - added the missing configuration, but the project has no ' + platformName +
                      ' specific settings so the build may still fail.');

    if not FModified then
    begin
      FLogger.Debug('Project [' + FProjectName + '] already declares [' + platformName + '] / [' +
                    configuration + '] - no changes needed.');
      exit(TProjectPatchResult.NotNeeded);
    end;

    if SaveToFile(projectFile) then
    begin
      FLogger.Debug('Project [' + FProjectName + '] updated in the package cache.');
      result := TProjectPatchResult.Patched;
    end;
  except
    on e : Exception do
    begin
      FLogger.Warning('Error while checking project [' + FProjectName + '] : ' + e.Message);
      result := TProjectPatchResult.Failed;
    end;
  end;
end;

end.
