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

unit DPM.Core.Project.Editor;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.MSXML,
  DPM.Core.Logging,
  DPM.Core.Project.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Configuration.Interfaces;

//TODO : create a factory object so we can create different editor implementations for different delphi versions (bdsproj, dof/cfg?)


type
  TProjectEditor = class(TInterfacedObject, IProjectEditor)
  private
    FLogger : ILogger;
    FConfig : IConfiguration;
    FProjectXML : IXMLDOMDocument;
    FCompiler : TCompilerVersion;
    FPlatforms : TDPMPlatforms;
    FProjectFile : string;
    FMainSource : string;
    FProjectVersion : string;
    FDPMCompilerVersion : string;
    FPackageRefences : IPackageReference;
    FFileName : string;
    FAppType : TAppType;
    FConfigurations : IDictionary<string, IProjectConfiguration>;
    FConfigNames : IList<string>;
    //Set true only when a DOM write actually changes content. SaveProject skips the
    //(re)format + disk write when this is false, so a no-op restore does not rewrite the dproj.
    FModified : boolean;
    //Guarded DOM writers - only mutate (and mark modified) when the value actually differs.
    procedure SetElementText(const element : IXMLDOMElement; const value : string);
    procedure SetElementAttr(const element : IXMLDOMElement; const name : string; const value : string);
    //Structural compare of the existing PackageReference DOM against the resolved graph - lets
    //UpdatePackageReferences skip the remove/rebuild (and leave FModified false) when unchanged.
    //Compares semantic attributes only, so pretty-print whitespace never reads as a difference.
    function PackageRefElementMatches(const element : IXMLDOMElement; const reference : IPackageReference) : boolean;
    function PackageRefsMatch(const parentElement : IXMLDOMElement; const references : IEnumerable<IPackageReference>) : boolean;
  protected
    function GetOutputElementName : string;

    function LoadDPMCompilerVersion : boolean;
    function LoadProjectVersion : boolean;
    function LoadMainSource : boolean;
    function LoadAppType : boolean;
    function LoadProjectPlatforms : boolean;
    function LoadPackageRefences : boolean;
    function LoadConfigurations : boolean;

    function InternalLoadFromXML(const elements : TProjectElements) : boolean;
    procedure Reset;

    function GetPlatforms : TDPMPlatforms;
    function GetCompilerVersion : TCompilerVersion;
    function GetDPMCompilerVersion : TCompilerVersion;
    function GetProjectVersion : string;
    function GetAppType : TAppType;
    function GetHasPackages : boolean;
    function GetProjectFile : string;
    function GetHasDPM : boolean;
    function GetProjectConfiguration(const name : string; const platform : TDPMPlatform) : IProjectConfiguration;

    procedure SetCompiler(const value : TCompilerVersion);

    function LoadProject(const filename : string; const elements : TProjectElements = [TProjectElement.All]) : Boolean;
    function SaveProject(const filename : string) : Boolean;

    function GetDPMPropertyGroup : IXMLDOMElement;
    function EnsureBaseSearchPath : boolean;

    procedure RemoveFromSearchPath(const platform : TDPMPlatform; const packageId : string);
    function AddSearchPaths(const platform : TDPMPlatform; const searchPaths : IList<string> ; const packageCacheLocation : string) : boolean;
    function EnsureCopyLocalImport(const dpmExePath : string) : boolean;
    procedure UpdatePackageReferences(const dependencyGraph : IPackageReference);
    function GetPackageReferences : IPackageReference;
    function GetConfigNames: IReadOnlyList<string>;
    function GetSourceFiles : IList<string>;
  public
    constructor Create(const logger : ILogger; const config : IConfiguration; const compilerVersion : TCompilerVersion);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  DPM.Core.Constants,
  DPM.Core.Utils.Xml,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Reference,
//  DPM.Core.Project.PackageReference,
  DPM.Core.Project.CopyLocalTargets,
  DPM.Core.Project.Configuration;

const
  msbuildNamespace = 'http://schemas.microsoft.com/developer/msbuild/2003';

  projectVersionXPath = '/x:Project/x:PropertyGroup/x:ProjectVersion';

  dpmCcompilerXPath = '/x:Project/x:PropertyGroup/x:DPMCompiler';

  mainSourceXPath = '/x:Project/x:PropertyGroup/x:MainSource';

  projectAppTypeXPath = '/x:Project/x:PropertyGroup/x:AppType';

  platformsXPath = '/x:Project/x:ProjectExtensions/x:BorlandProject/x:Platforms/x:Platform';

  projectExtensionsXPath = '/x:Project/x:ProjectExtensions';

  packagesXPath = '/x:Project/x:ProjectExtensions/x:DPM/x:Packages';

  dpmElementPath = '/x:Project/x:PropertyGroup/x:DPM';

  baseConfigPath = '/x:Project/x:PropertyGroup[@Condition="''$(Base)''!=''''"]';

  buildConfigsXPath = 'x:Project/x:ItemGroup/x:BuildConfiguration';

  configMainXPath = '/x:Project/x:PropertyGroup[@Condition="''$(%s)''!=''''"]';


function StringToAppType(const value : string) : TAppType;
begin
  result := TAppType.Unknown;
  if (value = 'Application') or (value = 'Console') then
    result := TAppType.Application
  else if value = 'Package' then
    result := TAppType.Package
  else if value = 'Library' then
    result := TAppType.Lib;
  //TODO : what are the other project types, and do they matter.

end;

{ TProjectEditor }

procedure TProjectEditor.SetElementText(const element : IXMLDOMElement; const value : string);
begin
  if element = nil then
    exit;
  if element.text <> value then
  begin
    element.text := value;
    FModified := true;
  end;
end;

procedure TProjectEditor.SetElementAttr(const element : IXMLDOMElement; const name : string; const value : string);
var
  current : OleVariant;
begin
  if element = nil then
    exit;
  current := element.getAttribute(name);
  //getAttribute returns Null when the attribute is absent; VarToStr maps that to ''.
  if VarIsNull(current) or (VarToStr(current) <> value) then
  begin
    element.setAttribute(name, value);
    FModified := true;
  end;
end;

function TProjectEditor.AddSearchPaths(const platform : TDPMPlatform; const searchPaths : IList<string> ; const packageCacheLocation : string) : boolean;
var
  dpmGroup : IXMLDOMElement;
  dpmSearchElement : IXMLDOMElement;
  condition : string;
  searchPath : string;
  searchPathString : string;
begin
  result := false;
  dpmGroup := GetDPMPropertyGroup;
  if dpmGroup = nil then
  begin
    FLogger.Error('Unable to find or create PropertyGroup for DPM in the project file');
    exit;
  end;

  result := EnsureBaseSearchPath;
  if not result then
    exit;


  condition := '''$(Platform)''==''' + DPMPlatformToBDString(platform) + '''';

  dpmSearchElement := dpmGroup.selectSingleNode('x:DPMSearch[@Condition = "' + condition + '"]') as IXMLDOMElement;

  if dpmSearchElement = nil then
  begin
    dpmSearchElement := FProjectXML.createNode(NODE_ELEMENT, 'DPMSearch', msbuildNamespace) as IXMLDOMElement;
    dpmGroup.appendChild(dpmSearchElement);
    FModified := true;
  end;
  SetElementAttr(dpmSearchElement, 'Condition', condition);

  for searchPath in searchPaths do
  begin
    searchPathString := searchPathString + '$(DPM)\' + searchPath + ';'
  end;

  SetElementText(dpmSearchElement, searchPathString);



end;

function TProjectEditor.EnsureCopyLocalImport(const dpmExePath : string) : boolean;
var
  dpmGroup : IXMLDOMElement;
  dpmExeElement : IXMLDOMElement;
  importElement : IXMLDOMElement;
  importProject : string;
begin
  result := false;
  dpmGroup := GetDPMPropertyGroup;
  if dpmGroup = nil then
  begin
    FLogger.Error('Unable to find or create PropertyGroup for DPM in the project file');
    exit;
  end;

  //Record the dpm.exe path so the targets file can prefer it over a PATH lookup. Only write it when
  //we actually know it's dpm (the IDE plugin restoring would otherwise record bds.exe here).
  if dpmExePath <> '' then
  begin
    dpmExeElement := dpmGroup.selectSingleNode('x:DPMExe') as IXMLDOMElement;
    if dpmExeElement = nil then
    begin
      dpmExeElement := FProjectXML.createNode(NODE_ELEMENT, 'DPMExe', msbuildNamespace) as IXMLDOMElement;
      dpmGroup.appendChild(dpmExeElement);
      FModified := true;
    end;
    SetElementText(dpmExeElement, dpmExePath);
  end;

  //Cache-relative import - $(DPMCache) is defined in the DPM PropertyGroup above (earlier in document
  //order), so it resolves when MSBuild evaluates this import. Exists()-guarded so a build never fails
  //when the cache or targets file is missing.
  importProject := '$(DPMCache)\' + cCopyLocalTargetsFileName;
  importElement := FProjectXML.selectSingleNode('/x:Project/x:Import[@Project="' + importProject + '"]') as IXMLDOMElement;
  if importElement = nil then
  begin
    importElement := FProjectXML.createNode(NODE_ELEMENT, 'Import', msbuildNamespace) as IXMLDOMElement;
    FProjectXML.documentElement.appendChild(importElement);
    FModified := true;
  end;
  SetElementAttr(importElement, 'Project', importProject);
  SetElementAttr(importElement, 'Condition', 'Exists(''' + importProject + ''')');
  result := true;
end;

constructor TProjectEditor.Create(const logger : ILogger; const config : IConfiguration; const compilerVersion : TCompilerVersion);
begin
  FLogger := logger;
  FConfig := config;
  FCompiler := compilerVersion;
  FPlatforms := [];
  FPackageRefences := nil;
  FConfigurations := TCollections.CreateDictionary <string, IProjectConfiguration> ;
  FConfigNames := TCollections.CreateList<string>;
  FModified := false;
end;

function TProjectEditor.GetPackageReferences : IPackageReference;
begin
  result := FPackageRefences;

end;

function TProjectEditor.GetPlatforms : TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TProjectEditor.GetProjectConfiguration(const name: string;  const platform: TDPMPlatform): IProjectConfiguration;
var
  sKey : string;
begin
  result := nil;
  sKey := LowerCase(name + '_' + DPMPlatformToBDString(platform));
  FConfigurations.TryGetValue(sKey, result);
end;

function TProjectEditor.GetProjectFile: string;
begin
  result := FProjectFile;
end;

function TProjectEditor.GetProjectVersion : string;
begin
  result := FProjectVersion;
end;

function TProjectEditor.EnsureBaseSearchPath : boolean;
var
  baseElement : IXMLDOMElement;
  dccSearchElement : IXMLDOMElement;
  searchPath : string;
begin
  result := false;

  baseElement := FProjectXML.selectSingleNode(baseConfigPath) as IXMLDOMElement;
  if baseElement = nil then
  begin
    FLogger.Error('Unable to find the base config element in the project, cannot set search path');
    exit;
  end;

  dccSearchElement := baseElement.selectSingleNode('x:DCC_UnitSearchPath') as IXMLDOMElement;
  if dccSearchElement = nil then
  begin
    dccSearchElement := FProjectXML.createNode(NODE_ELEMENT, 'DCC_UnitSearchPath', msbuildNamespace) as IXMLDOMElement;
    dccSearchElement.text := '$(DPMSearch);$(DCC_UnitSearchPath)';
    baseElement.appendChild(dccSearchElement);
    FModified := true;
  end
  else
  begin
    //remove and add at the beginning
    searchPath := StringReplace(dccSearchElement.text, '$(DPMSearch);', '', [rfReplaceAll]);
    searchPath := StringReplace(searchPath, '$(DPMSearch)', '', [rfReplaceAll]);
    searchPath := '$(DPMSearch);' + searchPath;
    SetElementText(dccSearchElement, searchPath);
  end;
  result := true;
end;

function TProjectEditor.GetDPMPropertyGroup : IXMLDOMElement;
var
  projectVersionGroup : IXMLDOMElement;
  xmlElement : IXMLDOMElement;
  dpmCompilerElement : IXMLDOMElement;
  dpmElement : IXMLDOMElement;
  dpmCondition : IXMLDOMElement;
begin

  dpmElement := FProjectXML.selectSingleNode(dpmElementPath) as IXMLDOMElement;
  if dpmElement = nil then
  begin
    //dpm isn't confugured in this project, so create the propertyGroup
    xmlElement := FProjectXML.selectSingleNode(projectVersionXPath) as IXMLDOMElement;
    if xmlElement = nil then
    begin
      FLogger.Error('Unable to location position in project to insert dpm propertygroup');
      exit;
    end;
    projectVersionGroup := xmlElement.parentNode as IXMLDOMElement;
    result := FProjectXML.createNode(NODE_ELEMENT, 'PropertyGroup', msbuildNamespace) as IXMLDOMElement;
    FProjectXML.documentElement.insertBefore(result, projectVersionGroup.nextSibling);
    FModified := true;
  end
  else
    result := dpmElement.parentNode as IXMLDOMElement;

  dpmCompilerElement := result.selectSingleNode('x:DPMCompiler') as IXMLDOMElement;
  if dpmCompilerElement = nil then
  begin
    dpmCompilerElement := FProjectXML.createNode(NODE_ELEMENT, 'DPMCompiler', msbuildNamespace) as IXMLDOMElement;
    result.appendChild(dpmCompilerElement);
    FModified := true;
  end;
  SetElementText(dpmCompilerElement, CompilerToString(FCompiler));


  dpmCondition := result.selectSingleNode('x:DPMCache[@Condition != '''']') as IXMLDOMElement;
  if dpmCondition = nil then
  begin
    dpmCondition := FProjectXML.createNode(NODE_ELEMENT, 'DPMCache', msbuildNamespace) as IXMLDOMElement;
    result.appendChild(dpmCondition);
    FModified := true;
  end;
  SetElementAttr(dpmCondition, 'Condition', '''$(DPMCache)'' == ''''');
  if FConfig.IsDefaultPackageCacheLocation then
    SetElementText(dpmCondition, StringReplace(cDefaultPackageCache, '%APPDATA%', '$(APPDATA)', [rfIgnoreCase])) //
  else
  begin
    //see if we can covert this to a relative path
    SetElementText(dpmCondition, FConfig.PackageCacheLocation);
  end;

  dpmElement := result.selectSingleNode('x:DPM') as IXMLDOMElement;
  if dpmElement = nil then
  begin
    dpmElement := FProjectXML.createNode(NODE_ELEMENT, 'DPM', msbuildNamespace) as IXMLDOMElement;
    result.appendChild(dpmElement);
    FModified := true;
  end;
  //One package folder per compiler now contains all platforms - no $(Platform) segment.
  //Per-platform paths are formed by appending \$(Platform) to the lib folder where applicable.
  SetElementText(dpmElement, '$(DPMCache)\$(DPMCompiler)');

end;

function TProjectEditor.GetHasDPM: boolean;
begin
  result := FDPMCompilerVersion <> '';
end;

function TProjectEditor.GetHasPackages : boolean;
begin
  result := (FPackageRefences <> nil) and FPackageRefences.HasChildren;
end;

function TProjectEditor.GetOutputElementName : string;
begin
  case FAppType of
    TAppType.Application,
    TAppType.Lib : result := 'x:DCC_ExeOutput';
    TAppType.Package : result := 'x:DCC_BplOutput';
    TAppType.Unknown : raise Exception.Create('Invalid AppType');
  end;
end;

function TProjectEditor.GetAppType : TAppType;
begin
  result := FAppType;
end;

function TProjectEditor.GetCompilerVersion : TCompilerVersion;
begin
  result := FCompiler;
end;

function TProjectEditor.GetDPMCompilerVersion : TCompilerVersion;
begin
  //FDPMCompilerVersion is the raw <DPMCompiler> text captured at load - the compiler the project
  //was last managed by DPM under. Empty (UnknownVersion) when the project has no DPM marker.
  if FDPMCompilerVersion = '' then
    result := TCompilerVersion.UnknownVersion
  else
    result := StringToCompilerVersion(FDPMCompilerVersion);
end;

function TProjectEditor.GetConfigNames: IReadOnlyList<string>;
begin
  result := FConfigNames as IReadOnlyList<string>;
end;

function TProjectEditor.GetSourceFiles : IList<string>;
var
  nodes : IXMLDOMNodeList;
  element : IXMLDOMElement;
  i : integer;
  includeValue : string;
begin
  //Returns the raw Include attribute values of every DCCReference + DelphiCompile
  //in the dproj. Preserves the original (typically relative) form so the caller
  //can resolve against the dproj's directory as needed. Parses on demand from
  //FProjectXML - no LoadProject element flag required.
  result := TCollections.CreateList<string>;
  if FProjectXML = nil then
    exit;

  //DCCReference items - the regular .pas units compiled into the binary.
  nodes := FProjectXML.selectNodes('/x:Project/x:ItemGroup/x:DCCReference');
  if nodes <> nil then
  begin
    for i := 0 to nodes.length - 1 do
    begin
      element := nodes.item[i] as IXMLDOMElement;
      if (element <> nil) and (element.getAttributeNode('Include') <> nil) then
      begin
        includeValue := element.getAttribute('Include');
        if includeValue <> '' then
          result.Add(includeValue);
      end;
    end;
  end;

  //DelphiCompile items - the main .dpr / .dpk source file (typically one).
  //Often an MSBuild macro like '$(MainSource)' rather than a literal path -
  //add as-is, callers tolerate unresolvable entries (only the directory matters).
  nodes := FProjectXML.selectNodes('/x:Project/x:ItemGroup/x:DelphiCompile');
  if nodes <> nil then
  begin
    for i := 0 to nodes.length - 1 do
    begin
      element := nodes.item[i] as IXMLDOMElement;
      if (element <> nil) and (element.getAttributeNode('Include') <> nil) then
      begin
        includeValue := element.getAttribute('Include');
        if (includeValue <> '') and (Pos('$(', includeValue) = 0) then
          result.Add(includeValue);
      end;
    end;
  end;
end;

function TProjectEditor.InternalLoadFromXML(const elements : TProjectElements) : boolean;
var
  loadAll : boolean;
begin
  result := true;
  loadAll := TProjectElement.All in elements;
  if loadAll or (TProjectElement.DPMCompiler in elements) then
    result := result and LoadDPMCompilerVersion;
  if loadAll or (TProjectElement.ProjectVersion in elements) then
    result := result and LoadProjectVersion;
  if loadAll or (TProjectElement.MainSource in elements) then
    result := result and LoadMainSource;
  if loadAll or (TProjectElement.AppType in elements) then
    result := result and LoadAppType;
  if loadAll or (TProjectElement.Platforms in elements) then
    result := result and LoadProjectPlatforms;
  if loadAll or (TProjectElement.Configs in elements) then
    result := result and LoadConfigurations; //must be after platforms
  if loadAll or (TProjectElement.PackageRefs in elements) then
    result := result and LoadPackageRefences;
end;

function TProjectEditor.LoadAppType : boolean;
var
  xmlElement : IXMLDOMElement;
  sAppType : string;
begin
  result := false;
  xmlElement := FProjectXML.selectSingleNode(projectAppTypeXPath) as IXMLDOMElement;
  if xmlElement <> nil then
  begin
    sAppType := xmlElement.text;
    if sAppType <> '' then
    begin
      FAppType := StringToAppType(sAppType);

      if FAppType <> TAppType.Unknown then
        result := true
      else
        FLogger.Error('Unable to determine AppType from project file.');
    end
    else
      FLogger.Error('AppType element is empty, unable to determine AppType from project file.');
  end
  else
    FLogger.Error('ProjectVersion element not found, unable to determine AppType from project file.');
end;

function TProjectEditor.LoadConfigurations : boolean;
var
  configNodeList : IXMLDOMNodeList;
  tmpElement : IXMLDOMElement;
  keyElement : IXMLDOMElement;
  parentElement : IXMLDOMElement;
  i : integer;
  sName : string;
  sKey : string;
  sParent : string;
  configKeys : TStringList;
  configParents : TStringList;
  platform : TDPMPlatform;
  sPlatform : string;

  sOutputDir : string;
  sUsePackages : string;
  newConfig : IProjectConfiguration;

  function TryGetConfigValue(const configKey : string; const platform : string; const elementName : string; out value : string) : boolean;
  var
    configElement : IXMLDOMElement;
    valueElement : IXMLDOMElement;
    sParentKey : string;
  begin
    result := false;
    //first we try the config_platform eg cfg_3_Win32
    configElement := FProjectXML.selectSingleNode(Format(configMainXPath, [configKey + '_' + platform])) as IXMLDOMElement;
    if configElement <> nil then
    begin
      valueElement := configElement.selectSingleNode(elementName) as IXMLDOMElement;
      if valueElement <> nil then
      begin
        value := valueElement.text;
        exit(true);
      end;
    end;
    //the config_platform didn't work, so try just the config
    configElement := FProjectXML.selectSingleNode(Format(configMainXPath, [configKey])) as IXMLDOMElement;
    if configElement <> nil then
    begin
      valueElement := configElement.selectSingleNode(elementName) as IXMLDOMElement;
      if valueElement <> nil then
      begin
        value := valueElement.text;
        exit(true);
      end;
    end;
    //that didn't work, so recurse
    sParentKey := configParents.Values[configKey];
    if sParentKey = '' then //we are at Base so we're done
      exit;

    result := TryGetConfigValue(sParentKey, platform, elementName, value);
  end;

begin
  result := false;
  configNodeList := FProjectXML.selectNodes(buildConfigsXPath);
  if configNodeList.length > 0 then
  begin
    configKeys := TStringList.Create;
    configParents := TStringList.Create;
    try
      for i := 0 to configNodeList.length - 1 do
      begin
        sName := '';
        sKey := '';
        sParent := '';
        tmpElement := configNodeList.item[i] as IXMLDOMElement;
        if tmpElement <> nil then
        begin
          sName := tmpElement.getAttribute('Include');
          keyElement := tmpElement.selectSingleNode('x:Key') as IXMLDOMElement;
          if keyElement <> nil then
            sKey := keyElement.text;
          parentElement := tmpElement.selectSingleNode('x:CfgParent') as IXMLDOMElement;
          if parentElement <> nil then
            sParent := parentElement.text;

          FConfigNames.Add(sName);
          configKeys.Add(sName + '=' + sKey);
          configParents.Add(sKey + '=' + sParent);
        end;
      end;

      //loop through the configs and get the values we need, then create a config item
      for platform in FPlatforms do
      begin
        sPlatform := DPMPlatformToBDString(platform);
        for i := 0 to configKeys.Count - 1 do
        begin
          //don't create a config for Base!
          if configKeys.Names[i] = 'Base' then
            continue;
          sKey := configKeys.ValueFromIndex[i];

          sOutputDir := '';
          if TryGetConfigValue(sKey, sPlatform, GetOutputElementName, sOutputDir) then
          begin
            //deal with $(Platform)\$(Config)
            sOutputDir := StringReplace(sOutputDir, '$(Platform)', sPlatform, [rfReplaceAll, rfIgnoreCase]);
            sOutputDir := StringReplace(sOutputDir, '$(Config)', configKeys.Names[i], [rfReplaceAll, rfIgnoreCase]);
          end;
//          if sOutputDir = '' then
//             FLogger.Debug('No output directory found for config ' + configKeys.Names[i] + '_' + sPlatform);

          sUsePackages := 'false';
          TryGetConfigValue(sKey, sPlatform, 'x:UsePackages', sUsePackages);
          newConfig := TProjectConfiguration.Create(configKeys.Names[i], sOutputDir, platform, StrToBoolDef(sUsePackages, false), nil);
          sKey := LowerCase(configKeys.Names[i] + '_' + sPlatform);
          FConfigurations[sKey] := newConfig;
        end;
      end;
      result := true;
    finally
      configKeys.Free;
      configParents.Free;
    end;

  end
  else
    FLogger.Error('Unabled to find any BuildConfiguration elements in project file!');


end;

function TProjectEditor.LoadMainSource : boolean;
var
  xmlElement : IXMLDOMElement;
begin
  result := false;
  xmlElement := FProjectXML.selectSingleNode(mainSourceXPath) as IXMLDOMElement;
  if xmlElement <> nil then
  begin
    FMainSource := xmlElement.text;
    result := FMainSource <> '';
    if not result then
      FLogger.Error('Unable to determine Compiler version from ProjectVersion');
  end
  else
    FLogger.Error('ProjectVersion element not found, unable to determine Compiler version');
end;

function TProjectEditor.LoadPackageRefences : boolean;

  procedure ReadPackageReferences(const parentReference : IPackageReference; const parentElement : IXMLDOMElement);
  var
    isTransitive : boolean;
    packageNodes : IXMLDOMNodeList;
    packageElement : IXMLDOMElement;
    i : integer;
    id : string;
    sVersion : string;
    version : TPackageVersion;
    error : string;
    sRange : string;
    range : TVersionRange;
    dupCheckReference : IPackageReference;
    useSource : boolean;
    sUseSource : string;
    newNode : IPackageReference;
  begin
    isTransitive := parentReference <> nil;

    packageNodes := parentElement.selectNodes('x:PackageReference');
    if packageNodes.length > 0 then
    begin
      for i := 0 to packageNodes.length - 1 do
      begin
        id := '';
        sVersion := '';
        sUseSource := '';

        packageElement := packageNodes.item[i] as IXMLDOMElement;
        if packageElement.getAttributeNode('id') <> nil then
          id := packageElement.getAttribute('id');
        if id = '' then
        begin
          FLogger.Error('Invalid package reference detected in project, missing required [id] attribute');
          result := false;
          exit;
        end;

        if packageElement.getAttributeNode('version') <> nil then
          sVersion := packageElement.getAttribute('version');
        if sVersion = '' then
        begin
          FLogger.Error('Invalid package reference detected in project, missing required [version] attribute');
          result := false;
           exit;
        end;
        if not TPackageVErsion.TryParseWithError(sVersion, version, error) then
        begin
          FLogger.Error('Invalid package reference detected in project, [version] attribute is not valid');
          FLogger.Error(' ' + error);
          result := false;
          exit;
        end;

        //if the parent reference is using source then we must use source for Transitive references.
        if (parentReference <> nil) and parentReference.UseSource then
          useSource := true
        else if packageElement.getAttributeNode('useSource') <> nil then
        begin
          sUseSource := packageElement.getAttribute('useSource');
          useSource := StrToBoolDef(sUseSource, false);
        end
        else
          useSource := false;

        if FPackageRefences = nil then
          FPackageRefences := TPackageReference.CreateRoot(FCompiler);

        //check for duplicate references
        if isTransitive then
          dupCheckReference := parentReference
        else
          dupCheckReference := FPackageRefences;

        if dupCheckReference.FindTopLevelChild(id) <> nil then
        begin
          if parentReference <> nil then
            raise Exception.Create('Duplicate package reference for package [' + id  + '] under [' + parentReference.Id + ']')
          else
            raise Exception.Create('Duplicate package reference for package [' + id + ']');
        end;

        //only transitive packages need a range
        if isTransitive then
        begin
          range := TVersionRange.Empty;
          if packageElement.getAttributeNode('range') <> nil then
          begin
            sRange := packageElement.getAttribute('range');
            if not TVersionRange.TryParseWithError(sRange, range, error) then
            begin
              FLogger.Error('Invalid package reference detected in project, [version] attribute is not valid');
              FLogger.Error(' ' + error);
              result := false;
              exit;
            end;
          end
          else
          begin
            FLogger.Error('Invalid package reference detected in project, missing required [range] attribute on transitive refererence');
            FLogger.Error('Remove the reference and run restore to correct the reference.');
            result := false;
            exit;
          end;
        end;
        if isTransitive then
        begin
          newNode  := parentReference.AddChild(id, version, range);
          newNode.UseSource := useSource;
        end
        else
        begin
          newNode := FPackageRefences.AddChild(id, version, TVersionRange.Empty);
          newNode.UseSource := useSource;
        end;
        // P2 §2.6 — optional manifest-hash lock attribute. Tolerated when
        // absent; older projects without a lock just won't get a lock check.
        if packageElement.getAttributeNode('manifestHash') <> nil then
          newNode.ManifestHash := packageElement.getAttribute('manifestHash');
        ReadPackageReferences(newNode, packageElement);
      end;
    end;
  end;

var
  packagesElement : IXMLDOMElement;
begin
  result := true;
  packagesElement := FProjectXML.selectSingleNode(packagesXPath) as IXMLDOMElement;
  if packagesElement <> nil then
    ReadPackageReferences(nil, packagesElement);
end;

function TProjectEditor.LoadProject(const filename : string; const elements : TProjectElements) : Boolean;
begin
  result := false;
  FPackageRefences := nil;
  FPlatforms := [];
  FProjectVersion := '';

  if not FileExists(fileName) then
  begin
    FLogger.Error('Project file : [' + filename + '] does not exist');
    exit;
  end;
  FProjectFile := filename;
  FProjectXML := CoDOMDocument60.Create;

  try
    result := TXMLUtils.LoadXMLFromFile(FProjectXML, fileName);
    if not result then
    begin
      FLogger.Error('Error loading project file : ' + FProjectXML.parseError.reason);
      exit;
    end;
    FFileName := filename;
    (FProjectXML as IXMLDOMDocument2).setProperty('SelectionLanguage', 'XPath');
    (FProjectXML as IXMLDOMDocument2).setProperty('SelectionNamespaces', 'xmlns:x=''http://schemas.microsoft.com/developer/msbuild/2003''');

    result := InternalLoadFromXML(elements);
    //freshly loaded document is in sync with disk - any later guarded write flips this.
    FModified := false;
  except
    on e : Exception do
    begin
      FLogger.Error('Error loading project xml doc : ' + e.Message);
      exit;
    end;
  end;
end;

//TODO - check that the dproj platforms convert to ours.
function TProjectEditor.LoadProjectPlatforms : boolean;
var
  platformNodes : IXMLDOMNodeList;
  platformElement : IXMLDOMElement;
  i : integer;
  sValue : string;
  sEnabled : string;
  platform : TDPMPlatform;
begin
  result := true;
  platformNodes := FProjectXML.selectNodes(platformsXPath);
  if platformNodes.length > 0 then
  begin
    for i := 0 to platformNodes.length - 1 do
    begin
      sValue := '';
      platformElement := platformNodes.item[i] as IXMLDOMElement;
      if platformElement.getAttributeNode('value') <> nil then
        sValue := platformElement.getAttribute('value');
      sEnabled := platformElement.text;
      if StrToBoolDef(sEnabled, false) then
      begin
        platform := ProjectPlatformToDPMPlatform(sValue);
        if platform <> TDPMPlatform.UnknownPlatform then
          FPlatforms := FPlatforms + [platform]
        else
          result := false;
      end;
    end;
  end;
end;

function TProjectEditor.LoadDPMCompilerVersion : boolean;
var
  xmlElement : IXMLDOMElement;
begin
  result := true;
  xmlElement := FProjectXML.selectSingleNode(dpmCcompilerXPath) as IXMLDOMElement;
  if xmlElement <> nil then
  begin
    FDPMCompilerVersion := xmlElement.text;
    if FDPMCompilerVersion <> '' then
    begin
      //only use the dpmcompilerversion if we haven't already set the compilerversion.
      if FCompiler = TCompilerVersion.UnknownVersion then
      begin
        FCompiler := StringToCompilerVersion(FDPMCompilerVersion);
        if FCompiler <> TCompilerVersion.UnknownVersion then
          result := true
        else
          FLogger.Error('Unable to determine Compiler version from DPMCompiler');
      end;
    end
  end
end;


function TProjectEditor.LoadProjectVersion : boolean;
var
  xmlElement : IXMLDOMElement;
begin
  result := false;
  xmlElement := FProjectXML.selectSingleNode(projectVersionXPath) as IXMLDOMElement;
  if xmlElement <> nil then
  begin
    FProjectVersion := xmlElement.text;
    if FProjectVersion <> '' then
    begin
      //only use the projectversion if we haven't already set the compilerversion.
      if FCompiler = TCompilerVersion.UnknownVersion then
      begin
        FCompiler := ProjectVersionToCompilerVersion(FProjectVersion);
        if FCompiler <> TCompilerVersion.UnknownVersion then
          result := true
        else
          FLogger.Error('Unable to determine Compiler version from ProjectVersion');
      end
      else
        result := true;
    end
    else
      FLogger.Error('ProjectVersion element is empty, unable to determine Compiler version.');
  end
  else
    FLogger.Error('ProjectVersion element not found, unable to determine Compiler version');
end;

procedure TProjectEditor.RemoveFromSearchPath(const platform: TDPMPlatform; const packageId: string);
var
  dpmGroup : IXMLDOMElement;
  dpmSearchElement : IXMLDOMElement;
  condition : string;
  searchPathPrefix : string;
  sList : TStringList;
  i : integer;
begin
  dpmGroup := GetDPMPropertyGroup;
  if dpmGroup = nil then
  begin
    FLogger.Error('Unable to find or create PropertyGroup for DPM in the project file');
    exit;
  end;

  condition := '''$(Platform)''==''' + DPMPlatformToBDString(platform) + '''';

  dpmSearchElement := dpmGroup.selectSingleNode('x:DPMSearch[@Condition = "' + condition + '"]') as IXMLDOMElement;

  //not found..
  if dpmSearchElement = nil then
    exit;

  searchPathPrefix := '$(DPM)\' + packageId + '\';

  sList := TStringList.Create;
  try
    sList.Delimiter := ';';
    sList.DelimitedText := dpmSearchElement.text;

    for i := sList.Count -1  downto 0 do
    begin
      // if there is a trailing delimeter we end up with an empty entry which we do not want
      if StartsText(searchPathPrefix, sList.Strings[i]) or (sList.Strings[i] = '') then
      begin
        sList.Delete(i);
      end;
    end;
    SetElementText(dpmSearchElement, sList.DelimitedText);

  finally
    sList.Free;
  end;
end;

procedure TProjectEditor.Reset;
begin
  FProjectXML := nil;
  FCompiler := TCompilerVersion.UnknownVersion;
  FPlatforms := [];
  FModified := false;
end;


function TProjectEditor.SaveProject(const filename : string) : Boolean;
var
  projectFileName : string;
begin
  result := false;
  if FProjectXML = nil then
  begin
    FLogger.Error('Unable to save project file, nothing loaded.');
    exit;
  end;


  if filename <> '' then
    projectFileName := fileName
  else
    projectFileName := FFileName;

  //Nothing changed in the DOM since load/last save and we are writing back to the same file -
  //skip the (re)format + disk write entirely. This is the common warm-restore no-op case.
  //An explicit target filename (save-as) always writes.
  if (filename = '') and (not FModified) then
  begin
    FLogger.Debug('Project [' + projectFileName + '] unchanged - skipping save.');
    result := true;
    exit;
  end;

  //TODO : Apply package references.


  try
    TXMLUtils.PrettyFormatXML(FProjectXML.documentElement, 4);

    FProjectXML.save(projectFileName);
    FModified := false;
    result := true;
  except
    on e : Exception do
    begin
      FLogger.Error('Error saving project [' + filename + ']');
      FLogger.Error('  ' + e.Message);
    end;
  end;
end;

procedure TProjectEditor.SetCompiler(const value : TCompilerVersion);
begin
  FCompiler := value;


end;

function TProjectEditor.PackageRefElementMatches(const element : IXMLDOMElement; const reference : IPackageReference) : boolean;
begin
  result := false;
  if VarToStr(element.getAttribute('id')) <> reference.Id then
    exit;
  if VarToStr(element.getAttribute('version')) <> reference.Version.ToStringNoMeta then
    exit;
  //range/useSource/manifestHash are only written when meaningful - so an absent attribute must
  //match a default-valued reference, and a present attribute must match the written value.
  if reference.VersionRange.IsEmpty then
  begin
    if not VarIsNull(element.getAttribute('range')) then
      exit;
  end
  else if VarToStr(element.getAttribute('range')) <> reference.VersionRange.ToString then
    exit;

  if reference.UseSource then
  begin
    if VarToStr(element.getAttribute('useSource')) <> 'true' then
      exit;
  end
  else if not VarIsNull(element.getAttribute('useSource')) then
    exit;

  if reference.ManifestHash <> '' then
  begin
    if VarToStr(element.getAttribute('manifestHash')) <> reference.ManifestHash then
      exit;
  end
  else if not VarIsNull(element.getAttribute('manifestHash')) then
    exit;

  if reference.HasChildren then
    result := PackageRefsMatch(element, reference.Children)
  else
    result := element.selectNodes('x:PackageReference').length = 0;
end;

function TProjectEditor.PackageRefsMatch(const parentElement : IXMLDOMElement; const references : IEnumerable<IPackageReference>) : boolean;
var
  childElements : IXMLDOMNodeList;
  reference : IPackageReference;
  childElement : IXMLDOMElement;
  i : integer;
begin
  result := false;
  childElements := parentElement.selectNodes('x:PackageReference');
  i := 0;
  for reference in references do
  begin
    if i >= childElements.length then
      exit;
    childElement := childElements.item[i] as IXMLDOMElement;
    if not PackageRefElementMatches(childElement, reference) then
      exit;
    Inc(i);
  end;
  //the graph and the DOM must have exactly the same number of refs at this level
  result := i = childElements.length;
end;

procedure TProjectEditor.UpdatePackageReferences(const dependencyGraph: IPackageReference);
var
  projectExtensionsElement: IXMLDOMElement;
  dpmElement: IXMLDOMElement;
  packagesElement: IXMLDOMElement;
  packageRefs: IXMLDOMNodeList;
  i: integer;
  topLevelReference: IPackageReference;

  procedure WritePackageRef(const parentElement: IXMLDOMElement; const packageReference: IPackageReference);
  var
    packageRefElement: IXMLDOMElement;
    dependency: IPackageReference;
  begin
    packageRefElement := FProjectXML.createNode(NODE_ELEMENT, 'PackageReference', msbuildNamespace) as IXMLDOMElement;
    packageRefElement.setAttribute('id', packageReference.Id);
    packageRefElement.setAttribute('version', packageReference.Version.ToStringNoMeta);
    if not packageReference.VersionRange.IsEmpty then
      packageRefElement.setAttribute('range', packageReference.VersionRange.ToString);
    if packageReference.UseSource then
      packageRefElement.setAttribute('useSource', 'true');
    // P2 §2.6 — write the lock hash if the installer recorded one. Older
    // installs may have no hash yet; that's tolerated until next upgrade.
    if packageReference.ManifestHash <> '' then
      packageRefElement.setAttribute('manifestHash', packageReference.ManifestHash);
    parentElement.appendChild(packageRefElement);
    if packageReference.HasChildren then
    begin
      for dependency in packageReference.Children do
        WritePackageRef(packageRefElement, dependency);
    end;
  end;

begin
  projectExtensionsElement := FProjectXML.selectSingleNode(projectExtensionsXPath) as IXMLDOMElement;
  if projectExtensionsElement = nil then
  begin
    FLogger.Error('Unable to find ProjectExtensions element in project file.');
    exit;
  end;

  dpmElement := projectExtensionsElement.selectSingleNode('x:DPM') as IXMLDOMElement;
  if dpmElement = nil then
  begin
    dpmElement := FProjectXML.createNode(NODE_ELEMENT, 'DPM', msbuildNamespace) as IXMLDOMElement;
    projectExtensionsElement.appendChild(dpmElement);
    FModified := true;
  end;

  // Find or create the Packages element
  packagesElement := dpmElement.selectSingleNode('x:Packages') as IXMLDOMElement;

  //If the existing Packages element already represents this exact graph there is nothing to do -
  //skip the remove/rebuild churn and leave FModified untouched so SaveProject can skip the write.
  if (packagesElement <> nil) and PackageRefsMatch(packagesElement, dependencyGraph.Children) then
    exit;

  if packagesElement <> nil then
  begin
    // Remove existing package references
    packageRefs := packagesElement.selectNodes('x:PackageReference');
    for i := packageRefs.length - 1 downto 0 do
      packagesElement.removeChild(packageRefs.item[i]);
  end
  else
  begin
    packagesElement := FProjectXML.createNode(NODE_ELEMENT, 'Packages', msbuildNamespace) as IXMLDOMElement;
    dpmElement.appendChild(packagesElement);
  end;

  // Write all top-level package references
  for topLevelReference in dependencyGraph.Children do
    WritePackageRef(packagesElement, topLevelReference);

  //We only reach here when the graph differs from the DOM (or there was no Packages element).
  FModified := true;
end;

end.

