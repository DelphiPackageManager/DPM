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
    FPackageRefences : IDictionary<TDPMPlatform, IGraphNode>;
    FFileName : string;
    FAppType : TAppType;
    FConfigurations : IDictionary<string, IProjectConfiguration>;
    FConfigNames : IList<string>;
  protected
    function GetOutputElementName : string;

    function LoadProjectVersion : boolean;
    function LoadMainSource : boolean;
    function LoadAppType : boolean;
    function LoadProjectPlatforms : boolean;
    function LoadPackageRefences : boolean;
    function LoadConfigurations : boolean;

    function SavePackageReferences : boolean;

    function InternalLoadFromXML : boolean;
    procedure Reset;

    function GetPlatforms : TDPMPlatforms;
    function GetCompilerVersion : TCompilerVersion;
    function GetProjectVersion : string;
    function GetAppType : TAppType;
    function GetHasPackages : boolean;
    function GetProjectFile : string;
    function GetProjectConfiguration(const name : string; const platform : TDPMPlatform) : IProjectConfiguration;

    procedure SetCompiler(const value : TCompilerVersion);

    function LoadProject(const filename : string) : Boolean;
    function SaveProject(const filename : string) : Boolean;

    function GetDPMPropertyGroup : IXMLDOMElement;
    function EnsureBaseSearchPath : boolean;

    function AddSearchPaths(const platform : TDPMPlatform; const searchPaths : IList<string> ; const packageCacheLocation : string) : boolean;
    procedure UpdatePackageReferences(const dependencyGraph : IGraphNode; const platform : TDPMPlatform);
    function GetPackageReferences(const platform : TDPMPlatform) : IGraphNode;
    function GetConfigNames: IReadOnlyList<string>;
  public
    constructor Create(const logger : ILogger; const config : IConfiguration; const compilerVersion : TCompilerVersion);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  DPM.Core.Constants,
  DPM.Core.Utils.Xml,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Graph,
//  DPM.Core.Project.PackageReference,
  DPM.Core.Project.Configuration;

const
  msbuildNamespace = 'http://schemas.microsoft.com/developer/msbuild/2003';

  projectVersionXPath = '/x:Project/x:PropertyGroup/x:ProjectVersion';

  mainSourceXPath = '/x:Project/x:PropertyGroup/x:ProjectVersion';

  projectAppTypeXPath = '/x:Project/x:PropertyGroup/x:AppType';

  platformsXPath = '/x:Project/x:ProjectExtensions/x:BorlandProject/x:Platforms/x:Platform';

  projectExtensionsXPath = '/x:Project/x:ProjectExtensions';

  packageReferencesXPath = '/x:Project/x:ProjectExtensions/x:DPM/x:PackageReference';

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
    FLogger.Error('Unabled to find or create PropertyGroup for DPM in the project file');
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
  end;
  dpmSearchElement.setAttribute('Condition', condition);

  for searchPath in searchPaths do
  begin
    searchPathString := searchPathString + '$(DPM)\' + searchPath + ';'
  end;

  dpmSearchElement.text := searchPathString;



end;

constructor TProjectEditor.Create(const logger : ILogger; const config : IConfiguration; const compilerVersion : TCompilerVersion);
begin
  FLogger := logger;
  FConfig := config;
  FCompiler := compilerVersion;
  FPlatforms := [];
  FPackageRefences := TCollections.CreateDictionary<TDPMPlatform, IGraphNode>;
  FConfigurations := TCollections.CreateDictionary <string, IProjectConfiguration> ;
  FConfigNames := TCollections.CreateList<string>;
end;

function TProjectEditor.GetPackageReferences(const platform : TDPMPlatform) : IGraphNode;
begin
  result := nil;
  FPackageRefences.TryGetValue(platform,Result);
  //TODO : Should we return an empty root here?

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
  end
  else
  begin
    //remove and add at the beginning
    searchPath := StringReplace(dccSearchElement.text, '$(DPMSearch);', '', [rfReplaceAll]);
    searchPath := StringReplace(searchPath, '$(DPMSearch)', '', [rfReplaceAll]);
    searchPath := '$(DPMSearch);' + searchPath;
    dccSearchElement.text := searchPath;
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
  end
  else
    result := dpmElement.parentNode as IXMLDOMElement;

  dpmCompilerElement := result.selectSingleNode('x:DPMCompiler') as IXMLDOMElement;
  if dpmCompilerElement = nil then
  begin
    dpmCompilerElement := FProjectXML.createNode(NODE_ELEMENT, 'DPMCompiler', msbuildNamespace) as IXMLDOMElement;
    result.appendChild(dpmCompilerElement);
  end;
  dpmCompilerElement.text := CompilerToString(FCompiler);


  dpmCondition := result.selectSingleNode('x:DPMCache[@Condition != '''']') as IXMLDOMElement;
  if dpmCondition = nil then
  begin
    dpmCondition := FProjectXML.createNode(NODE_ELEMENT, 'DPMCache', msbuildNamespace) as IXMLDOMElement;
    result.appendChild(dpmCondition);
  end;
  dpmCondition.setAttribute('Condition', '''$(DPMCache)'' == ''''');
  if FConfig.IsDefaultPackageCacheLocation then
    dpmCondition.text := StringReplace(cDefaultPackageCache, '%APPDATA%', '$(APPDATA)', [rfIgnoreCase]) //
  else
  begin
    //see if we can covert this to a relative path
    dpmCondition.text := FConfig.PackageCacheLocation;
  end;

  dpmElement := result.selectSingleNode('x:DPM') as IXMLDOMElement;
  if dpmElement = nil then
  begin
    dpmElement := FProjectXML.createNode(NODE_ELEMENT, 'DPM', msbuildNamespace) as IXMLDOMElement;
    result.appendChild(dpmElement);
  end;
  dpmElement.text := '$(DPMCache)\$(DPMCompiler)\$(Platform)';

end;

function TProjectEditor.GetHasPackages : boolean;
begin
  result := FPackageRefences.Any;
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

function TProjectEditor.GetConfigNames: IReadOnlyList<string>;
begin
  result := FConfigNames as IReadOnlyList<string>;
end;

function TProjectEditor.InternalLoadFromXML : boolean;
begin
  result := LoadProjectVersion;
  result := result and LoadMainSource;
  result := result and LoadAppType;
  result := result and LoadProjectPlatforms;
  result := result and LoadConfigurations; //must be after platforms
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

  function TryGetConfigValue(configKey : string; const platform : string; const elementName : string; out value : string) : boolean;
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
          if sOutputDir = '' then
             FLogger.Debug('No output directory found for config ' + configKeys.Names[i] + '_' + sPlatform);

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

  procedure ReadPackageReferences(const parentReference : IGraphNode; const parentElement : IXMLDOMElement);
  var
    isTransitive : boolean;
    packageNodes : IXMLDOMNodeList;
    packageElement : IXMLDOMElement;
    i : integer;
    id : string;
    sVersion : string;
    version : TPackageVersion;
    error : string;
    sPlatform : string;
    platform : TDPMPlatform;
    sRange : string;
    range : TVersionRange;
    dupCheckNode : IGraphNode;
    sXPath : string;
    useSource : boolean;
    sUseSource : string;
    newNode : IGraphNode;
    rootNode : IGraphNode;
  begin
    isTransitive := parentReference <> nil;


    if isTransitive then
      sXPath := 'x:PackageReference'
    else
      sXPath := packageReferencesXPath;

    packageNodes := parentElement.selectNodes(sXPath);
    if packageNodes.length > 0 then
    begin
      for i := 0 to packageNodes.length - 1 do
      begin
        rootNode := nil;
        packageElement := packageNodes.item[i] as IXMLDOMElement;
        if packageElement.getAttributeNode('id') <> nil then
        begin
          id := packageElement.getAttribute('id');
        end
        else
        begin
          FLogger.Error('Invalid package reference detected in project, missing required [id] attribute');
          result := false;
          exit;
        end;

        if packageElement.getAttributeNode('version') <> nil then
        begin
          sVersion := packageElement.getAttribute('version');
          if not TPackageVErsion.TryParseWithError(sVersion, version, error) then
          begin
            FLogger.Error('Invalid package reference detected in project, [version] attribute is not valid');
            FLogger.Error(' ' + error);
            result := false;
            exit;
          end;
        end
        else
        begin
          FLogger.Error('Invalid package reference detected in project, missing required [version] attribute');
          result := false;
          exit;
        end;
        //if we have a parent that isn't root then we will use it's platform
        if (parentReference <> nil) and (not parentReference.IsRoot) then
          platform := parentReference.Platform
        else if packageElement.getAttributeNode('platform') <> nil then
        begin
          sPlatform := packageElement.getAttribute('platform');
          platform := StringToDPMPlatform(sPlatform);
          if platform = TDPMPlatform.UnknownPlatform then
          begin
            FLogger.Error('Invalid package reference platform value [' + sPlatform + '] in platform attribute for [' + id + ']');
            result := false;
            exit;
          end;
        end
        else
        begin
          FLogger.Error('Package reference missing platform for [' + id + ']');
          result := false;
          exit;
        end;

        //if the parent reference is using source then we must use source for transient references.
        if (parentReference <> nil) and parentReference.UseSource then
          useSource := true
        else if packageElement.getAttributeNode('useSource') <> nil then
        begin
          sUseSource := packageElement.getAttribute('useSource');
          useSource := StrToBoolDef(sUseSource, false);
        end
        else
          useSource := false;

        if not FPackageRefences.TryGetValue(platform, rootNode) then
        begin
          rootNode := TGraphNode.CreateRoot(FCompiler,platform);
          FPackageRefences[platform] := rootNode;
        end;




        //check for duplicate references
        if isTransitive then
          dupCheckNode := parentReference
        else
          dupCheckNode := rootNode;

        if dupCheckNode.FindChild(id) <> nil then
        begin
          if parentReference <> nil then
            raise Exception.Create('Duplicate package reference for package [' + id + '  ' + DPMPlatformToString(platform) + '] under [' + parentReference.Id + ']')
          else
            raise Exception.Create('Duplicate package reference for package [' + id + '  ' + DPMPlatformToString(platform) + ']');
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
          newNode  := parentReference.AddPackageChildNode(id, version, range);
          newNode.UseSource := useSource;
        end
        else
        begin
          newNode := rootNode.AddPackageChildNode(id, version, TVersionRange.Empty);
          newNode.UseSource := useSource;
        end;
        ReadPackageReferences(newNode, packageElement);
      end;
    end;
  end;

begin
  result := true;
  ReadPackageReferences(nil, FProjectXML.documentElement );

end;

function TProjectEditor.LoadProject(const filename : string) : Boolean;
begin
  result := false;
  FPackageRefences.Clear;
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
    result := FProjectXML.load(fileName);
    if not result then
    begin
      FLogger.Error('Error loading project file : ' + FProjectXML.parseError.reason);
      exit;
    end;
    FFileName := filename;
    (FProjectXML as IXMLDOMDocument2).setProperty('SelectionLanguage', 'XPath');
    (FProjectXML as IXMLDOMDocument2).setProperty('SelectionNamespaces', 'xmlns:x=''http://schemas.microsoft.com/developer/msbuild/2003''');

    result := InternalLoadFromXML;
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

procedure TProjectEditor.Reset;
begin
  FProjectXML := nil;
  FCompiler := TCompilerVersion.UnknownVersion;
  FPlatforms := [];
end;

function TProjectEditor.SavePackageReferences : boolean;
begin
  result := false;
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

  //TODO : Apply package references.


  try
    TXMLUtils.PrettyFormatXML(FProjectXML.documentElement, 4);

    FProjectXML.save(projectFileName);
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

procedure TProjectEditor.UpdatePackageReferences(const dependencyGraph : IGraphNode; const platform : TDPMPlatform);
var
  projectExtensionsElement : IXMLDOMElement;
  dpmElement : IXMLDOMElement;
  packageReferenceElements : IXMLDOMNodeList;
  i : integer;
  topLevelNode : IGraphNode;

  procedure WritePackageReference(const parentElement : IXMLDOMElement; const packageReference : IGraphNode);
  var
    packageReferenceElement : IXMLDOMElement;
    childNode : IGraphNode;
  begin
    packageReferenceElement := FProjectXML.createNode(NODE_ELEMENT, 'PackageReference', msbuildNamespace) as IXMLDOMElement;
    packageReferenceElement.setAttribute('id', packageReference.Id);
    packageReferenceElement.setAttribute('platform', DPMPlatformToBDString(packageReference.Platform));
    packageReferenceElement.setAttribute('version', packageReference.Version.ToStringNoMeta);
    if not packageReference.SelectedOn.IsEmpty then
      packageReferenceElement.setAttribute('range', packageReference.SelectedOn.ToString);
    if packageReference.UseSource then
      packageReferenceElement.setAttribute('useSource', 'true');
    parentElement.appendChild(packageReferenceElement);
    if packageReference.HasChildren then
    begin
      for childNode in packageReference.ChildNodes do
        WritePackageReference(packageReferenceElement, childNode);
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
  end
  else
  begin
    //remove existing nodes, we'll rewrite them below.
    packageReferenceElements := dpmElement.selectNodes('x:PackageReference[@platform="' + DPMPlatformToString(platform) + '"]');
    for i := 0 to packageReferenceElements.length - 1 do
      dpmElement.removeChild(packageReferenceElements.item[i]);
  end;

  for topLevelNode in dependencyGraph.ChildNodes do
    WritePackageReference(dpmElement, topLevelNode);

end;

end.

