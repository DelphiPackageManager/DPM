unit DPM.Core.Compiler.ProjectSettings;

interface

uses
  System.Classes,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.MSXML;

type
  IProjectSettingsLoader = interface
  ['{83E9EEA4-02E5-4D13-9C13-21D7F7219F4A}']
    function GetSearchPath : string;
    //The value of propName for the loader's (config, platform), walking up the same chain of
    //property groups msbuild would evaluate. Returns defaultValue when no group sets it.
    function GetStringProperty(const propName, defaultValue : string) : string;
  end;

  TDPMProjectSettingsLoader = class(TInterfacedObject, IProjectSettingsLoader)
  private
    FLogger : ILogger;
    FConfigKeys : TStringList;
    FConfigParents : TStringList;
    FConfigName : string;
    FXMLDoc : IXMLDOMDocument;
    FPlatform : string;
    function GetConfigParent(const key: string): string;
  protected
    procedure LoadConfigs;
    function GetStringProperty(const propName, defaultValue: string): string;
    function DoGetStringProperty(const configKey, propName, defaultValue: string): string;
    function GetSearchPath : string;
  public
    constructor Create(const logger : ILogger; const projectFile : string; const configName : string; const platform : TDPMPlatform);
    destructor Destroy;override;
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils,
  DPM.Core.Utils.XML;

const
  PropertyXPathFormatStr = '/def:Project/def:PropertyGroup[@Condition="''$(%s)''!=''''"]/def:%s';


{ TDOMProjectSettingsLoader }

constructor TDPMProjectSettingsLoader.Create(const logger : ILogger; const projectFile, configName: string; const platform : TDPMPlatform);
begin
  FLogger := logger;
  FConfigName := configName;
  FXMLDoc := CoDOMDocument60.Create;
  FLogger.Debug('Loading project xml');


  if not TXMLUtils.LoadXMLFromFile(FXMLDoc, projectFile) then
    raise Exception.Create('Error loading dproj [' + projectFile + '] : '  + FXMLDoc.parseError.reason);
  (FXMLDoc as IXMLDOMDocument2).setProperty('SelectionLanguage', 'XPath');
  (FXMLDoc as IXMLDOMDocument2).setProperty('SelectionNamespaces', 'xmlns:def=''http://schemas.microsoft.com/developer/msbuild/2003''');

  FConfigKeys := TStringList.Create;
  FConfigParents := TStringList.Create;
  FPlatform := DPMPlatformToBDString(platform);

  FLogger.Debug('Loading configs');
  LoadConfigs;
end;

destructor TDPMProjectSettingsLoader.Destroy;
begin
  FConfigKeys.Free;
  FConfigParents.Free;
  FXMLDoc := nil;
  inherited;
end;

function GetXPath(const sPath, PropertyName: string): string;
begin
  result := Format(PropertyXPathFormatStr,[sPath,PropertyName]);
end;

//Returns the settings group msbuild evaluates immediately before the one for key - ie where an
//inherited $(PropName) in key's value comes from. For Config=Release (Cfg_1) Platform=Win64 the
//groups apply in document order Base, Base_Win64, Cfg_1, Cfg_1_Win64, so walking up from the
//most specific :
//
//  Cfg_1_Win64 -> Cfg_1 -> Base_Win64 -> Base
//
//A config that inherits from another config (Cfg_4 -> Cfg_2 -> Base) gets its parent's all
//platforms group but NOT the parent's platform group - the IDE writes the child's activator after
//the parent's platform activators, so msbuild never switches those on :
//
//  Cfg_4_Win64 -> Cfg_4 -> Cfg_2 -> Base_Win64 -> Base
function TDPMProjectSettingsLoader.GetConfigParent(const key: string): string;
var
  platformSuffix : string;
begin
  result := '';
  if SameText(key, 'Base') then
    exit;
  platformSuffix := '_' + FPlatform;
  if EndsText(platformSuffix, key) then
  begin
    result := Copy(key, 1, Length(key) - Length(platformSuffix));
    exit;
  end;
  result := FConfigParents.Values[key];
  //a config that declares no parent hangs off Base, the same as one that says so.
  if (result = '') or SameText(result, 'Base') then
    result := 'Base' + platformSuffix;
end;


//a ; separated list without its empty entries - what substituting an empty inherited value leaves.
function TidyList(const value : string) : string;
var
  parts : TStringList;
  i : integer;
begin
  result := '';
  parts := TStringList.Create;
  try
    parts.StrictDelimiter := true;
    parts.Delimiter := ';';
    parts.DelimitedText := value;
    for i := 0 to parts.Count - 1 do
    begin
      if Trim(parts[i]) = '' then
        continue;
      if result <> '' then
        result := result + ';';
      result := result + parts[i];
    end;
  finally
    parts.Free;
  end;
end;

//The value msbuild ends up with for propName in configKey's group : the group's own, with each $(propName) in it
//replaced by what the group before it ends up with - where it appears, so ..\src;$(X) and $(X);..\src keep their
//order. A group that doesn't set it has the value of the one before it.
function TDPMProjectSettingsLoader.DoGetStringProperty(const configKey, propName, defaultValue : string) : string;
var
  tmpElement    : IXMLDOMElement;
  sParentConfig : string;
  sInherit      : string;
  sParentValue  : string;
begin
  sInherit := '$(' + propName + ')';
  sParentConfig := GetConfigParent(configKey);
  tmpElement := FXMLDoc.selectSingleNode(GetXPath(configKey, propName)) as IXMLDOMElement;
  if tmpElement = nil then
  begin
    if sParentConfig <> '' then
      result := DoGetStringProperty(sParentConfig, propName, defaultValue)
    else
      result := defaultValue;
    exit;
  end;
  result := tmpElement.text;
  if ContainsText(result, sInherit) then
  begin
    if sParentConfig <> '' then
      sParentValue := DoGetStringProperty(sParentConfig, propName, defaultValue)
    else
      sParentValue := defaultValue;
    result := StringReplace(result, sInherit, sParentValue, [rfReplaceAll, rfIgnoreCase]);
  end;
  result := TidyList(result);
end;


function TDPMProjectSettingsLoader.GetSearchPath: string;
begin
  //nothing set anywhere is nothing - not a $(DCC_UnitSearchPath) the command line can't expand.
  result := GetStringProperty('DCC_UnitSearchPath', '');
  //The caller hands this to msbuild as a global (command line) property, and property references
  //do not survive that - $(Platform) and $(Config) reach the compiler as nothing at all, turning
  //..\Source\$(Platform) into ..\Source. We know both values, so resolve them here. msbuild
  //property names are not case sensitive, the IDE itself writes $(PLATFORM) in library paths.
  result := StringReplace(result, '$(Platform)', FPlatform, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '$(Config)', FConfigName, [rfReplaceAll, rfIgnoreCase]);
end;


function TDPMProjectSettingsLoader.GetStringProperty(const propName,  defaultValue: string): string;
var
  sConfigKey : string;
begin
  sConfigKey := FConfigKeys.Values[FConfigName];
  if sConfigKey <> '' then
    //start at the most specific group - the config's own platform group - and walk up from there,
    //otherwise anything set per platform (Base_<Platform>, Cfg_N_<Platform>) is never seen.
    result := DoGetStringProperty(sConfigKey + '_' + FPlatform, propName, defaultValue)
  else
    result := '';
end;

procedure TDPMProjectSettingsLoader.LoadConfigs;
var
  configs       : IXMLDOMNodeList;
  tmpElement    : IXMLDOMElement;
  keyElement    : IXMLDOMElement;
  parentElement : IXMLDOMElement;
  i             : integer;
  sName         : string;
  sKey          : string;
  sParent       : string;
begin
  //Only the declared config -> parent config links are recorded here. The per platform groups
  //are not listed as BuildConfiguration items at all, GetConfigParent slots them into the chain.
  FLogger.Debug('Loading project configs');
  configs := FXMLDoc.selectNodes('/def:Project/def:ItemGroup/def:BuildConfiguration');
  FLogger.Debug('configs.length : ' + IntToStr(configs.length));

  if configs.length > 0 then
  begin
    for i := 0 to configs.length - 1 do
    begin
      sName   := '';
      sKey    := '';
      sParent := '';
      tmpElement := configs.item[i] as IXMLDOMElement;
      if tmpElement <> nil then
      begin
        sName := tmpElement.getAttribute('Include');
        keyElement := tmpElement.selectSingleNode('def:Key') as IXMLDOMElement;
        if keyElement <> nil then
          sKey := keyElement.text;
        parentElement := tmpElement.selectSingleNode('def:CfgParent') as IXMLDOMElement;
        if parentElement <> nil then
          sParent := parentElement.text;
        FConfigKeys.Add(sName + '=' + sKey);
        FConfigParents.Add(sKey + '=' + sParent);
      end;
    end;
  end;
  FLogger.Debug('ConfigKeys.Count : ' + IntToStr(FConfigKeys.Count));
end;

end.
