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

function IncludeTrailingChar(const sValue :string; const AChar : Char) : string;
var
  l : integer;
begin
  result := sValue;
  l := Length(result);
  if l > 0 then
    if result[l] <> AChar then
      result := result + AChar
end;

function ExcludeTrailingChar(const sValue :string; const AChar : Char) : string;
var
  l : integer;
begin
  result := sValue;
  l := Length(result);
  if l > 0 then
    if result[l] = AChar then
      Delete(result,l,1);
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


function TDPMProjectSettingsLoader.DoGetStringProperty(const configKey, propName, defaultValue : string) : string;
var
  tmpElement    : IXMLDOMElement;
  sParentConfig : string;
  sInherit      : string;
  bInherit      : boolean;
  sParentValue  : string;
begin
  bInherit := False;

  sInherit := '$(' + propName + ')';
  tmpElement := FXMLDoc.selectSingleNode(GetXPath(configKey, propName)) as IXMLDOMElement;
  if tmpElement <> nil then
  begin
    result := tmpElement.text;
    if not bInherit then
      if Pos(sInherit,result) > 0  then
        bInherit := True;
  end
  else
  begin
    bInherit := True; //didn't find a value so we will look at it's base config for a value
    result := '';
  end;

  if bInherit then
  begin
    if sInherit <> '' then
      result := StringReplace(Result,sInherit,'',[rfIgnoreCase]);

    sParentConfig := GetConfigParent(configKey);
    if sParentConfig <> '' then
    begin
      sParentValue := DoGetStringProperty(sParentConfig,propName, defaultValue);
      result := IncludeTrailingChar(sParentValue,';')  + result;
    end
    else
      result := StringReplace(Result,sInherit,'',[rfIgnoreCase]);
  end;
  if result = '' then
     result := defaultValue;
  result := ExcludeTrailingChar(Result,';');
end;


function TDPMProjectSettingsLoader.GetSearchPath: string;
begin
  result := GetStringProperty('DCC_UnitSearchPath', '$(DCC_UnitSearchPath)' );
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
