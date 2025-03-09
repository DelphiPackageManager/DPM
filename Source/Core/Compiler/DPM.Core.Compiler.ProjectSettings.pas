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
  System.SysUtils;

const
  PropertyXPathFormatStr = '/def:Project/def:PropertyGroup[@Condition="''$(%s)''!=''''"]/def:%s';


{ TDOMProjectSettingsLoader }

constructor TDPMProjectSettingsLoader.Create(const logger : ILogger; const projectFile, configName: string; const platform : TDPMPlatform);
begin
  FLogger := logger;
  FConfigName := configName;
  FXMLDoc := CoDOMDocument60.Create;
  FLogger.Debug('Loading project xml');
  if not FXMLDoc.load(projectFile) then
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


function TDPMProjectSettingsLoader.GetConfigParent(const key: string): string;
begin
  if key = 'Base' then
    exit('');
  result := FConfigParents.Values[key];
  if result = '' then
  begin
    //if we didn't find a parent then try and remove the platform
    result := StringReplace(key,'_' + FPlatform,'',[rfIgnoreCase]);
  end;
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
end;


function TDPMProjectSettingsLoader.GetStringProperty(const propName,  defaultValue: string): string;
var
  sConfigKey : string;
begin
  sConfigKey := FConfigKeys.Values[FConfigName];
  if sConfigKey <> '' then
    result := DoGetStringProperty(sConfigKey, propName, defaultValue)
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
  //Avert your eyes, ugly code ahead to deal with how config inheritance works in
  //dproj files.. sometimes the intermediate configs are not present in the dproj
  //so we have to fudge things to make the tree correct.
  //TODO : find a neater way to do this.
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

        //This is a hack to deal with Platforms.. not enough info in the dproj to walk the inheritance tree fully
        if (sKey <> 'Base') and (sParent <> '') then
          FConfigParents.Add(sKey + '_' + FPlatform + '=' + sParent + '_' + FPlatform);
      end;
    end;
  end;
  FLogger.Debug('ConfigKeys.Count : ' + IntToStr(FConfigKeys.Count));
  for i := 0 to FConfigKeys.Count - 1 do
  begin
    sKey := FConfigKeys.ValueFromIndex[i];
    //get the parent that we retrieved from the project file
    sParent := GetConfigParent(sKey);
    //remap the parents to inject the platform parents
    FConfigParents.Add(sKey + '_' + FPlatform + '=' + sKey);
    if sParent <> '' then
    begin
      FConfigParents.Add(sKey + '=' + sParent + '_' + FPlatform);
      FConfigParents.Add(sParent + '_' + FPlatform + '=' + sParent);
    end;
  end;



end;

end.
