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

unit DPM.Core.Configuration.Classes;

interface

uses
  JsonDataObjects,
  VSoft.YAML,
  Spring.Cryptography,
  DPM.Core.Types,
  DPM.Core.Sources.Types,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  Spring.Collections;

type

  TSourceConfig = class(TInterfacedObject, ISourceConfig)
  private
    FEnabled : Boolean;
    FFileName : string;
    FName : string;
    FPassword : string;
    FUserName : string;
    FSource : string;
    FLogger : ILogger;
    FCrypt : ISymmetricAlgorithm;
    FSourceType : TSourceType;
  protected
    function GetIsEnabled : Boolean;
    function GetFileName : string;
    function GetName : string;
    function GetPassword : string;
    function GetUserName : string;
    function GetSource : string;
    function GetIsHttp : Boolean;
    function GetSourceType : TSourceType;
    //TODO : Move these to a utils class
    function EncryptString(const value : string) : string;
    function DecryptString(const value : string) : string;
    function CreateCrypt : ISymmetricAlgorithm;
    procedure SetIsEnabled(const value : Boolean);
    procedure SetName(const value : string);
    procedure SetPassword(const value : string);
    procedure SetUserName(const value : string);
    procedure SetSource(const value : string);
    procedure SetSourceType(const value : TSourceType);


    function LoadFromYAML(const yamlObj : IYAMLValue) : boolean;

    function SaveToYAML(const parentObj : IYAMLValue) : boolean;

  public
    constructor Create(const logger : ILogger); overload;
    constructor Create(const name : string; const source : string; const sourceType : TSourceType; const userName : string; const password : string; const enabled : boolean); overload;
  end;

  TTrustedPublisherConfig = class(TInterfacedObject, ITrustedPublisherConfig)
  private
    FName : string;
    FSpki : string;
  protected
    function GetName : string;
    function GetSpki : string;
    procedure SetName(const value : string);
    procedure SetSpki(const value : string);
    function LoadFromYAML(const yamlObj : IYAMLValue) : boolean;
    function SaveToYAML(const parentObj : IYAMLValue) : boolean;
  end;

  TTrustedRepositoryConfig = class(TInterfacedObject, ITrustedRepositoryConfig)
  private
    FUrl : string;
    FSpki : string;
  protected
    function GetUrl : string;
    function GetSpki : string;
    procedure SetUrl(const value : string);
    procedure SetSpki(const value : string);
    function LoadFromYAML(const yamlObj : IYAMLValue) : boolean;
    function SaveToYAML(const parentObj : IYAMLValue) : boolean;
  end;

  TSigningConfig = class(TInterfacedObject, ISigningConfig)
  private
    FValidationMode : string;
    FAuthorDowngradePolicy : string;
    FAllowKeyCompromiseOverride : boolean;
    FTrustedPublishers : IList<ITrustedPublisherConfig>;
    FTrustedRepositories : IList<ITrustedRepositoryConfig>;
  protected
    function GetValidationMode : string;
    procedure SetValidationMode(const value : string);
    function GetAuthorDowngradePolicy : string;
    procedure SetAuthorDowngradePolicy(const value : string);
    function GetAllowKeyCompromiseOverride : boolean;
    procedure SetAllowKeyCompromiseOverride(const value : boolean);
    function GetTrustedPublishers : IList<ITrustedPublisherConfig>;
    function GetTrustedRepositories : IList<ITrustedRepositoryConfig>;
    function LoadFromYAML(const yamlObj : IYAMLValue) : boolean;
    function SaveToYAML(const parentObj : IYAMLValue) : boolean;
  public
    constructor Create;
  end;

  TConfiguration = class(TInterfacedObject, IConfiguration, IConfigurationLoadSave)
  private
    FSources : IList<ISourceConfig>;
    FPackageCacheLocation : string;
    FFileName : string;
    FAuthor : string;
    FRegistryRefreshIntervalMinutes : integer;
    FSigning : ISigningConfig;
    FLogger : ILogger;
  protected
    function GetFileName : string;
    procedure SetFileName(const value : string);
    function GetPackageCacheLocation : string;
    function GetSources : IList<ISourceConfig>;
    procedure SetPackageCacheLocation(const value : string);
    function GetIsDefaultPackageCacheLocation : Boolean;
    procedure AddDefaultSources;
    function GetSourceByName(const name : string) : ISourceConfig;
    function GetAuthor : string;
    procedure SetAuthor(const value : string);
    function GetRegistryRefreshIntervalMinutes : integer;
    procedure SetRegistryRefreshIntervalMinutes(const value : integer);
    function GetSigning : ISigningConfig;

    function LoadFromFile(const fileName : string) : boolean;
    function SaveToFile(const fileName : string) : boolean;

    function LoadFromYAML(const yamlObj : IYAMLValue) : boolean;

    function SaveToYAML(const parentObj : IYAMLValue) : boolean;

  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  System.SysUtils,
  Spring.Cryptography.DES,
  DPM.Core.Constants,
  DPM.Core.Utils.System,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.Enum,
  VSoft.Uri;

{ TSourceConfig }

constructor TSourceConfig.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
  FEnabled := true;
  FCrypt := CreateCrypt;
  FSourceType := TSourceType.Folder;
end;

//NOTE : DO NOT CHANGE THIS KEY - YOU WILL BREAK THINGS FOR EXISTING SOURCES!
const
  key : array[0..23] of Byte = ($11, $03, $45, $6, $8A, $8B, $DD, $EF, $EE,
    $0C, $1A, $38, $26, $41, $32, $A0, $89, $AB, $CD, $EF, $01, $23, $45, $67);

constructor TSourceConfig.Create(const name, source : string; const sourceType : TSourceType; const userName, password : string; const enabled : boolean);
begin
  FName := name;
  FSource := source;
  FSourceType := sourceType;
  FUserName := userName;
  FPassword := password;
  FEnabled := enabled;
end;

function TSourceConfig.CreateCrypt : ISymmetricAlgorithm;
begin
  result := TTripleDES.Create;
  result.CipherMode := TCipherMode.ECB;
  result.PaddingMode := TPaddingMode.PKCS7;
  result.Key := TBuffer.Create(key);
end;

function TSourceConfig.DecryptString(const value : string) : string;
var
  input : TBuffer;
  output : TBuffer;
begin
  input := TBuffer.FromHexString(value);
  output := FCrypt.Decrypt(input);
  result := TEncoding.Unicode.GetString(output.AsBytes);
end;

function TSourceConfig.EncryptString(const value : string) : string;
var
  input : TBuffer;
  output : TBuffer;
begin
  input := TBuffer.Create(value);
  output := FCrypt.Encrypt(input);
  result := output.ToHexString;
end;


function TSourceConfig.GetIsEnabled : Boolean;
begin
  result := FEnabled;
end;

function TSourceConfig.GetFileName : string;
begin
  result := FFileName;
end;

function TSourceConfig.GetIsHttp : Boolean;
begin
  result := TStringUtils.StartsWith(LowerCase(FSource), 'http://') or TStringUtils.StartsWith(LowerCase(FSource), 'https://')
end;

function TSourceConfig.GetName : string;
begin
  result := FName;
end;

function TSourceConfig.GetPassword : string;
begin
  result := FPassword;
end;

function TSourceConfig.GetUserName : string;
begin
  result := FUserName;
end;

function TSourceConfig.GetSource : string;
begin
  result := FSource;
end;

function TSourceConfig.GetSourceType : TSourceType;
begin
  result := FSourceType;
end;


function TSourceConfig.LoadFromYAML(const yamlObj: IYAMLValue): boolean;
var
  obj : IYAMLMapping;
  srcType : string;
  uri : IUri;
begin
  result := true;
  obj := yamlObj.AsMapping;

  FName := obj.S['name'];
  FSource := obj.S['source'];
  FUserName := obj.S['userName'];
  FPassword := obj.S['password'];
  srcType := obj.S['type'];

  if FPassword <> '' then
    FPassword := DecryptString(FPassword);

  if obj.Contains('enabled') then
    FEnabled := obj.B['enabled'];



  if srcType <> '' then
    FSourceType := TEnumUtils.StringtoEnum<TSourceType>(srcType)
  else
    FSourceType := TSourceType.Folder;

  //source type was added later so we can differenciate between
  //remote source types like github and https servers

  if FSourceType = TSourceType.Folder then
  begin
    //it might not have been set before, so we need to figure it out.
    if IsGitRegistryUri(FSource) then
      FSourceType := TSourceType.GitRegistry
    else if TUriFactory.TryParse(FSource, false, uri) then
    begin
      if (uri.Scheme <> 'file') then
      begin
        if (uri.Scheme = 'http') or (uri.Scheme = 'https') then
            FSourceType := TSourceType.DPMServer
        else
          raise EArgumentOutOfRangeException.Create('Invalid Source uri scheme - only https or file supported. ');
      end;
    end;
  end;

end;


function TSourceConfig.SaveToYAML(const parentObj: IYAMLValue): boolean;
var
  sourceObj : IYAMLMapping;
begin
  sourceObj := parentObj.AsSequence.AddMapping;
  sourceObj.S['name'] := FName;
  sourceObj.S['source'] := FSource;
  if FUserName <> '' then
    sourceObj.S['userName'] := FUserName;

  sourceObj.S['type'] := TEnumUtils.EnumToString<TSourceType>(FSourceType);

  if FPassword <> '' then
    sourceObj.S['password'] := EncryptString(FPassword);

  sourceObj.B['enabled'] := FEnabled;
  result := true;

end;

procedure TSourceConfig.SetIsEnabled(const value : Boolean);
begin
  FEnabled := value;
end;

procedure TSourceConfig.SetName(const value : string);
begin
  FName := value;
end;

procedure TSourceConfig.SetPassword(const value : string);
begin
  FPassword := value;
end;

procedure TSourceConfig.SetUserName(const value : string);
begin
  FUserName := value;
end;

procedure TSourceConfig.SetSource(const value : string);
begin
  FSource := value;
end;

procedure TSourceConfig.SetSourceType(const value : TSourceType);
begin
  FSourceType := value;
end;

{ TTrustedPublisherConfig }

function TTrustedPublisherConfig.GetName : string;
begin result := FName; end;

function TTrustedPublisherConfig.GetSpki : string;
begin result := FSpki; end;

procedure TTrustedPublisherConfig.SetName(const value : string);
begin FName := value; end;

procedure TTrustedPublisherConfig.SetSpki(const value : string);
begin FSpki := value; end;

function TTrustedPublisherConfig.LoadFromYAML(const yamlObj : IYAMLValue) : boolean;
var
  obj : IYAMLMapping;
begin
  if not yamlObj.IsMapping then
    exit(false);
  obj := yamlObj.AsMapping;
  FName := obj.S['name'];
  FSpki := obj.S['spki'];
  result := true;
end;

function TTrustedPublisherConfig.SaveToYAML(const parentObj : IYAMLValue) : boolean;
var
  item : IYAMLMapping;
begin
  item := parentObj.AsSequence.AddMapping;
  item.S['name'] := FName;
  item.S['spki'] := FSpki;
  result := true;
end;

{ TTrustedRepositoryConfig }

function TTrustedRepositoryConfig.GetUrl : string;
begin result := FUrl; end;

function TTrustedRepositoryConfig.GetSpki : string;
begin result := FSpki; end;

procedure TTrustedRepositoryConfig.SetUrl(const value : string);
begin FUrl := value; end;

procedure TTrustedRepositoryConfig.SetSpki(const value : string);
begin FSpki := value; end;

function TTrustedRepositoryConfig.LoadFromYAML(const yamlObj : IYAMLValue) : boolean;
var
  obj : IYAMLMapping;
begin
  if not yamlObj.IsMapping then
    exit(false);
  obj := yamlObj.AsMapping;
  FUrl := obj.S['url'];
  FSpki := obj.S['spki'];
  result := true;
end;

function TTrustedRepositoryConfig.SaveToYAML(const parentObj : IYAMLValue) : boolean;
var
  item : IYAMLMapping;
begin
  item := parentObj.AsSequence.AddMapping;
  item.S['url'] := FUrl;
  item.S['spki'] := FSpki;
  result := true;
end;

{ TSigningConfig }

constructor TSigningConfig.Create;
begin
  inherited Create;
  // Architecture doc defaults — see plan §1.11.
  FValidationMode := 'permissive';
  FAuthorDowngradePolicy := 'prompt';
  FAllowKeyCompromiseOverride := false;
  FTrustedPublishers := TCollections.CreateList<ITrustedPublisherConfig>;
  FTrustedRepositories := TCollections.CreateList<ITrustedRepositoryConfig>;
end;

function TSigningConfig.GetValidationMode : string;
begin result := FValidationMode; end;

procedure TSigningConfig.SetValidationMode(const value : string);
begin FValidationMode := value; end;

function TSigningConfig.GetAuthorDowngradePolicy : string;
begin result := FAuthorDowngradePolicy; end;

procedure TSigningConfig.SetAuthorDowngradePolicy(const value : string);
begin FAuthorDowngradePolicy := value; end;

function TSigningConfig.GetAllowKeyCompromiseOverride : boolean;
begin result := FAllowKeyCompromiseOverride; end;

procedure TSigningConfig.SetAllowKeyCompromiseOverride(const value : boolean);
begin FAllowKeyCompromiseOverride := value; end;

function TSigningConfig.GetTrustedPublishers : IList<ITrustedPublisherConfig>;
begin result := FTrustedPublishers; end;

function TSigningConfig.GetTrustedRepositories : IList<ITrustedRepositoryConfig>;
begin result := FTrustedRepositories; end;

function TSigningConfig.LoadFromYAML(const yamlObj : IYAMLValue) : boolean;
var
  obj : IYAMLMapping;
  publishers : IYAMLSequence;
  repositories : IYAMLSequence;
  i : integer;
  publisher : ITrustedPublisherConfig;
  repository : ITrustedRepositoryConfig;
begin
  if not yamlObj.IsMapping then
    exit(false);
  obj := yamlObj.AsMapping;

  if obj.Contains('validationMode') then
    FValidationMode := obj.S['validationMode'];
  if obj.Contains('authorDowngradePolicy') then
    FAuthorDowngradePolicy := obj.S['authorDowngradePolicy'];
  if obj.Contains('allowKeyCompromiseOverride') then
    FAllowKeyCompromiseOverride := obj.B['allowKeyCompromiseOverride'];

  FTrustedPublishers.Clear;
  if obj.Contains('trustedPublishers') then
  begin
    publishers := obj.A['trustedPublishers'];
    for i := 0 to publishers.Count - 1 do
    begin
      publisher := TTrustedPublisherConfig.Create;
      if publisher.LoadFromYAML(publishers[i]) then
        FTrustedPublishers.Add(publisher);
    end;
  end;

  FTrustedRepositories.Clear;
  if obj.Contains('trustedRepositories') then
  begin
    repositories := obj.A['trustedRepositories'];
    for i := 0 to repositories.Count - 1 do
    begin
      repository := TTrustedRepositoryConfig.Create;
      if repository.LoadFromYAML(repositories[i]) then
        FTrustedRepositories.Add(repository);
    end;
  end;

  result := true;
end;

function TSigningConfig.SaveToYAML(const parentObj : IYAMLValue) : boolean;
var
  obj : IYAMLMapping;
  publishers : IYAMLSequence;
  repositories : IYAMLSequence;
  i : integer;
begin
  obj := parentObj.AsMapping.AddOrSetMapping('signing');
  obj.S['validationMode'] := FValidationMode;
  obj.S['authorDowngradePolicy'] := FAuthorDowngradePolicy;
  if FAllowKeyCompromiseOverride then
    obj.B['allowKeyCompromiseOverride'] := true;

  publishers := obj.A['trustedPublishers'];
  for i := 0 to FTrustedPublishers.Count - 1 do
    FTrustedPublishers[i].SaveToYAML(publishers);

  repositories := obj.A['trustedRepositories'];
  for i := 0 to FTrustedRepositories.Count - 1 do
    FTrustedRepositories[i].SaveToYAML(repositories);

  result := true;
end;

{ TConfiguration }

procedure TConfiguration.AddDefaultSources;
var
  source : ISourceConfig;
begin
  source := TSourceConfig.Create(cDefaultSourceName, cDefaultSourceUrl, TSourceType.DPMServer, '', '', true);
  FSources.Add(source);
end;

constructor TConfiguration.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
  FSources := TCollections.CreateList<ISourceConfig>;
  FSigning := TSigningConfig.Create;
  FRegistryRefreshIntervalMinutes := cDefaultRegistryRefreshMinutes;
end;

function TConfiguration.GetSigning : ISigningConfig;
begin
  result := FSigning;
end;

function TConfiguration.GetAuthor : string;
begin
  result := FAuthor;
end;

procedure TConfiguration.SetAuthor(const value : string);
begin
  FAuthor := value;
end;

function TConfiguration.GetRegistryRefreshIntervalMinutes : integer;
begin
  result := FRegistryRefreshIntervalMinutes;
end;

procedure TConfiguration.SetRegistryRefreshIntervalMinutes(const value : integer);
begin
  FRegistryRefreshIntervalMinutes := value;
end;

function TConfiguration.GetFileName : string;
begin
  result := FFileName;
end;

function TConfiguration.GetIsDefaultPackageCacheLocation : Boolean;
begin
  result := SameText(FPackageCacheLocation, cDefaultPackageCache) or
  SameText(FPackageCacheLocation, TSystemUtils.ExpandEnvironmentStrings(cDefaultPackageCache));
end;

function TConfiguration.GetPackageCacheLocation : string;
begin
  result := FPackageCacheLocation;
end;

function TConfiguration.GetSourceByName(const name: string): ISourceConfig;
var
  sourceConfig : ISourceConfig;
begin
  result := nil;
  for sourceConfig in FSources do
  begin
    if SameText(sourceConfig.Name, name) then
      exit(sourceConfig);
  end;
end;

function TConfiguration.GetSources : IList<ISourceConfig>;
begin
  result := FSources;
end;

function TConfiguration.LoadFromFile(const fileName : string) : boolean;
var
  doc : IYAMLDocument;
  defSource : ISourceConfig;
begin
  result := false;

  try
    doc := TYAML.LoadFromFile(fileName);
    Result := LoadFromYAML(doc.Root);
      if not FSources.Any then
        AddDefaultSources
      else
      begin
        defSource := FSources.FirstOrDefault(
          function (const item : ISourceConfig) : boolean
          begin
            result := SameText(cDefaultSourceName, item.Name);
          end);
        if defSource <> nil then
        begin
          if not SameText(defSource.Source, cDefaultSourceUrl) then
            defSource.Source := cDefaultSourceUrl;
        end;
      end;

      FFileName := fileName;
  except
    on e : Exception do
    begin
      FLogger.Error('Exception while loading config file [' + fileName + ']' + #13#10 + e.Message);
      exit;
    end;
  end;
end;


function TConfiguration.LoadFromYAML(const yamlObj: IYAMLValue): boolean;
var
  root : IYAMLMapping;
  sources : IYAMLSequence;
  source : ISourceConfig;
  bResult : boolean;
  i : integer;
begin
  result := true;
  root := yamlObj.AsMapping;
  FPackageCacheLocation := root.S['packageCacheLocation'];
  if root.Contains('author') then
    FAuthor := root.S['author'];
  if root.Contains('registryRefreshIntervalMinutes') then
    FRegistryRefreshIntervalMinutes := root.I['registryRefreshIntervalMinutes'];
  if root.Contains('signing') then
    FSigning.LoadFromYAML(root.Values['signing']);
  sources := root.A['packageSources'];
  bResult := true;
  for i := 0 to sources.Count - 1 do
  begin
    source := TSourceConfig.Create(FLogger);
    bResult := source.LoadFromYAML(sources[i]);
    if bResult then
    begin
      if not FSources.Any(
        function(const item : ISourceConfig) : boolean
        begin
          result := SameText(item.Name, source.Name);
        end) then
        begin
          FSources.Add(source);
        end;
    end;
  end;

  if not FSources.Any(
    function(const item : ISourceConfig) : boolean
    begin
      result := SameText(item.Name, 'DPM')
    end) then
    begin
      AddDefaultSources;
    end;
    result := result and bResult;


end;

function TConfiguration.SaveToFile(const fileName : string) : boolean;
var
  sFileName : string;
  yamlDoc : IYAMLDocument;
begin

  if fileName <> '' then
    sFileName := fileName
  else
    sFileName := FFileName;

  if sFileName = '' then
  begin
    FLogger.Error('No filename set for config file, unable to save');
    exit(false);
  end;

  yamlDoc := TYAML.CreateMapping;
  try
    result := SaveToYAML(yamlDoc.Root);
    if result then
      TYAML.WriteToFile(yamlDoc, sFileName);
    FFileName := sFileName;
  except
    on e : Exception do
    begin
      FLogger.Error('Exception while saving config to file [' + sFileName + ']' + #13#10 + e.Message);
      result := false;
    end;
  end;
end;



function TConfiguration.SaveToYAML(const parentObj: IYAMLValue): boolean;
var
  i : integer;
  root : IYAMLMapping;
  sources : IYAMLSequence;
begin
  result := true;
  root := parentObj.AsMapping;
  root.S['packageCacheLocation'] := FPackageCacheLocation;
  if FAuthor <> '' then
    root.S['author'] := FAuthor;
  root.I['registryRefreshIntervalMinutes'] := FRegistryRefreshIntervalMinutes;
  FSigning.SaveToYAML(parentObj);
  sources := root.A['packageSources'];
  for i := 0 to FSources.Count - 1 do
    result := FSources[i].SaveToYAML(sources) and result;

end;

procedure TConfiguration.SetFileName(const value : string);
begin
  FFileName := value;
end;

procedure TConfiguration.SetPackageCacheLocation(const value : string);
begin
  FPackageCacheLocation := value;
end;

end.


