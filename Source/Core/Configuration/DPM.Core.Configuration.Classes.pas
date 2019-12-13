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

unit DPM.Core.Configuration.Classes;

interface

uses
  JsonDataObjects,
  Spring.Cryptography,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  Spring.Collections;

type

  TSourceConfig = class(TInterfacedObject, ISourceConfig)
  private
    FApiKey: string;
    FEnabled: Boolean;
    FFileName: string;
    FName: string;
    FPassword: string;
    FUserName: string;
    FSource : string;
    FLogger : ILogger;
    FCrypt  : ISymmetricAlgorithm;
  protected
    function GetApiKey: string;
    function GetIsEnabled: Boolean;
    function GetFileName: string;
    function GetName: string;
    function GetPassword: string;
    function GetUserName: string;
    function GetSource: string;
    function GetIsHttp: Boolean;
    //TODO : Move these to a utils class
    function EncryptString(const value : string) : string;
    function DecryptString(const value : string) : string;
    function CreateCrypt : ISymmetricAlgorithm;
    procedure SetApiKey(const value: string);
    procedure SetIsEnabled(const value: Boolean);
    procedure SetName(const value: string);
    procedure SetPassword(const value: string);
    procedure SetUserName(const value: string);
    procedure SetSource(const value: string);

    function LoadFromJson(const jsonObj : TJsonObject) : boolean;
    function SaveToJson(const parentObj : TJsonObject) : boolean;
  public
    constructor Create(const logger: ILogger);
  end;

  TConfiguration = class(TInterfacedObject, IConfiguration,IConfigurationLoadSave)
  private
    FSources : IList<ISourceConfig>;
    FPackageCacheLocation : string;
    FFileName : string;
    FLogger : ILogger;
  protected
    function GetFileName : string;
    procedure SetFileName(const value : string);
    function GetPackageCacheLocation: string;
    function GetSources: IList<ISourceConfig>;
    procedure SetPackageCacheLocation(const value : string);
    function GetIsDefaultPackageCacheLocation: Boolean;

    function LoadFromFile(const fileName : string) : boolean;
    function SaveToFile(const fileName : string) : boolean;

    function LoadFromJson(const jsonObj : TJsonObject) : boolean;
    function SaveToJson(const parentObj : TJsonObject) : boolean;
  public
    constructor Create(const logger: ILogger);
  end;

  //TODO : should be checking for invalid xml chars when saving!

implementation

uses
  System.SysUtils,
  Spring.Cryptography.DES,
  DPM.Core.Constants,
  DPM.Core.Utils.System,
  DPM.Core.Utils.Strings;

{ TSourceConfig }

constructor TSourceConfig.Create(const logger: ILogger);
begin
  inherited Create;
  FLogger := logger;
  FEnabled := true;
  FCrypt := CreateCrypt;
end;

//NOTE : DO NOT CHANGE THIS KEY - YOU WILL BREAK THINGS FOR EXISTING SOURCES!
const
  key: array[0..23] of Byte = ($11, $03, $45, $6, $8A, $8B, $DD, $EF, $EE,
    $0C, $1A, $38, $26, $41, $32, $A0, $89, $AB, $CD, $EF, $01, $23, $45, $67);

function TSourceConfig.CreateCrypt: ISymmetricAlgorithm;
begin
  result := TTripleDES.Create;
  result.CipherMode := TCipherMode.ECB;
  result.PaddingMode := TPaddingMode.PKCS7;
  result.Key := TBuffer.Create(key);
end;

function TSourceConfig.DecryptString(const value: string): string;
var
  input : TBuffer;
  output : TBuffer;
begin
  input := TBuffer.FromHexString(value);
  output := FCrypt.Decrypt(input);
  result := TEncoding.Unicode.GetString(output.AsBytes);
end;

function TSourceConfig.EncryptString(const value: string): string;
var
  input : TBuffer;
  output : TBuffer;
begin
  input := TBuffer.Create(value);
  output := FCrypt.Encrypt(input);
  result := output.ToHexString;
end;

function TSourceConfig.GetApiKey: string;
begin
  result := FApiKey;
end;

function TSourceConfig.GetIsEnabled: Boolean;
begin
  result := FEnabled;
end;

function TSourceConfig.GetFileName: string;
begin
  result := FFileName;
end;

function TSourceConfig.GetIsHttp: Boolean;
begin
  result := TStringUtils.StartsWith(LowerCase(FSource), 'http://') or TStringUtils.StartsWith(LowerCase(FSource), 'https://')
end;

function TSourceConfig.GetName: string;
begin
  result := FName;
end;

function TSourceConfig.GetPassword: string;
begin
  result := FPassword;
end;

function TSourceConfig.GetUserName: string;
begin
  result := FUserName;
end;

function TSourceConfig.GetSource: string;
begin
  result := FSource;
end;

function TSourceConfig.LoadFromJson(const jsonObj: TJsonObject): boolean;
begin
  result := true;
  FName     := jsonObj['name'];
  FSource   := jsonObj['source'];
  FUserName := jsonObj['userNAme'];
  FPassword := jsonObj['password'];
  if FPassword <> '' then
    FPassword := DecryptString(FPassword);
  FApiKey   := jsonObj['apiKey'];
  if FApiKey <> '' then
    FApiKey := DecryptString(FApiKey);
  if jsonObj.Contains('enabled') then
    FEnabled := jsonObj.B['enabled'];
end;

function TSourceConfig.SaveToJson(const parentObj: TJsonObject): boolean;
var
  sourceObj : TJsonObject;
begin
  sourceObj := parentObj.A['packageSources'].AddObject;
  sourceObj['name'] := FName;
  sourceObj['source'] := FSource;
  if FUserName <> '' then
    sourceObj['userName'] :=  FUserName;


  if FPassword <> '' then
    sourceObj['password'] := EncryptString(FPassword);
  if FApiKey <> '' then
    sourceObj['apiKey'] := EncryptString(FApiKey);

  sourceObj.B['enabled'] := FEnabled;
  result := true;
end;


procedure TSourceConfig.SetApiKey(const value: string);
begin
  FApiKey := value;
end;

procedure TSourceConfig.SetIsEnabled(const value: Boolean);
begin
  FEnabled := value;
end;

procedure TSourceConfig.SetName(const value: string);
begin
  FName := value;
end;

procedure TSourceConfig.SetPassword(const value: string);
begin
  FPassword := value;
end;

procedure TSourceConfig.SetUserName(const value: string);
begin
  FUserName := value;
end;

procedure TSourceConfig.SetSource(const value: string);
begin
  FSource := value;
end;

{ TConfiguration }

constructor TConfiguration.Create(const logger: ILogger);
begin
  inherited Create;
  FLogger := logger;
  FSources := TCollections.CreateList<ISourceConfig>;

end;

function TConfiguration.GetFileName: string;
begin
  result := FFileName;
end;

function TConfiguration.GetIsDefaultPackageCacheLocation: Boolean;
begin
  result := SameText(FPackageCacheLocation, cDefaultPackageCache) or
            SameText(FPackageCacheLocation, TSystemUtils.ExpandEnvironmentStrings(cDefaultPackageCache));
end;

function TConfiguration.GetPackageCacheLocation: string;
begin
  result := FPackageCacheLocation;
end;

function TConfiguration.GetSources: IList<ISourceConfig>;
begin
  result := FSources;
end;

function TConfiguration.LoadFromFile(const fileName: string): boolean;
var
  jsonObj : TJsonObject;
begin
  result := false;

  try
    jsonObj := TJsonObject.ParseFromFile(fileName) as TJsonObject;
    try
      Result := LoadFromJson(jsonObj);
      FFileName := fileName;
    finally
      jsonObj.Free;
    end;
  except
    on e : Exception do
    begin
      FLogger.Error('Exception while loading config file [' + fileName + ']' + #13#10 + e.Message);
      exit;
    end;
  end;
  FFileName := fileName;
end;

function TConfiguration.LoadFromJson(const jsonObj: TJsonObject): boolean;
var
  sourcesArray : TJsonArray;
  source : ISourceConfig;
  bResult : boolean;
  i : integer;
begin
  result := true;
  FPackageCacheLocation := jsonObj['packageCacheLocation'];
  sourcesArray := jsonObj.A['packageSources'];

  for i := 0 to sourcesArray.Count -1 do
  begin
    source := TSourceConfig.Create(FLogger);
    bResult := source.LoadFromJson(sourcesArray.O[i]);
    if bResult then
    begin
      if not FSources.Where(function(const item : ISourceConfig):boolean
                            begin
                              result := SameText(item.Name, source.Name);
                            end).Any then
      begin
        FSources.Add(source);
      end;
    end;
    result := result and bResult;
  end;
end;

function TConfiguration.SaveToFile(const fileName: string): boolean;
var
  sFileName : string;
  jsonObj : TJsonObject;
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

  jsonObj := TJsonObject.Create;
  try
    try
      result := SaveToJson(jsonObj);
      if result then
        jsonObj.SaveToFile(sFileName, false);
      FFileName := sFileName;
    except
      on e : Exception do
      begin
        FLogger.Error('Exception while saving config to file [' + sFileName + ']' + #13#10 + e.Message);
        result := false;
      end;
    end;
  finally
    jsonObj.Free;
  end;
end;

function TConfiguration.SaveToJson(const parentObj: TJsonObject): boolean;
var
  i: integer;
begin
  result := true;
  parentObj['packageCacheLocation'] := FPackageCacheLocation;

  for i := 0 to FSources.Count -1 do
    result := FSources[i].SaveToJson(parentObj) and result;

end;

procedure TConfiguration.SetFileName(const value: string);
begin
  FFileName := value;
end;

procedure TConfiguration.SetPackageCacheLocation(const value: string);
begin
  FPackageCacheLocation := value;
end;

end.
