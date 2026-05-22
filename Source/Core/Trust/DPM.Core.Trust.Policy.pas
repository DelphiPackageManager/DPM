{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
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

unit DPM.Core.Trust.Policy;

// Effective trust policy resolution. Pulls validation mode and SPKI pin sets
// out of configuration, computes a fingerprint over the canonical projection
// (the receipt comparison input), and answers `is this SPKI trusted?`.

interface

uses
  System.Classes, System.SysUtils,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Trust.Interfaces;

type
  TTrustPolicyService = class(TInterfacedObject, ITrustPolicyService)
  private
    FConfigManager : IConfigurationManager;
    FTrustSet : ITrustSet;
    FHashing : IHashingService;
  protected
    function GetEffectivePolicy : TTrustPolicy;
    function PolicyFingerprint(const policy : TTrustPolicy) : string;
    function PublisherTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
    function RepositoryTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
  public
    constructor Create(const configManager : IConfigurationManager;
                       const trustSet : ITrustSet;
                       const hashing : IHashingService);
  end;

implementation

uses
  System.Generics.Defaults, System.Generics.Collections,
  DPM.Core.Utils.Config;

constructor TTrustPolicyService.Create(const configManager : IConfigurationManager;
                                       const trustSet : ITrustSet;
                                       const hashing : IHashingService);
begin
  if (configManager = nil) or (trustSet = nil) or (hashing = nil) then
    raise ETrust.Create('TTrustPolicyService dependencies missing');
  inherited Create;
  FConfigManager := configManager;
  FTrustSet := trustSet;
  FHashing := hashing;
end;

function ParseValidationMode(const value : string; fallback : TValidationMode) : TValidationMode;
begin
  if SameText(value, 'permissive') then
    result := vmPermissive
  else if SameText(value, 'require') then
    result := vmRequire
  else if SameText(value, 'repository-required') then
    result := vmRepositoryRequired
  else if SameText(value, 'author-and-repository') then
    result := vmAuthorAndRepository
  else
    result := fallback;
end;

function ParseAuthorDowngradePolicy(const value : string) : TAuthorDowngradePolicy;
begin
  if SameText(value, 'block') then
    result := adpBlock
  else if SameText(value, 'allow') then
    result := adpAllow
  else
    result := adpPrompt;
end;

function TTrustPolicyService.GetEffectivePolicy : TTrustPolicy;
var
  config : IConfiguration;
  signing : ISigningConfig;
  i : integer;
  pub : TTrustedPublisher;
  repo : TTrustedRepository;
  fromTrustSet : TArray<TTrustedRepository>;
  combined : TArray<TTrustedRepository>;
  k : integer;
  duplicate : boolean;
  j : integer;
begin
  // Start with trust-set defaults so a missing config still verifies safely.
  result.ValidationMode := FTrustSet.DefaultValidationMode;
  result.AuthorDowngradePolicy := adpPrompt;
  result.AllowKeyCompromiseOverride := false;
  SetLength(result.TrustedPublishers, 0);
  SetLength(result.TrustedRepositories, 0);
  result.TrustSetVersion := FTrustSet.Version;

  FConfigManager.EnsureDefaultConfig;
  config := FConfigManager.LoadConfig(TConfigUtils.GetDefaultConfigFileName);
  if config = nil then
    exit;

  signing := config.Signing;
  if signing = nil then
    exit;

  result.ValidationMode := ParseValidationMode(signing.ValidationMode,
    FTrustSet.DefaultValidationMode);
  result.AuthorDowngradePolicy := ParseAuthorDowngradePolicy(signing.AuthorDowngradePolicy);
  result.AllowKeyCompromiseOverride := signing.AllowKeyCompromiseOverride;

  SetLength(result.TrustedPublishers, signing.TrustedPublishers.Count);
  for i := 0 to signing.TrustedPublishers.Count - 1 do
  begin
    pub.Name := signing.TrustedPublishers[i].Name;
    pub.SpkiHex := signing.TrustedPublishers[i].Spki;
    result.TrustedPublishers[i] := pub;
  end;

  // Merge configured repository pins with the built-in trust-set pins, with
  // dedup by normalised SPKI hex.
  SetLength(combined, signing.TrustedRepositories.Count);
  for i := 0 to signing.TrustedRepositories.Count - 1 do
  begin
    repo.Url := signing.TrustedRepositories[i].Url;
    repo.SpkiHex := signing.TrustedRepositories[i].Spki;
    combined[i] := repo;
  end;

  fromTrustSet := FTrustSet.RepositorySpkis;
  for k := 0 to High(fromTrustSet) do
  begin
    duplicate := false;
    for j := 0 to High(combined) do
      if SameText(combined[j].SpkiHex, fromTrustSet[k].SpkiHex) then
      begin
        duplicate := true;
        Break;
      end;
    if not duplicate then
    begin
      SetLength(combined, Length(combined) + 1);
      combined[High(combined)] := fromTrustSet[k];
    end;
  end;
  result.TrustedRepositories := combined;
end;

function NormHex(const s : string) : string;
var
  i : integer;
begin
  // Strip "sha256:" prefix, spaces, and colons, then lowercase.
  result := LowerCase(Trim(s));
  if (Length(result) > 7) and (Copy(result, 1, 7) = 'sha256:') then
    result := Copy(result, 8, MaxInt);
  for i := Length(result) downto 1 do
    if (result[i] = ' ') or (result[i] = ':') then
      Delete(result, i, 1);
end;

function TTrustPolicyService.PolicyFingerprint(const policy : TTrustPolicy) : string;
var
  sb : TStringBuilder;
  i : integer;
  modeStr : string;
  hashBytes : TBytes;
begin
  case policy.ValidationMode of
    vmPermissive          : modeStr := 'permissive';
    vmRequire             : modeStr := 'require';
    vmRepositoryRequired  : modeStr := 'repository-required';
    vmAuthorAndRepository : modeStr := 'author-and-repository';
  else
    modeStr := 'unknown';
  end;

  sb := TStringBuilder.Create;
  try
    sb.Append('mode='); sb.Append(modeStr); sb.Append(';');
    sb.Append('adp='); sb.Append(IntToStr(Ord(policy.AuthorDowngradePolicy))); sb.Append(';');
    sb.Append('keyCompromiseOverride='); sb.Append(BoolToStr(policy.AllowKeyCompromiseOverride, True)); sb.Append(';');
    sb.Append('trustSetVersion='); sb.Append(IntToStr(policy.TrustSetVersion)); sb.Append(';');
    sb.Append('publishers=[');
    for i := 0 to High(policy.TrustedPublishers) do
    begin
      if i > 0 then sb.Append(',');
      sb.Append(NormHex(policy.TrustedPublishers[i].SpkiHex));
    end;
    sb.Append('];');
    sb.Append('repositories=[');
    for i := 0 to High(policy.TrustedRepositories) do
    begin
      if i > 0 then sb.Append(',');
      sb.Append(NormHex(policy.TrustedRepositories[i].SpkiHex));
    end;
    sb.Append(']');

    hashBytes := FHashing.HashString(sb.ToString, haSha256);
    result := BytesToHex(hashBytes);
  finally
    sb.Free;
  end;
end;

function TTrustPolicyService.PublisherTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
var
  target : string;
  i : integer;
begin
  target := NormHex(spkiHex);
  for i := 0 to High(policy.TrustedPublishers) do
    if NormHex(policy.TrustedPublishers[i].SpkiHex) = target then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TTrustPolicyService.RepositoryTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
var
  target : string;
  i : integer;
begin
  target := NormHex(spkiHex);
  for i := 0 to High(policy.TrustedRepositories) do
    if NormHex(policy.TrustedRepositories[i].SpkiHex) = target then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

end.
