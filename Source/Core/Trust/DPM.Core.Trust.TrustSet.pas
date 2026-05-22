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

unit DPM.Core.Trust.TrustSet;

// Built-in versioned trust set. Phase 1 ships a YAML resource compiled into
// the binary; the loader tolerates a missing or out-of-format resource and
// falls back to an empty trust set (the user can still configure trust via
// dpm.config.yaml). Phase 2 adds signed rollover metadata.

interface

uses
  System.Classes, System.SysUtils,
  VSoft.YAML,
  DPM.Core.Trust.Interfaces;

type
  TBuiltInTrustSet = class(TInterfacedObject, ITrustSet)
  private
    FVersion : integer;
    FDefaultMode : TValidationMode;
    FRepositorySpkis : TArray<TTrustedRepository>;
    FRevokedRepositorySpkis : TArray<string>;
    procedure LoadFromYaml(const yaml : string);
  protected
    function Version : integer;
    function DefaultValidationMode : TValidationMode;
    function RepositorySpkis : TArray<TTrustedRepository>;
    function RevokedRepositorySpkis : TArray<string>;
  public
    /// <summary>
    /// Default constructor — loads from the DPM_TRUST_SET YAML resource
    /// compiled into the host binary. Missing resource yields safe defaults.
    /// </summary>
    constructor Create; overload;
    /// <summary>
    /// Loads from an explicit YAML string. Used by tests and by callers
    /// that need to apply trust-set rollover metadata fetched at runtime.
    /// </summary>
    constructor Create(const yaml : string); overload;
  end;

implementation

uses
  Winapi.Windows;

const
  cTrustSetResource = 'DPM_TRUST_SET';
  cTrustSetResType  = 'YAML';

function LoadResourceAsString(const resName, resType : string) : string;
var
  hRes : HRSRC;
  hMem : HGLOBAL;
  ptr : Pointer;
  size : DWORD;
  bytes : TBytes;
begin
  result := '';
  hRes := FindResource(HInstance, PChar(resName), PChar(resType));
  if hRes = 0 then
    exit;
  hMem := LoadResource(HInstance, hRes);
  if hMem = 0 then
    exit;
  ptr := LockResource(hMem);
  size := SizeofResource(HInstance, hRes);
  if (ptr = nil) or (size = 0) then
    exit;
  SetLength(bytes, size);
  Move(ptr^, bytes[0], size);
  result := TEncoding.UTF8.GetString(bytes);
end;

constructor TBuiltInTrustSet.Create;
var
  text : string;
begin
  inherited Create;
  FVersion := 0;
  FDefaultMode := vmPermissive;
  SetLength(FRepositorySpkis, 0);
  SetLength(FRevokedRepositorySpkis, 0);
  text := LoadResourceAsString(cTrustSetResource, cTrustSetResType);
  if text <> '' then
    LoadFromYaml(text);
end;

constructor TBuiltInTrustSet.Create(const yaml : string);
begin
  inherited Create;
  FVersion := 0;
  FDefaultMode := vmPermissive;
  SetLength(FRepositorySpkis, 0);
  SetLength(FRevokedRepositorySpkis, 0);
  if yaml <> '' then
    LoadFromYaml(yaml);
end;

procedure TBuiltInTrustSet.LoadFromYaml(const yaml : string);
var
  doc : IYAMLDocument;
  root, repoSeq, item, modeNode : IYAMLValue;
  i : integer;
  seq : IYAMLSequence;
  entry : TTrustedRepository;
begin
  try
    doc := TYAML.LoadFromString(yaml);
  except
    on Exception do
      exit;   // tolerate malformed resource; effective trust set stays empty
  end;
  if (doc = nil) or (doc.Root = nil) or not doc.Root.IsMapping then
    exit;
  root := doc.Root;

  if root.AsMapping.Values['dpmTrustSetVersion'].IsInteger then
    FVersion := root.AsMapping.Values['dpmTrustSetVersion'].AsInteger;

  modeNode := root.AsMapping.Values['defaultValidationMode'];
  if modeNode.IsString then
  begin
    if SameText(modeNode.AsString, 'require') then
      FDefaultMode := vmRequire
    else if SameText(modeNode.AsString, 'repository-required') then
      FDefaultMode := vmRepositoryRequired
    else if SameText(modeNode.AsString, 'author-and-repository') then
      FDefaultMode := vmAuthorAndRepository
    else
      FDefaultMode := vmPermissive;
  end;

  repoSeq := root.AsMapping.Values['repositorySpki'];
  if (repoSeq <> nil) and repoSeq.IsSequence then
  begin
    seq := repoSeq.AsSequence;
    SetLength(FRepositorySpkis, seq.Count);
    for i := 0 to seq.Count - 1 do
    begin
      item := seq.Items[i];
      if (item = nil) or not item.IsMapping then
        Continue;
      entry.Url := '';
      if item.AsMapping.Values['name'].IsString then
        entry.Url := item.AsMapping.Values['name'].AsString;
      if item.AsMapping.Values['spki'].IsString then
        entry.SpkiHex := item.AsMapping.Values['spki'].AsString;
      FRepositorySpkis[i] := entry;
    end;
  end;

  // P3 §3.4 — emergency revocation channel. Plain sequence of SPKI strings.
  // YAML quirk: an unquoted scalar like `sha256:abcd1234` parses as a 1-key
  // mapping (`{sha256: abcd1234}`) in strict YAML because of the embedded
  // colon. We tolerate both shapes here so users don't have to remember to
  // quote — the unquoted mapping form is reassembled into `key:value`.
  repoSeq := root.AsMapping.Values['revokedRepositorySpki'];
  if (repoSeq <> nil) and repoSeq.IsSequence then
  begin
    seq := repoSeq.AsSequence;
    SetLength(FRevokedRepositorySpkis, seq.Count);
    for i := 0 to seq.Count - 1 do
    begin
      item := seq.Items[i];
      if item = nil then
        Continue;
      if item.IsString then
        FRevokedRepositorySpkis[i] := item.AsString
      else if item.IsMapping and (item.AsMapping.Count = 1) then
      begin
        // Reassemble {key: value} back into `key:value`. Value side may be
        // an integer if it parses as one — defensively coerce. (For real
        // hex SPKIs the value always has at least one a-f char so it
        // shouldn't, but we don't want a 0..9-only SPKI to vanish.)
        FRevokedRepositorySpkis[i] := item.AsMapping.GetKey(0) + ':';
        item := item.AsMapping.Values[item.AsMapping.GetKey(0)];
        if item.IsString then
          FRevokedRepositorySpkis[i] := FRevokedRepositorySpkis[i] + item.AsString
        else if item.IsInteger then
          FRevokedRepositorySpkis[i] := FRevokedRepositorySpkis[i] + IntToStr(item.AsInteger);
      end;
    end;
  end;
end;

function TBuiltInTrustSet.Version : integer;
begin result := FVersion; end;

function TBuiltInTrustSet.DefaultValidationMode : TValidationMode;
begin result := FDefaultMode; end;

function TBuiltInTrustSet.RepositorySpkis : TArray<TTrustedRepository>;
begin result := FRepositorySpkis; end;

function TBuiltInTrustSet.RevokedRepositorySpkis : TArray<string>;
begin result := FRevokedRepositorySpkis; end;

end.
