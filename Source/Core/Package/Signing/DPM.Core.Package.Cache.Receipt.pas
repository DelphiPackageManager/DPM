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

unit DPM.Core.Package.Cache.Receipt;

// dpm-verification-receipt.yaml — written last on a successful verify-and-
// extract (V-31). Records the manifest hash, signer SPKIs, effective signing
// time, trust decision, and a fingerprint of the policy that produced the
// decision so a policy change forces a re-verify (V-32, V-33).

interface

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  VSoft.YAML,
  DPM.Core.Crypto.Algorithms;

type
  TReceiptSignature = record
    Role                : string;       // 'author' | 'repository'
    SignerSpkiHex       : string;
    SignerSubject       : string;
    Thumbprint          : string;
    EffectiveSigningTime : TDateTime;
    TimestampAuthority  : string;
    RevocationStatus    : string;       // 'notChecked' | 'good' | 'revoked' | 'unknown'
    // P2 §2.2 — only populated for repository signatures from trusted repos.
    // AttestationNamespace empty = no attestation read.
    AttestationNamespace : string;
    AttestationAuthorSpkiHex : string;
    AttestationUnsignedReason : string; // '' | 'neverSigned' | 'authorCeasedSigning'
  end;

  TVerificationReceipt = record
    ReceiptVersion          : integer;
    PackageId               : string;
    Version                 : string;
    Compiler                : string;
    ManifestHashAlgorithm   : THashAlgorithm;
    ManifestHashHex         : string;
    Signatures              : TArray<TReceiptSignature>;
    TrustDecision           : string;     // 'trusted' | 'unsigned' | 'untrusted-publisher' | 'invalid'
    TrustPolicyFingerprint  : string;
    VerifiedAt              : TDateTime;
    DpmVersion              : string;
  end;

  IReceiptService = interface
    ['{D7B41D1A-0D38-4D5E-BB02-3DA0FB1F36E1}']
    procedure Write(const folder : string; const receipt : TVerificationReceipt);
    function TryRead(const folder : string; out receipt : TVerificationReceipt) : boolean;
    function ReceiptPath(const folder : string) : string;
  end;

  TYamlReceiptService = class(TInterfacedObject, IReceiptService)
  protected
    procedure Write(const folder : string; const receipt : TVerificationReceipt);
    function TryRead(const folder : string; out receipt : TVerificationReceipt) : boolean;
    function ReceiptPath(const folder : string) : string;
  end;

const
  cReceiptFileName = 'dpm-verification-receipt.yaml';
  cCurrentReceiptVersion = 1;

implementation

uses
  System.IOUtils,
  DPM.Core.Utils.DateTime;

function IsoUtc(value : TDateTime) : string;
begin
  if value = 0 then
    result := ''
  else
    result := TDPMDateTimeUtils.DateToISO8601(value, True);
end;

// YAML single-quote a string. Doubles any embedded single quote. Quoting
// every user-provided field prevents YAML from re-interpreting bare scalars
// (e.g. "02" as integer 2, "no" as boolean false) on read.
function YamlSQ(const value : string) : string;
begin
  result := '''' + StringReplace(value, '''', '''''', [rfReplaceAll]) + '''';
end;

function ParseIso(const value : string; out dt : TDateTime) : boolean;
begin
  dt := 0;
  result := false;
  if value = '' then
    exit;
  try
    dt := TDPMDateTimeUtils.ISO8601ToDate(value, True);
    result := true;
  except
    on Exception do
      result := false;
  end;
end;

{ TYamlReceiptService }

function TYamlReceiptService.ReceiptPath(const folder : string) : string;
begin
  result := IncludeTrailingPathDelimiter(folder) + cReceiptFileName;
end;

procedure TYamlReceiptService.Write(const folder : string; const receipt : TVerificationReceipt);
var
  sb : TStringBuilder;
  sig : TReceiptSignature;
  i : integer;
  destPath : string;
  tempPath : string;
begin
  if not DirectoryExists(folder) then
    raise EReadError.CreateFmt('Cache folder %s does not exist', [folder]);

  sb := TStringBuilder.Create;
  try
    sb.AppendLine('---');
    sb.Append('dpmReceiptVersion: '); sb.AppendLine(IntToStr(receipt.ReceiptVersion));
    sb.Append('packageId: '); sb.AppendLine(YamlSQ(receipt.PackageId));
    sb.Append('version: '); sb.AppendLine(YamlSQ(receipt.Version));
    sb.Append('compiler: '); sb.AppendLine(YamlSQ(receipt.Compiler));
    sb.Append('manifestHashAlgorithm: ');
    sb.AppendLine(YamlSQ(TAlgorithmProfile.HashAlgorithmName(receipt.ManifestHashAlgorithm)));
    sb.Append('manifestHash: '); sb.AppendLine(YamlSQ(receipt.ManifestHashHex));

    if Length(receipt.Signatures) = 0 then
      sb.AppendLine('signatures: []')
    else
    begin
      sb.AppendLine('signatures:');
      for i := 0 to High(receipt.Signatures) do
      begin
        sig := receipt.Signatures[i];
        sb.Append('- role: '); sb.AppendLine(YamlSQ(sig.Role));
        sb.Append('  signerSpki: '); sb.AppendLine(YamlSQ(sig.SignerSpkiHex));
        sb.Append('  signerSubject: '); sb.AppendLine(YamlSQ(sig.SignerSubject));
        sb.Append('  thumbprint: '); sb.AppendLine(YamlSQ(sig.Thumbprint));
        sb.Append('  effectiveSigningTime: '); sb.AppendLine(YamlSQ(IsoUtc(sig.EffectiveSigningTime)));
        sb.Append('  timestampAuthority: '); sb.AppendLine(YamlSQ(sig.TimestampAuthority));
        sb.Append('  revocationStatus: '); sb.AppendLine(YamlSQ(sig.RevocationStatus));
        // P2 §2.2 — attestation block is omitted entirely when not present
        // (i.e. for author signatures or untrusted-repo signatures).
        if sig.AttestationNamespace <> '' then
          begin
            sb.Append('  attestationNamespace: '); sb.AppendLine(YamlSQ(sig.AttestationNamespace));
          end;
        if sig.AttestationAuthorSpkiHex <> '' then
          begin
            sb.Append('  attestationAuthorSpki: '); sb.AppendLine(YamlSQ(sig.AttestationAuthorSpkiHex));
          end;
        if sig.AttestationUnsignedReason <> '' then
          begin
            sb.Append('  attestationUnsignedReason: '); sb.AppendLine(YamlSQ(sig.AttestationUnsignedReason));
          end;
      end;
    end;

    sb.Append('trustDecision: '); sb.AppendLine(YamlSQ(receipt.TrustDecision));
    sb.Append('trustPolicyFingerprint: '); sb.AppendLine(YamlSQ(receipt.TrustPolicyFingerprint));
    sb.Append('verifiedAt: '); sb.AppendLine(YamlSQ(IsoUtc(receipt.VerifiedAt)));
    sb.Append('dpmVersion: '); sb.AppendLine(YamlSQ(receipt.DpmVersion));

    destPath := ReceiptPath(folder);
    tempPath := destPath + '.tmp';
    TFile.WriteAllText(tempPath, sb.ToString, TEncoding.UTF8);
    if FileExists(destPath) then
      DeleteFile(destPath);
    if not RenameFile(tempPath, destPath) then
      raise EWriteError.CreateFmt('Failed to rename %s -> %s', [tempPath, destPath]);
  finally
    sb.Free;
  end;
end;

function TYamlReceiptService.TryRead(const folder : string; out receipt : TVerificationReceipt) : boolean;
var
  doc : IYAMLDocument;
  root : IYAMLValue;
  mapping : IYAMLMapping;
  sigSeq : IYAMLSequence;
  sigItem : IYAMLValue;
  i : integer;
  algName : string;
  algorithm : THashAlgorithm;
  filePath : string;
  sig : TReceiptSignature;
begin
  result := false;
  FillChar(receipt, SizeOf(receipt), 0);
  // Strings inside the record are managed types — re-initialise to empty.
  receipt.PackageId := '';
  receipt.Version := '';
  receipt.Compiler := '';
  receipt.ManifestHashHex := '';
  receipt.TrustDecision := '';
  receipt.TrustPolicyFingerprint := '';
  receipt.DpmVersion := '';
  SetLength(receipt.Signatures, 0);

  filePath := ReceiptPath(folder);
  if not FileExists(filePath) then
    exit;
  try
    doc := TYAML.LoadFromFile(filePath);
  except
    on Exception do
      exit;
  end;
  if (doc = nil) or (doc.Root = nil) or not doc.Root.IsMapping then
    exit;
  root := doc.Root;
  mapping := root.AsMapping;

  if mapping.Values['dpmReceiptVersion'].IsInteger then
    receipt.ReceiptVersion := mapping.Values['dpmReceiptVersion'].AsInteger;
  if mapping.Values['packageId'].IsString then
    receipt.PackageId := mapping.Values['packageId'].AsString;
  if mapping.Values['version'].IsString then
    receipt.Version := mapping.Values['version'].AsString;
  if mapping.Values['compiler'].IsString then
    receipt.Compiler := mapping.Values['compiler'].AsString;
  if mapping.Values['manifestHashAlgorithm'].IsString then
  begin
    algName := mapping.Values['manifestHashAlgorithm'].AsString;
    if TAlgorithmProfile.ParseHashName(algName, algorithm) then
      receipt.ManifestHashAlgorithm := algorithm
    else
      receipt.ManifestHashAlgorithm := haUnknown;
  end;
  if mapping.Values['manifestHash'].IsString then
    receipt.ManifestHashHex := mapping.Values['manifestHash'].AsString;
  if mapping.Values['trustDecision'].IsString then
    receipt.TrustDecision := mapping.Values['trustDecision'].AsString;
  if mapping.Values['trustPolicyFingerprint'].IsString then
    receipt.TrustPolicyFingerprint := mapping.Values['trustPolicyFingerprint'].AsString;
  if mapping.Values['dpmVersion'].IsString then
    receipt.DpmVersion := mapping.Values['dpmVersion'].AsString;
  if mapping.Values['verifiedAt'].IsString then
    ParseIso(mapping.Values['verifiedAt'].AsString, receipt.VerifiedAt);

  if mapping.Values['signatures'].IsSequence then
  begin
    sigSeq := mapping.Values['signatures'].AsSequence;
    SetLength(receipt.Signatures, sigSeq.Count);
    for i := 0 to sigSeq.Count - 1 do
    begin
      sigItem := sigSeq.Items[i];
      if (sigItem = nil) or not sigItem.IsMapping then
        Continue;
      sig.Role := '';
      sig.SignerSpkiHex := '';
      sig.SignerSubject := '';
      sig.Thumbprint := '';
      sig.TimestampAuthority := '';
      sig.RevocationStatus := '';
      sig.EffectiveSigningTime := 0;
      sig.AttestationNamespace := '';
      sig.AttestationAuthorSpkiHex := '';
      sig.AttestationUnsignedReason := '';

      if sigItem.AsMapping.Values['role'].IsString then
        sig.Role := sigItem.AsMapping.Values['role'].AsString;
      if sigItem.AsMapping.Values['signerSpki'].IsString then
        sig.SignerSpkiHex := sigItem.AsMapping.Values['signerSpki'].AsString;
      if sigItem.AsMapping.Values['signerSubject'].IsString then
        sig.SignerSubject := sigItem.AsMapping.Values['signerSubject'].AsString;
      if sigItem.AsMapping.Values['thumbprint'].IsString then
        sig.Thumbprint := sigItem.AsMapping.Values['thumbprint'].AsString;
      if sigItem.AsMapping.Values['timestampAuthority'].IsString then
        sig.TimestampAuthority := sigItem.AsMapping.Values['timestampAuthority'].AsString;
      if sigItem.AsMapping.Values['revocationStatus'].IsString then
        sig.RevocationStatus := sigItem.AsMapping.Values['revocationStatus'].AsString;
      if sigItem.AsMapping.Values['effectiveSigningTime'].IsString then
        ParseIso(sigItem.AsMapping.Values['effectiveSigningTime'].AsString, sig.EffectiveSigningTime);
      if sigItem.AsMapping.Values['attestationNamespace'].IsString then
        sig.AttestationNamespace := sigItem.AsMapping.Values['attestationNamespace'].AsString;
      if sigItem.AsMapping.Values['attestationAuthorSpki'].IsString then
        sig.AttestationAuthorSpkiHex := sigItem.AsMapping.Values['attestationAuthorSpki'].AsString;
      if sigItem.AsMapping.Values['attestationUnsignedReason'].IsString then
        sig.AttestationUnsignedReason := sigItem.AsMapping.Values['attestationUnsignedReason'].AsString;

      receipt.Signatures[i] := sig;
    end;
  end;

  result := true;
end;

end.
