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

unit DPM.IDE.SigningBadge;

// IDE-2: helper that resolves the verification receipt for a cached package
// and projects it into a small, display-ready record. PackageDetailsFrame
// consumes this and renders the status header above the version selector.
//
// The receipt is the source of truth — we don't re-verify here. If no
// receipt exists (package not yet in the cache), we fall back to the
// gallery-reported isSigned/signedBy fields off the feed, so users can
// still see who signed a package before they install it. The local
// verification will run again at install time against the trust set, so
// the gallery-reported state is advisory only.

interface

uses
  DPM.Core.Types,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Cache.Receipt;

type
  TSigningBadgeState = (
    sbsUnverified,           // no receipt and no gallery info
    sbsGalleryReported,      // no local receipt yet, but gallery reports signed
    sbsIntegrityUnsigned,    // receipt present, no signatures (permissive)
    sbsUntrustedPublisher,   // signed, signer not in trustedPublishers
    sbsSignedTrusted,        // signed, trusted (this is the green case)
    sbsInvalid               // receipt records a verification failure
  );

  TSigningBadge = record
    State       : TSigningBadgeState;
    Caption     : string;     // one-line summary for the header label
    Detail      : string;     // multi-line text for the click-through dialog
    // BGR-packed colour the IDE label uses for tint. Plain integer so the
    // resolver stays Core-friendly (no Vcl.Graphics dep), the IDE casts
    // this to TColor at the consumer.
    AccentColor : Cardinal;
  end;

  TSigningBadgeResolver = record
  public
    /// <summary>
    /// Reads the verification receipt for a given (id, version, compiler)
    /// out of the package cache and projects it into a display badge.
    /// When no receipt is present, falls back to the gallery-reported
    /// signing fields (advisory). Pass gallerySigned=false to suppress
    /// the fallback (e.g. local package sources that don't supply them).
    /// </summary>
    class function Resolve(const cache : IPackageCache;
                           const receiptService : IReceiptService;
                           const id : string;
                           const version : TPackageVersion;
                           const compiler : TCompilerVersion;
                           const gallerySigned : boolean;
                           const gallerySignedBy : string) : TSigningBadge; overload; static;
    // Convenience overload for callers that don't have gallery hints.
    class function Resolve(const cache : IPackageCache;
                           const receiptService : IReceiptService;
                           const id : string;
                           const version : TPackageVersion;
                           const compiler : TCompilerVersion) : TSigningBadge; overload; static;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  DPM.Core.Package.Classes;

// Extract the Common Name from an X.500 DN string. Receipt stores the full
// SignerSubject (e.g. "CN=VSoft Technologies, O=VSoft Technologies, L=...,
// C=AU") which is far too long for the header label. Mirrors the parser on
// TCertificate (see DPM.Core.Crypto.X509) — quote-aware so a CN containing
// a literal comma like CN="Foo, Bar" parses correctly.
function ExtractCommonName(const dn : string) : string;
const
  cCnPrefix = 'CN=';
var
  startPos : integer;
  endPos : integer;
  inQuotes : boolean;
  c : Char;
begin
  result := '';
  startPos := Pos(cCnPrefix, dn);
  if startPos = 0 then
    exit;
  Inc(startPos, Length(cCnPrefix));
  inQuotes := false;
  endPos := startPos;
  while endPos <= Length(dn) do
  begin
    c := dn[endPos];
    if c = '"' then
      inQuotes := not inQuotes
    else if (c = ',') and not inQuotes then
      Break;
    Inc(endPos);
  end;
  result := Trim(Copy(dn, startPos, endPos - startPos));
  // Strip surrounding quotes if the whole value was quoted.
  if (Length(result) >= 2) and (result[1] = '"') and (result[Length(result)] = '"') then
    result := Copy(result, 2, Length(result) - 2);
end;

class function TSigningBadgeResolver.Resolve(const cache : IPackageCache;
                                              const receiptService : IReceiptService;
                                              const id : string;
                                              const version : TPackageVersion;
                                              const compiler : TCompilerVersion) : TSigningBadge;
begin
  result := Resolve(cache, receiptService, id, version, compiler, false, '');
end;

class function TSigningBadgeResolver.Resolve(const cache : IPackageCache;
                                              const receiptService : IReceiptService;
                                              const id : string;
                                              const version : TPackageVersion;
                                              const compiler : TCompilerVersion;
                                              const gallerySigned : boolean;
                                              const gallerySignedBy : string) : TSigningBadge;
var
  packageFolder : string;
  receipt : TVerificationReceipt;
  signerName : string;
  i : integer;
  haveReceipt : boolean;
begin
  result.State := sbsUnverified;
  result.Caption := 'Unverified — install to verify';
  result.Detail := '';
  result.AccentColor := $00808080;   // grey

  haveReceipt := false;
  if (cache <> nil) and (receiptService <> nil) and (id <> '') then
  begin
    packageFolder := cache.GetPackagePath(id, version.ToStringNoMeta, compiler);
    if DirectoryExists(packageFolder) and receiptService.TryRead(packageFolder, receipt) then
      haveReceipt := true;
  end;

  if not haveReceipt then
  begin
    // No local receipt — fall back to the gallery's advisory signing
    // fields if we have them. The CLI will still verify locally at
    // install time against the trust set; this is just so the user can
    // see who signed a package before they download it.
    if gallerySigned then
    begin
      result.State := sbsGalleryReported;
      if gallerySignedBy <> '' then
        result.Caption := 'Signed by ' + gallerySignedBy + ' (gallery)'
      else
        result.Caption := 'Signed (gallery)';
      result.Detail :=
        'The gallery reports that this package version was signed at upload time.' +
        sLineBreak +
        'The signature has not yet been verified locally — that happens on install ' +
        'against your active trust set.';
      // Steel-blue: informational, distinct from trusted-green and amber-untrusted.
      result.AccentColor := $00B07020;
    end;
    exit;
  end;

  // Project the receipt onto the badge.
  if SameText(receipt.TrustDecision, 'unsigned') then
  begin
    result.State := sbsIntegrityUnsigned;
    result.Caption := 'Integrity-checked, unsigned';
    result.AccentColor := $00808080;   // grey-ish
  end
  else if SameText(receipt.TrustDecision, 'untrusted-publisher') then
  begin
    result.State := sbsUntrustedPublisher;
    result.Caption := 'Signed, signer not in trusted publishers';
    result.AccentColor := $000080FF;   // amber
  end
  else if SameText(receipt.TrustDecision, 'trusted') then
  begin
    result.State := sbsSignedTrusted;
    // Pick a display name from the first signature. Show just the CN —
    // the full DN ("CN=..., O=..., L=..., S=..., C=AU") doesn't fit the
    // status header. The Detail dialog still carries the full subject.
    signerName := '';
    for i := 0 to High(receipt.Signatures) do
    begin
      if receipt.Signatures[i].SignerSubject <> '' then
      begin
        signerName := ExtractCommonName(receipt.Signatures[i].SignerSubject);
        if signerName = '' then
          // No CN parsed — fall back to the raw subject so the user still
          // sees *something* identifying the signer.
          signerName := receipt.Signatures[i].SignerSubject;
        break;
      end;
    end;
    if signerName = '' then
      result.Caption := 'Signed by trusted publisher'
    else
      result.Caption := 'Signed by ' + signerName;
    result.AccentColor := $00207020;   // green
  end
  else
  begin
    result.State := sbsInvalid;
    result.Caption := 'Signature invalid';
    result.AccentColor := $002020D0;   // red
  end;

  // Build the multi-line detail. Kept brief — the dialog can grow as
  // needed when we wire it up.
  result.Detail :=
    Format('Package: %s %s' + sLineBreak +
           'Verified: %s' + sLineBreak +
           'Manifest hash (%s): %s' + sLineBreak +
           'Signatures: %d',
      [receipt.PackageId, receipt.Version,
       DateTimeToStr(receipt.VerifiedAt),
       'SHA-256',
       receipt.ManifestHashHex,
       Length(receipt.Signatures)]);
  for i := 0 to High(receipt.Signatures) do
    result.Detail := result.Detail + sLineBreak +
      Format('  [%s] %s (sha256:%s)',
        [receipt.Signatures[i].Role,
         receipt.Signatures[i].SignerSubject,
         receipt.Signatures[i].SignerSpkiHex]);
end;

end.
