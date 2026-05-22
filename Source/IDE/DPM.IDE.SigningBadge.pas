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
// receipt exists (package not yet in the cache), the status is "unverified"
// and the frame should show a neutral state.

interface

uses
  DPM.Core.Types,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Cache.Receipt;

type
  TSigningBadgeState = (
    sbsUnverified,           // no receipt — package not cached yet
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
    /// Returns an sbsUnverified badge when no receipt is present.
    /// </summary>
    class function Resolve(const cache : IPackageCache;
                           const receiptService : IReceiptService;
                           const id : string;
                           const version : TPackageVersion;
                           const compiler : TCompilerVersion) : TSigningBadge; static;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  DPM.Core.Package.Classes;

class function TSigningBadgeResolver.Resolve(const cache : IPackageCache;
                                              const receiptService : IReceiptService;
                                              const id : string;
                                              const version : TPackageVersion;
                                              const compiler : TCompilerVersion) : TSigningBadge;
var
  packageFolder : string;
  receipt : TVerificationReceipt;
  signerName : string;
  i : integer;
begin
  result.State := sbsUnverified;
  result.Caption := 'Unverified — install to verify';
  result.Detail := '';
  result.AccentColor := $00808080;   // grey

  if (cache = nil) or (receiptService = nil) or (id = '') then
    exit;

  packageFolder := cache.GetPackagePath(id, version.ToStringNoMeta, compiler);
  if not DirectoryExists(packageFolder) then
    exit;

  if not receiptService.TryRead(packageFolder, receipt) then
    exit;

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
    // Pick a display name from the first signature, falling back to subject.
    signerName := '';
    for i := 0 to High(receipt.Signatures) do
    begin
      if receipt.Signatures[i].SignerSubject <> '' then
      begin
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
