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

unit DPM.IDE.Trust.Prompt;

// IDE implementation of ITrustPromptStrategy — wraps the modal
// TTrustPromptForm. Registered by the IDE plugin in place of the Core
// non-interactive default.

interface

uses
  DPM.Core.Trust.Prompt;

type
  TIdeTrustPromptStrategy = class(TInterfacedObject, ITrustPromptStrategy)
  protected
    function PromptAuthorDowngrade(const context : TTrustPromptContext;
                                   out decision : TTrustPromptDecision) : boolean;
    function PromptRepositoryRatchet(const context : TRepoTrustPromptContext;
                                     out decision : TTrustPromptDecision) : boolean;
  end;

implementation

uses
  System.SysUtils,
  DPM.IDE.TrustPromptForm;

const
  cConsequence =
    'Choose "Override and install" to drop the prior trust record and install this build. ' +
    'Choose "Cancel install" to abort. "Always block" permanently bans every version of ' +
    'this package id.';

// Show only the first eight bytes of an SPKI hex for the at-a-glance view.
// The full value lives in trust-state.yaml for users who want to verify it.
function ShortSpki(const fullHex : string) : string;
begin
  if Length(fullHex) <= 16 then
    result := fullHex
  else
    result := Copy(fullHex, 1, 16) + '…';
end;

function MapDecision(formDecision : TTrustPromptResult) : TTrustPromptDecision;
begin
  case formDecision of
    tprOverride    : result := tpdOverride;
    tprCancel      : result := tpdBlockOnce;
    tprBlockAlways : result := tpdBlockAlways;
  else
    result := tpdBlockOnce;
  end;
end;

function TIdeTrustPromptStrategy.PromptAuthorDowngrade(
  const context : TTrustPromptContext;
  out decision : TTrustPromptDecision) : boolean;
var
  formDecision : TTrustPromptResult;
  summary : string;
  newDescription : string;
begin
  if context.NewSigned then
  begin
    summary :=
      'You previously installed a signed build of this package. The build you''re ' +
      'about to install is signed by a different key. This may be a legitimate ' +
      'signer rotation, or it may indicate the package has been tampered with.';
    newDescription := 'signed by a different key (sha256:' + ShortSpki(context.NewSpkiHex) + ')';
  end
  else
  begin
    summary :=
      'You previously installed a signed build of this package. The build you''re ' +
      'about to install is UNSIGNED. This is normal for in-house dev builds, but ' +
      'is a downgrade in assurance from what you trusted before.';
    newDescription := 'UNSIGNED';
  end;

  result := TTrustPromptForm.Prompt(
    'Package signing change detected',
    summary,
    cConsequence,
    'Package: ' + context.PackageId + '  ' + context.Version,
    'Prior signer: sha256:' + ShortSpki(context.PreviousSpkiHex),
    'New build: ' + newDescription,
    formDecision);
  decision := MapDecision(formDecision);
end;

function TIdeTrustPromptStrategy.PromptRepositoryRatchet(
  const context : TRepoTrustPromptContext;
  out decision : TTrustPromptDecision) : boolean;
var
  formDecision : TTrustPromptResult;
  summary : string;
  newDescription : string;
  techPrev : string;
begin
  if not context.NewHasTrustedRepo then
  begin
    summary :=
      'You previously installed this package from a trusted repository (with a ' +
      'repository signature). The build you''re about to install does NOT carry a ' +
      'trusted-repository signature. This may be a legitimate move to a different ' +
      'source, or it may indicate publisher impersonation.';
    newDescription := 'no trusted-repository signature';
  end
  else
  begin
    summary :=
      Format('You previously installed this package under namespace "%s". The build ' +
             'you''re about to install attests to a different namespace "%s". This ' +
             'is a hard fail per V-24 — closes the publisher-impersonation door.',
             [context.PreviousNamespace, context.NewNamespace]);
    newDescription := Format('namespace "%s" (sha256:%s)',
      [context.NewNamespace, ShortSpki(context.NewRepoSpkiHex)]);
  end;

  techPrev := 'Prior trusted repo: sha256:' + ShortSpki(context.PreviousRepoSpkiHex);
  if context.PreviousNamespace <> '' then
    techPrev := techPrev + ' / namespace "' + context.PreviousNamespace + '"';

  result := TTrustPromptForm.Prompt(
    'Repository trust change detected (V-24)',
    summary,
    cConsequence,
    'Package: ' + context.PackageId + '  ' + context.Version,
    techPrev,
    'New build: ' + newDescription,
    formDecision);
  decision := MapDecision(formDecision);
end;

end.
