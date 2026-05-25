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

unit DPM.Core.Trust.Prompt;

// Abstraction for the author-downgrade prompt. The cache layer detects the
// downgrade and consults this strategy when authorDowngradePolicy=prompt.
//
// Three implementations:
//   - TNonInteractiveTrustPromptStrategy : default in Core; always denies.
//     Used in headless contexts (CI tests, anything where no UI/stdin is
//     reachable). Safe default — fails closed.
//   - TConsoleTrustPromptStrategy (DPM.Console.Trust.Prompt) : CLI [y/N]
//     prompt on stdin.
//   - TIdeTrustPromptStrategy (DPM.IDE.Trust.Prompt) : wraps the modal
//     TTrustPromptForm.
//
// IDE/CLI register their own strategy in the DI container, overriding the
// non-interactive default registered by InitCore.

interface

type
  TTrustPromptDecision = (
    tpdOverride,       // accept this build, drop the prior trust entry so
                       // the next install is treated as fresh TOFU
    tpdBlockOnce,      // reject this build, will re-prompt next time
    tpdBlockAlways     // reject this build, persist permanent block
  );

  TTrustPromptContext = record
    PackageId           : string;
    Version             : string;
    PreviousSpkiHex     : string;   // SPKI we'd seen last time (hex, no prefix)
    PreviousSubject     : string;   // best-effort display name from last receipt
    NewSigned           : boolean;  // is this build signed at all?
    NewSpkiHex          : string;   // SPKI of the new signer (if signed)
    NewSubject          : string;   // display name of the new signer (if signed)
  end;

  // Context passed when a repository-assurance (V-24) ratchet fails: either
  // a build that previously had a trusted-repo signature no longer carries
  // one, or its attested namespace changed.
  TRepoTrustPromptContext = record
    PackageId             : string;
    Version               : string;
    PreviousRepoSpkiHex   : string;  // trusted-repo SPKI from prior install
    PreviousNamespace     : string;  // attested namespace from prior install (may be '')
    NewHasTrustedRepo     : boolean; // does the new build have a trusted-repo signature?
    NewRepoSpkiHex        : string;  // trusted-repo SPKI on new build (if any)
    NewNamespace          : string;  // attested namespace on new build (if any)
  end;

  ITrustPromptStrategy = interface
    ['{C5E5BC3A-3B0B-4E6A-9D6E-7F1E2A3B4C5D}']
    /// <summary>
    /// Ask the user how to handle an author-signing change. Returns true to
    /// proceed with the install, false to abort. The `decision` reflects the
    /// chosen action so the cache layer can persist the right state.
    /// </summary>
    function PromptAuthorDowngrade(const context : TTrustPromptContext;
                                   out decision : TTrustPromptDecision) : boolean;
    /// <summary>
    /// Ask the user how to handle a repository-assurance (V-24) ratchet
    /// failure. Returns true to proceed (and clear the repo trust entry),
    /// false to abort. Same decision enum as the author prompt.
    /// </summary>
    function PromptRepositoryRatchet(const context : TRepoTrustPromptContext;
                                     out decision : TTrustPromptDecision) : boolean;
  end;

  TNonInteractiveTrustPromptStrategy = class(TInterfacedObject, ITrustPromptStrategy)
  protected
    function PromptAuthorDowngrade(const context : TTrustPromptContext;
                                   out decision : TTrustPromptDecision) : boolean;
    function PromptRepositoryRatchet(const context : TRepoTrustPromptContext;
                                     out decision : TTrustPromptDecision) : boolean;
  end;

implementation

function TNonInteractiveTrustPromptStrategy.PromptAuthorDowngrade(
  const context : TTrustPromptContext;
  out decision : TTrustPromptDecision) : boolean;
begin
  // Fail closed when no prompt mechanism is wired. Caller decides whether
  // this is an error or a silent block.
  decision := tpdBlockOnce;
  result := false;
end;

function TNonInteractiveTrustPromptStrategy.PromptRepositoryRatchet(
  const context : TRepoTrustPromptContext;
  out decision : TTrustPromptDecision) : boolean;
begin
  // Same fail-closed contract as the author prompt: with no UI wired we
  // preserve V-24's hard-fail behaviour for CI/headless contexts.
  decision := tpdBlockOnce;
  result := false;
end;

end.
