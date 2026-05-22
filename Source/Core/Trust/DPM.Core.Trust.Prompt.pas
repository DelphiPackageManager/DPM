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
    tpdTrustOnce,      // accept this build, persist so we don't re-prompt
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

  ITrustPromptStrategy = interface
    ['{C5E5BC3A-3B0B-4E6A-9D6E-7F1E2A3B4C5D}']
    /// <summary>
    /// Ask the user how to handle an author-signing change. Returns true to
    /// proceed with the install, false to abort. The `decision` reflects the
    /// chosen action so the cache layer can persist the right state.
    /// </summary>
    function PromptAuthorDowngrade(const context : TTrustPromptContext;
                                   out decision : TTrustPromptDecision) : boolean;
  end;

  TNonInteractiveTrustPromptStrategy = class(TInterfacedObject, ITrustPromptStrategy)
  protected
    function PromptAuthorDowngrade(const context : TTrustPromptContext;
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

end.
