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
  end;

implementation

uses
  DPM.IDE.TrustPromptForm;

function TIdeTrustPromptStrategy.PromptAuthorDowngrade(
  const context : TTrustPromptContext;
  out decision : TTrustPromptDecision) : boolean;
var
  formDecision : TTrustPromptResult;
begin
  result := TTrustPromptForm.Prompt(
    context.PackageId,
    context.Version,
    context.PreviousSpkiHex,
    context.NewSigned,
    context.NewSpkiHex,
    formDecision);
  case formDecision of
    tprTrustOnce  : decision := tpdTrustOnce;
    tprBlockOnce  : decision := tpdBlockOnce;
    tprBlockAlways: decision := tpdBlockAlways;
  else
    decision := tpdBlockOnce;
  end;
end;

end.
