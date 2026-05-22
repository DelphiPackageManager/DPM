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

unit DPM.Core.Crypto.Provider.Interfaces;

interface

uses
  WinApi.Windows,
  System.SysUtils,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.X509.Interfaces;

type
  // The single abstraction the CMS layer uses for signing. The provider's
  // implementation owns the private key (or proxies a remote one in Phase 3).
  // The CMS layer never touches the key directly, never opens stores itself.
  //
  // The provider exposes the leaf certificate and the OS handle pair
  // (HCRYPTPROV_OR_NCRYPT_KEY_HANDLE + key spec) that the OS sign function
  // needs. In Phase 3 a TKeyVaultProvider replaces the handle pair with a
  // pre-computed digest signature; the CMS layer's seam will be widened then.
  ISigningProvider = interface
    ['{D90DD9C6-44B2-4C32-A3B1-5DD3D6E9D2BC}']
    function Certificate : ICertificate;
    function AcquirePrivateKey(out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                               out keySpec : DWORD;
                               out callerMustFree : boolean) : boolean;
  end;

  ECryptoProvider = class(Exception);

implementation

end.
