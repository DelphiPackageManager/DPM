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

unit DPM.Core.Utils.Base64Url;

// RFC 4648 §5 base64url codec. Standard base64 with `-`/`_` substituting for
// `+`/`/`, and trailing `=` padding stripped. JWS/JWA/Azure Key Vault all
// use this encoding for binary payloads embedded in JSON. Routed through
// VSoft.Base64 so callers compile on XE2..XE6 (no System.NetEncoding).

interface

uses
  System.SysUtils;

type
  TBase64Url = record
  public
    class function Encode(const bytes : TBytes) : string; static;
    class function Decode(const value : string) : TBytes; static;
  end;

  EBase64Url = class(Exception);

implementation

uses
  VSoft.Base64;

class function TBase64Url.Encode(const bytes : TBytes) : string;
var
  s : string;
begin
  s := TBase64.Encode(bytes, false);   // no line breaks
  s := StringReplace(s, '+', '-', [rfReplaceAll]);
  s := StringReplace(s, '/', '_', [rfReplaceAll]);
  while (Length(s) > 0) and (s[Length(s)] = '=') do
    SetLength(s, Length(s) - 1);
  result := s;
end;

class function TBase64Url.Decode(const value : string) : TBytes;
var
  s : string;
  pad : integer;
begin
  s := StringReplace(value, '-', '+', [rfReplaceAll]);
  s := StringReplace(s, '_', '/', [rfReplaceAll]);
  pad := Length(s) mod 4;
  if pad = 2 then
    s := s + '=='
  else if pad = 3 then
    s := s + '='
  else if pad <> 0 then
    raise EBase64Url.CreateFmt('Invalid base64url length: %d', [Length(value)]);
  result := TBase64.Decode(s);
end;

end.
