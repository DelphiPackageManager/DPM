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

unit DPM.Core.Diagnostics;

//The codes for every error and warning DPM reports - 'error DPME1101: ...', 'warning DPMW1004: ...'.
//docs\error-codes.md explains what each one means and how to fix it.

interface

uses
  System.SysUtils;

{$SCOPEDENUMS ON}
//The catalogues will grow past 256 entries, so store the code enums as 4 bytes. The size belongs to the
//type, so records holding a code pick it up wherever they are declared. Never declare a set of either
//enum - sets are limited to 256 elements.
{$MINENUMSIZE 4}

type
  //Every error DPM reports.
  // - the identifier carries the number, so a reported code can be grepped for.
  // - errors and warnings share one number space - a number belongs to one code only.
  // - the thousands digit is the subsystem, the hundreds digit an area within it :
  //     0 general/cli/options/config, 1 sources/repositories, 2 dependency resolution,
  //     3 install/restore/uninstall/cache, 4 project files, 5 compiling, 6 spec/pack/push,
  //     7 signing/trust/verify, 8 sbom/vulnerabilities, 9 ide/dspec creator/mcp.
  // - never renumber or reuse a released code - retire it (keep the entry, stop using it).
  // - no explicit ordinal values - an enum with them has no RTTI, which the tests rely on.
  // - keep the list in number order, and add the matching row to cErrorInfo and docs\error-codes.md.
  TErrorCode = (
    E0001_UnexpectedException,
    E0002_FailedWithoutDetail,
    E0003_InternalError
  );

  //Every warning DPM reports - the same rules as TErrorCode.
  TWarningCode = (
    W0004_OperationCancelled
  );

  TDiagnosticInfo = record
    Number : integer;
    Title : string;
  end;

const
  cErrorCodePrefix = 'DPME';
  cWarningCodePrefix = 'DPMW';

  cErrorInfo : array[TErrorCode] of TDiagnosticInfo = (
    (Number : 1; Title : 'An unexpected exception occurred'),
    (Number : 2; Title : 'The operation failed without reporting why'),
    (Number : 3; Title : 'Internal error')
  );

  cWarningInfo : array[TWarningCode] of TDiagnosticInfo = (
    (Number : 4; Title : 'The operation was cancelled')
  );

//'DPME0001'
function ErrorCodeToString(const code : TErrorCode) : string;

//'DPMW0004'
function WarningCodeToString(const code : TWarningCode) : string;

//'error DPME0001: message' - MSBuild's canonical format, which CI log parsers recognise.
function FormatError(const code : TErrorCode; const message : string) : string;

//'warning DPMW0004: message'
function FormatWarning(const code : TWarningCode; const message : string) : string;

//'error DPME0001: message' / 'warning DPMW0004: message', or 'error: message' when code is empty -
//errors and warnings that have not been given a code yet.
function FormatDiagnostic(const isError : boolean; const code : string; const message : string) : string;

type
  ///<summary> Raise this rather than logging an error and then raising - whoever catches it reports it
  ///  once, with its code (LogException in DPM.Core.Logging). </summary>
  EDPMError = class(Exception)
  private
    FCode : TErrorCode;
  public
    //these hide Exception.Create/CreateFmt on purpose - an EDPMError always has a code.
    constructor Create(const code : TErrorCode; const msg : string);
    constructor CreateFmt(const code : TErrorCode; const msg : string; const args : array of const);
    property Code : TErrorCode read FCode;
  end;

implementation

function ErrorCodeToString(const code : TErrorCode) : string;
begin
  result := cErrorCodePrefix + Format('%.4d', [cErrorInfo[code].Number]);
end;

function WarningCodeToString(const code : TWarningCode) : string;
begin
  result := cWarningCodePrefix + Format('%.4d', [cWarningInfo[code].Number]);
end;

function FormatDiagnostic(const isError : boolean; const code : string; const message : string) : string;
begin
  if isError then
    result := 'error'
  else
    result := 'warning';
  if code <> '' then
    result := result + ' ' + code;
  result := result + ': ' + message;
end;

function FormatError(const code : TErrorCode; const message : string) : string;
begin
  result := FormatDiagnostic(true, ErrorCodeToString(code), message);
end;

function FormatWarning(const code : TWarningCode; const message : string) : string;
begin
  result := FormatDiagnostic(false, WarningCodeToString(code), message);
end;

{ EDPMError }

constructor EDPMError.Create(const code : TErrorCode; const msg : string);
begin
  inherited Create(msg);
  FCode := code;
end;

constructor EDPMError.CreateFmt(const code : TErrorCode; const msg : string; const args : array of const);
begin
  inherited CreateFmt(msg, args);
  FCode := code;
end;

end.
