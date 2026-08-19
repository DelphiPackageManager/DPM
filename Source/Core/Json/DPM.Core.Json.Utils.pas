{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Json.Utils;

interface

uses
  System.Classes,
  Spring.Collections,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Dependency.Version;

type
  /// <summary>
  ///   Helpers for building JSON documents with VSoft.YAML.
  /// </summary>
  /// <remarks>
  ///   Empty strings : IYAMLMapping.AddOrSetValue(key, '') stores a *null*, because a Delphi
  ///   string parameter cannot distinguish '' from nil - VSoft.YAML documents this on
  ///   AddOrSetNull. Every string helper here therefore OMITS the key when the value is empty
  ///   rather than emitting null. Consumers tolerate a missing optional field far better than
  ///   a null (schema validators in particular), and it preserves the behaviour of the
  ///   SetStringIfNotEmpty helper these were extracted from. Call AddOrSetNull directly on the
  ///   mapping when a null is genuinely what you mean.
  ///
  ///   Sequences do not share this quirk - IYAMLSequence.AddValue('') stores a real empty
  ///   string - but the list helpers here still skip empty entries, since an empty tag or
  ///   author is noise rather than data.
  ///
  ///   Empty *collections* are emitted as [] rather than omitted. A consumer testing
  ///   dependencies.length should not have to special case a missing key.
  /// </remarks>
  TJsonUtils = record
  public
    /// <summary> Adds key only when value is non empty. See the type remarks. </summary>
    class procedure AddIfNotEmpty(const target : IYAMLMapping; const key : string; const value : string); static;

    /// <summary> Adds the version as its canonical string, omitting the key when empty. </summary>
    class procedure AddVersion(const target : IYAMLMapping; const key : string; const value : TPackageVersion); static;

    /// <summary> Adds the range as its canonical string, omitting the key when empty. </summary>
    class procedure AddVersionRange(const target : IYAMLMapping; const key : string; const value : TVersionRange); static;

    /// <summary> Adds the platforms as a JSON array of platform names. Always emitted, [] when the set is empty. </summary>
    class procedure AddPlatforms(const target : IYAMLMapping; const key : string; const value : TDPMPlatforms); static;

    /// <summary> Adds the compiler in wire form (delphi12.0), omitting the key when UnknownVersion. </summary>
    class procedure AddCompiler(const target : IYAMLMapping; const key : string; const value : TCompilerVersion); static;

    /// <summary> Adds a JSON array of strings, skipping empty entries. Always emitted, [] when there are none. </summary>
    class procedure AddStrings(const target : IYAMLMapping; const key : string; const values : IList<string>); overload; static;
    class procedure AddStrings(const target : IYAMLMapping; const key : string; const values : TStrings); overload; static;

    /// <summary> Serialises compactly - no whitespace, no line breaks. </summary>
    /// <remarks>
    ///   VSoft.YAML defaults PrettyPrint to TRUE, so this must be set explicitly every time.
    ///   Anything that frames JSON by line - the MCP server in particular - MUST use this,
    ///   since pretty printing would put raw newlines inside a frame.
    /// </remarks>
    class function ToCompactJson(const doc : IYAMLDocument) : string; static;

    /// <summary> Serialises indented, for human consumption. </summary>
    class function ToPrettyJson(const doc : IYAMLDocument) : string; static;

    /// <summary> Writes content to fileName as UTF-8. Extracted from the SBOM and Vuln writers, which each had a private copy. </summary>
    class procedure WriteUtf8(const fileName : string; const content : string); static;
  end;

implementation

uses
  System.SysUtils;

{ TJsonUtils }

class procedure TJsonUtils.AddIfNotEmpty(const target : IYAMLMapping; const key : string; const value : string);
begin
  if value <> '' then
    target.AddOrSetValue(key, value);
end;

class procedure TJsonUtils.AddVersion(const target : IYAMLMapping; const key : string; const value : TPackageVersion);
begin
  if not value.IsEmpty then
    target.AddOrSetValue(key, value.ToStringNoMeta);
end;

class procedure TJsonUtils.AddVersionRange(const target : IYAMLMapping; const key : string; const value : TVersionRange);
begin
  if not value.IsEmpty then
    target.AddOrSetValue(key, value.ToString);
end;

class procedure TJsonUtils.AddPlatforms(const target : IYAMLMapping; const key : string; const value : TDPMPlatforms);
var
  seq : IYAMLSequence;
  dpmPlatform : TDPMPlatform;
begin
  seq := target.AddOrSetSequence(key);
  for dpmPlatform := Low(TDPMPlatform) to High(TDPMPlatform) do
  begin
    if dpmPlatform = TDPMPlatform.UnknownPlatform then
      continue;
    if dpmPlatform in value then
      seq.AddValue(DPMPlatformToString(dpmPlatform));
  end;
end;

class procedure TJsonUtils.AddCompiler(const target : IYAMLMapping; const key : string; const value : TCompilerVersion);
begin
  if value <> TCompilerVersion.UnknownVersion then
    target.AddOrSetValue(key, CompilerToString(value));
end;

class procedure TJsonUtils.AddStrings(const target : IYAMLMapping; const key : string; const values : IList<string>);
var
  seq : IYAMLSequence;
  value : string;
begin
  seq := target.AddOrSetSequence(key);
  if values = nil then
    exit;
  for value in values do
  begin
    if value <> '' then
      seq.AddValue(value);
  end;
end;

class procedure TJsonUtils.AddStrings(const target : IYAMLMapping; const key : string; const values : TStrings);
var
  seq : IYAMLSequence;
  i : integer;
begin
  seq := target.AddOrSetSequence(key);
  if values = nil then
    exit;
  for i := 0 to values.Count - 1 do
  begin
    if values[i] <> '' then
      seq.AddValue(values[i]);
  end;
end;

class function TJsonUtils.ToCompactJson(const doc : IYAMLDocument) : string;
begin
  //PrettyPrint defaults to true - it must be turned off explicitly, and the JSON writer
  //snapshots the option when it is constructed, so this has to happen before WriteToJSONString.
  doc.Options.PrettyPrint := false;
  result := TYAML.WriteToJSONString(doc);
end;

class function TJsonUtils.ToPrettyJson(const doc : IYAMLDocument) : string;
begin
  doc.Options.PrettyPrint := true;
  result := TYAML.WriteToJSONString(doc);
end;

class procedure TJsonUtils.WriteUtf8(const fileName : string; const content : string);
var
  bytes : TBytes;
  fs : TFileStream;
begin
  bytes := TEncoding.UTF8.GetBytes(content);
  fs := TFileStream.Create(fileName, fmCreate);
  try
    if Length(bytes) > 0 then
      fs.WriteBuffer(bytes[0], Length(bytes));
  finally
    fs.Free;
  end;
end;

end.
