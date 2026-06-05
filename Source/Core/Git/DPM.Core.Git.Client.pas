{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Git.Client;

interface

uses
  System.Classes,
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Git.Interfaces;

type
  TGitClient = class(TInterfacedObject, IGitClient)
  private
    FLogger : ILogger;
    //runs 'git <args>' via cmd.exe, capturing stdout+stderr into output.
    //returns the process exit code.
    function RunGit(const cancellationToken : ICancellationToken; const args : string; const workingDir : string; const output : TStrings) : Cardinal;
  protected
    function IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;
    function LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;
    function LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;
    function Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
    function Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
    function Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
  public
    constructor Create(const logger : ILogger);

    //Pure parsing helpers - exposed for testing (no git/network required).
    //Parses 'git ls-remote --tags' output into a tagName -> commit map.
    class function ParseTags(const lines : TStrings) : IDictionary<string, string>;
    //Returns the commit sha from the first 'git ls-remote <url> <ref>' line.
    class function ParseRefCommit(const lines : TStrings) : string;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.StrUtils,
  DPM.Core.Utils.Process;

const
  cTagRefPrefix = 'refs/tags/';
  cPeeledSuffix = '^{}';

function QuoteArg(const value : string) : string;
begin
  result := '"' + value + '"';
end;

{ TGitClient }

constructor TGitClient.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

function TGitClient.RunGit(const cancellationToken : ICancellationToken; const args : string; const workingDir : string; const output : TStrings) : Cardinal;
var
  tmpFile : string;
  commandLine : string;
begin
  output.Clear;
  //temp file in the environment TEMP/TMP folder - never the working dir.
  tmpFile := TPath.GetTempFileName;
  try
    //Execute2 prefixes the executable, producing: "cmd.exe" /c git <args> > "tmp" 2>&1
    //The text after '/c ' does not start with a quote, so cmd's outer-quote
    //stripping rule is not triggered and the redirection works as expected.
    commandLine := '/c git ' + args + ' > ' + QuoteArg(tmpFile) + ' 2>&1';
    FLogger.Debug('[git] git ' + args);
    result := TProcess.Execute2(cancellationToken, 'cmd.exe', commandLine, workingDir);
    if TFile.Exists(tmpFile) then
    begin
      try
        output.LoadFromFile(tmpFile);
      except
        //ignore - output is best effort for diagnostics/parsing.
      end;
    end;
  finally
    if TFile.Exists(tmpFile) then
    begin
      try
        TFile.Delete(tmpFile);
      except
        //ignore
      end;
    end;
  end;
end;

class function TGitClient.ParseTags(const lines : TStrings) : IDictionary<string, string>;
var
  i : integer;
  line : string;
  sha : string;
  ref : string;
  tagName : string;
  tabPos : integer;
  peeled : boolean;
begin
  result := TCollections.CreateDictionary<string, string>;
  for i := 0 to lines.Count - 1 do
  begin
    line := lines.Strings[i];
    tabPos := Pos(#9, line);
    if tabPos <= 0 then
      continue;
    sha := Trim(Copy(line, 1, tabPos - 1));
    ref := Trim(Copy(line, tabPos + 1, MaxInt));
    if not StartsText(cTagRefPrefix, ref) then
      continue;
    tagName := Copy(ref, Length(cTagRefPrefix) + 1, MaxInt);
    peeled := EndsText(cPeeledSuffix, tagName);
    if peeled then
      tagName := Copy(tagName, 1, Length(tagName) - Length(cPeeledSuffix));
    if tagName = '' then
      continue;
    //The peeled (^{}) line carries the commit an annotated tag points at and
    //always wins. A lightweight tag has no peeled line, so its own sha (the
    //commit) is used unless a peeled value was already recorded.
    if peeled then
      result[tagName] := sha
    else if not result.ContainsKey(tagName) then
      result[tagName] := sha;
  end;
end;

class function TGitClient.ParseRefCommit(const lines : TStrings) : string;
var
  i : integer;
  line : string;
  tabPos : integer;
begin
  result := '';
  for i := 0 to lines.Count - 1 do
  begin
    line := lines.Strings[i];
    tabPos := Pos(#9, line);
    if tabPos > 0 then
    begin
      result := Trim(Copy(line, 1, tabPos - 1));
      if result <> '' then
        exit;
    end;
  end;
end;

function TGitClient.IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;
var
  output : TStringList;
begin
  output := TStringList.Create;
  try
    result := RunGit(cancellationToken, '--version', '', output) = 0;
    if not result then
      FLogger.Debug('[git] git executable not found on PATH.');
  finally
    output.Free;
  end;
end;

function TGitClient.LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;
var
  output : TStringList;
  i : integer;
begin
  tags := TCollections.CreateDictionary<string, string>;
  output := TStringList.Create;
  try
    result := RunGit(cancellationToken, 'ls-remote --tags ' + QuoteArg(url), '', output) = 0;
    if not result then
    begin
      FLogger.Error('git ls-remote failed for [' + url + ']');
      for i := 0 to output.Count - 1 do
        FLogger.Error(output.Strings[i]);
      exit;
    end;
    tags := ParseTags(output);
  finally
    output.Free;
  end;
end;

function TGitClient.LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;
var
  output : TStringList;
  theRef : string;
  i : integer;
begin
  commit := '';
  if ref <> '' then
    theRef := ref
  else
    theRef := 'HEAD';
  output := TStringList.Create;
  try
    result := RunGit(cancellationToken, 'ls-remote ' + QuoteArg(url) + ' ' + QuoteArg(theRef), '', output) = 0;
    if not result then
    begin
      FLogger.Error('git ls-remote failed for [' + url + '] ref [' + theRef + ']');
      for i := 0 to output.Count - 1 do
        FLogger.Error(output.Strings[i]);
      exit;
    end;
    commit := ParseRefCommit(output);
    result := commit <> '';
  finally
    output.Free;
  end;
end;

function TGitClient.Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
var
  output : TStringList;
  i : integer;
begin
  output := TStringList.Create;
  try
    result := RunGit(cancellationToken, 'clone ' + QuoteArg(url) + ' ' + QuoteArg(targetDir), '', output) = 0;
    if not result then
    begin
      FLogger.Error('git clone failed for [' + url + ']');
      for i := 0 to output.Count - 1 do
        FLogger.Error(output.Strings[i]);
    end;
  finally
    output.Free;
  end;
end;

function TGitClient.Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
var
  output : TStringList;
  i : integer;
begin
  output := TStringList.Create;
  try
    result := RunGit(cancellationToken, 'pull --ff-only', repoDir, output) = 0;
    if not result then
    begin
      FLogger.Error('git pull failed in [' + repoDir + ']');
      for i := 0 to output.Count - 1 do
        FLogger.Error(output.Strings[i]);
    end;
  finally
    output.Free;
  end;
end;

function TGitClient.Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
var
  output : TStringList;
  i : integer;
begin
  output := TStringList.Create;
  try
    result := RunGit(cancellationToken, 'checkout --quiet ' + QuoteArg(commitOrRef), repoDir, output) = 0;
    if not result then
    begin
      FLogger.Error('git checkout [' + commitOrRef + '] failed in [' + repoDir + ']');
      for i := 0 to output.Count - 1 do
        FLogger.Error(output.Strings[i]);
    end;
  finally
    output.Free;
  end;
end;

end.
