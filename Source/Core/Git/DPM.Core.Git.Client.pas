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
  DPM.Core.Utils.Process,
  DPM.Core.Git.Interfaces;

type
  TGitClient = class(TInterfacedObject, IGitClient)
  private
    class var
      FTimeoutSeconds : integer;
    class constructor Create;
    //whether the user's git config says which ssh to use (core.sshCommand) - asked once, the first time it matters.
    class function SshCommandConfigured(const exeFile : string) : boolean; static;
  private
    FLogger : ILogger;
    FLastError : string;
    //remembers why a command failed, with what git said - for the caller to report.
    procedure CommandFailed(const what : string; const output : TStrings);
    //runs git.exe (GitExe) with args, capturing stdout+stderr into output.
    //returns the process exit code.
    function RunGit(const cancellationToken : ICancellationToken; const args : string; const workingDir : string; const output : TStrings) : Cardinal;
  protected
    function IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;
    function LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;
    function LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;
    function Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
    function Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
    function Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
    function GetRemoteUrl(const cancellationToken : ICancellationToken; const repoDir : string; out url : string) : boolean;
    function LastError : string;
  public
    constructor Create(const logger : ILogger);

    //The git.exe every command runs, by its full path - '' when git isn't installed. Found once, on the PATH
    //as it was when DPM was loaded: the IDE later adds the folders of the packages it loads to PATH, and a
    //package can put any file in those.
    class function GitExe : string;
    //The first <folder>\git.exe on searchPath (a PATH value). Only absolute folders are searched - a relative
    //entry is relative to the current folder, which for a checkout is a repository someone else wrote - and
    //only for git.exe, never a git.cmd or git.bat.
    class function FindGitExe(const searchPath : string) : string;

    //What every command runs with. DPM's git has no console anyone can see, so a prompt - git asking for a
    //password, the credential manager's window, ssh asking about a host key or a passphrase - would wait until the
    //timeout. ssh runs in batch mode unless the user has said which ssh to use (GIT_SSH_COMMAND, GIT_SSH, or
    //core.sshCommand - sshConfigured) : GIT_SSH_COMMAND would win over theirs.
    class procedure MakeNonInteractive(const env : IEnvironmentBlock; const sshConfigured : boolean); static;
    //How long a command may run before it is stopped - git waiting on a network that has gone, say.
    class property TimeoutSeconds : integer read FTimeoutSeconds write FTimeoutSeconds;

    //Pure parsing helpers - exposed for testing (no git/network required).
    //Parses 'git ls-remote --tags' output into a tagName -> commit map.
    class function ParseTags(const lines : TStrings) : IDictionary<string, string>;
    //Returns the commit sha from the first 'git ls-remote <url> <ref>' line.
    class function ParseRefCommit(const lines : TStrings) : string;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.SyncObjs,
  System.IOUtils,
  System.StrUtils,
  System.Diagnostics,
  DPM.Core.Utils.Strings;

const
  cTagRefPrefix = 'refs/tags/';
  cPeeledSuffix = '^{}';
  cGitExeName = 'git.exe';

var
  //PATH as it was when DPM was loaded - see TGitClient.GitExe.
  StartupSearchPath : string;
  GitExeLock : TCriticalSection;
  GitExeResolved : boolean;
  SshConfigResolved : boolean;
  SshConfigured : boolean;
  GitExePath : string;

function QuoteArg(const value : string) : string;
begin
  result := '"' + value + '"';
end;

//'c:\x' or '\\server\share\x' - not '.', 'bin', '\x' (the root of the current drive) or 'c:x' (relative to
//that drive's current folder).
function IsAbsoluteFolder(const folder : string) : boolean;
begin
  result := (Length(folder) >= 3) and
            ((CharInSet(folder[1], ['a'..'z', 'A'..'Z']) and (folder[2] = ':') and (folder[3] = PathDelim)) or
             ((folder[1] = PathDelim) and (folder[2] = PathDelim)));
end;

{ TGitClient }

class constructor TGitClient.Create;
begin
  FTimeoutSeconds := 5 * 60;
end;

constructor TGitClient.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

procedure TGitClient.CommandFailed(const what : string; const output : TStrings);
var
  i : integer;
  line : string;
begin
  FLastError := what;
  for i := 0 to output.Count - 1 do
  begin
    line := Trim(output.Strings[i]);
    if line <> '' then
      FLastError := FLastError + sLineBreak + '  ' + line;
  end;
  FLogger.Verbose('[git] ' + FLastError);
end;

function TGitClient.LastError : string;
begin
  result := FLastError;
end;

class function TGitClient.FindGitExe(const searchPath : string) : string;
var
  entries : TArray<string>;
  folder : string;
  candidate : string;
  i : integer;
begin
  result := '';
  entries := TStringUtils.SplitStr(searchPath, ';', TSplitStringOptions.ExcludeEmpty);
  for i := 0 to Length(entries) - 1 do
  begin
    folder := Trim(entries[i]);
    //a PATH entry may be quoted.
    if (Length(folder) >= 2) and (folder[1] = '"') and (folder[Length(folder)] = '"') then
      folder := Copy(folder, 2, Length(folder) - 2);
    if not IsAbsoluteFolder(folder) then
      continue;
    candidate := IncludeTrailingPathDelimiter(folder) + cGitExeName;
    if FileExists(candidate) then
      exit(candidate);
  end;
end;

class function TGitClient.GitExe : string;
begin
  GitExeLock.Enter;
  try
    if not GitExeResolved then
    begin
      GitExePath := FindGitExe(StartupSearchPath);
      GitExeResolved := true;
    end;
    result := GitExePath;
  finally
    GitExeLock.Leave;
  end;
end;

class procedure TGitClient.MakeNonInteractive(const env : IEnvironmentBlock; const sshConfigured : boolean);
begin
  env.AddOrSet('GIT_TERMINAL_PROMPT', '0');
  env.AddOrSet('GCM_INTERACTIVE', 'never');
  if sshConfigured or (env.GetValue('GIT_SSH_COMMAND') <> '') or (env.GetValue('GIT_SSH') <> '') then
    exit;
  env.AddOrSet('GIT_SSH_COMMAND', 'ssh -oBatchMode=yes');
end;

class function TGitClient.SshCommandConfigured(const exeFile : string) : boolean;
begin
  GitExeLock.Enter;
  try
    if not SshConfigResolved then
    begin
      //exits 1 when it isn't set. Its own token - a cancelled operation mustn't leave the answer 'no'.
      try
        SshConfigured := TProcess.Execute2(TCancellationTokenSourceFactory.Create.Token, exeFile, 'config --get core.sshCommand', '', nil, 30 * 1000) = 0;
      except
        on e : EOSError do
          SshConfigured := false;
      end;
      SshConfigResolved := true;
    end;
    result := SshConfigured;
  finally
    GitExeLock.Leave;
  end;
end;

function TGitClient.RunGit(const cancellationToken : ICancellationToken; const args : string; const workingDir : string; const output : TStrings) : Cardinal;
var
  tmpFile : string;
  exeFile : string;
  env : IEnvironmentBlock;
  stopwatch : TStopwatch;
  timedOut : boolean;
begin
  output.Clear;
  exeFile := GitExe;
  if exeFile = '' then
  begin
    output.Add('git.exe was not found on the PATH - install Git for Windows (https://git-scm.com), or add the folder git.exe is in to the PATH.');
    result := ERROR_FILE_NOT_FOUND;
    exit;
  end;
  //temp file in the environment TEMP/TMP folder - never the working dir.
  tmpFile := TPath.GetTempFileName;
  try
    //git.exe by its full path, started directly with stdout and stderr going to tmpFile. Never through cmd.exe -
    //cmd looks for 'git' in the current folder before PATH, and the working folder of a checkout is a
    //repository someone else wrote, which could hold a git.cmd.
    FLogger.Debug('[git] ' + exeFile + ' ' + args);
    env := TEnvironmentBlockFactory.Create(nil, true);
    MakeNonInteractive(env, SshCommandConfigured(exeFile));
    try
      stopwatch := TStopwatch.StartNew;
      result := TProcess.Execute2(cancellationToken, exeFile, args, workingDir, env, FTimeoutSeconds * 1000, tmpFile);
      //stopped - by the token, or because it ran out of time.
      timedOut := (result <> 0) and (not cancellationToken.IsCancelled) and (stopwatch.ElapsedMilliseconds >= Int64(FTimeoutSeconds) * 1000);
    except
      //git couldn't be started - that is the output.
      on e : EOSError do
      begin
        output.Add(e.Message);
        result := e.ErrorCode;
        exit;
      end;
    end;
    if TFile.Exists(tmpFile) then
    begin
      try
        output.LoadFromFile(tmpFile);
      except
        //ignore - output is best effort for diagnostics/parsing.
      end;
    end;
    if timedOut then
      output.Insert(0, 'git did not finish within ' + IntToStr(FTimeoutSeconds) + ' seconds - it was stopped.');
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
begin
  tags := TCollections.CreateDictionary<string, string>;
  output := TStringList.Create;
  try
    FLastError := '';
    //'--' - the url comes from a registry entry, and one starting with '-' would be read as an option
    //('--upload-pack=<program>').
    result := RunGit(cancellationToken, 'ls-remote --tags -- ' + QuoteArg(url), '', output) = 0;
    if not result then
    begin
      CommandFailed('git ls-remote --tags failed for [' + url + ']', output);
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
begin
  commit := '';
  if ref <> '' then
    theRef := ref
  else
    theRef := 'HEAD';
  output := TStringList.Create;
  try
    FLastError := '';
    result := RunGit(cancellationToken, 'ls-remote -- ' + QuoteArg(url) + ' ' + QuoteArg(theRef), '', output) = 0;
    if not result then
    begin
      CommandFailed('git ls-remote failed for [' + url + '] ref [' + theRef + ']', output);
      exit;
    end;
    commit := ParseRefCommit(output);
    result := commit <> '';
    if not result then
      CommandFailed('git ls-remote found no commit for [' + url + '] ref [' + theRef + ']', output);
  finally
    output.Free;
  end;
end;

function TGitClient.Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
var
  output : TStringList;
begin
  output := TStringList.Create;
  try
    FLastError := '';
    result := RunGit(cancellationToken, 'clone -- ' + QuoteArg(url) + ' ' + QuoteArg(targetDir), '', output) = 0;
    if not result then
      CommandFailed('git clone failed for [' + url + ']', output);
  finally
    output.Free;
  end;
end;

function TGitClient.Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
var
  output : TStringList;
begin
  output := TStringList.Create;
  try
    FLastError := '';
    result := RunGit(cancellationToken, 'pull --ff-only', repoDir, output) = 0;
    if not result then
      CommandFailed('git pull failed in [' + repoDir + ']', output);
  finally
    output.Free;
  end;
end;

function TGitClient.GetRemoteUrl(const cancellationToken : ICancellationToken; const repoDir : string; out url : string) : boolean;
var
  output : TStringList;
  i : integer;
begin
  url := '';
  output := TStringList.Create;
  try
    FLastError := '';
    result := RunGit(cancellationToken, 'config --get remote.origin.url', repoDir, output) = 0;
    if result then
    begin
      for i := 0 to output.Count - 1 do
      begin
        url := Trim(output.Strings[i]);
        if url <> '' then
          break;
      end;
      result := url <> '';
    end;
    if not result then
      CommandFailed('git config --get remote.origin.url failed in [' + repoDir + ']', output);
  finally
    output.Free;
  end;
end;

function TGitClient.Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
var
  output : TStringList;
begin
  FLastError := '';
  //checkout can't take '--' in front of the commit (what follows it are paths), so a value git would read
  //as an option is refused instead.
  if StartsStr('-', commitOrRef) then
  begin
    FLastError := 'git checkout refused [' + commitOrRef + '] - it is not a commit or ref.';
    FLogger.Verbose('[git] ' + FLastError);
    exit(false);
  end;
  output := TStringList.Create;
  try
    result := RunGit(cancellationToken, 'checkout --quiet ' + QuoteArg(commitOrRef), repoDir, output) = 0;
    if not result then
      CommandFailed('git checkout [' + commitOrRef + '] failed in [' + repoDir + ']', output);
  finally
    output.Free;
  end;
end;

initialization
  //before anything - the IDE plugin included - has had a chance to change it.
  StartupSearchPath := GetEnvironmentVariable('PATH');
  GitExeLock := TCriticalSection.Create;

finalization
  GitExeLock.Free;

end.
