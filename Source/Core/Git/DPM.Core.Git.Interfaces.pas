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

unit DPM.Core.Git.Interfaces;

interface

uses
  VSoft.CancellationToken,
  Spring.Collections;

type
  //A thin wrapper around the git command line, used by the git registry
  //repository to discover versions (tags) and fetch package source. Lives in
  //Core (no VCL) - shells out to git.exe via DPM.Core.Utils.Process.
  IGitClient = interface
  ['{D7F3A2B1-4C5E-4A6F-9B8C-1E2D3F4A5B6C}']
    //returns true if a usable git executable is on the PATH.
    function IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;

    //git ls-remote --tags <url>. Returns a map of tagName -> commit sha. For
    //annotated tags the peeled (^{}) commit is returned; for lightweight tags
    //the tag's own sha (which is the commit) is returned.
    function LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;

    //git ls-remote <url> <ref>. Returns the commit sha the ref resolves to.
    //When ref is empty, HEAD (the default branch tip) is used.
    function LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;

    //git clone <url> <targetDir> (full clone so any commit can be checked out).
    function Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;

    //git pull --ff-only in repoDir (used to refresh a cloned registry mirror).
    function Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;

    //git checkout <commitOrRef> in repoDir.
    function Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
  end;

  TGitUtils = class
    /// <summary>Strips VCS keyword decoration from a commit id, so
    /// 'Id:0fe5fa48...', 'Id: 0fe5fa48...' and '$Id: 0fe5fa48... $' all reduce to the
    /// bare revision. Publishers whose build stamps the dspec via keyword expansion
    /// ship the decorated form, and it travels with the package - a decorated id makes
    /// an invalid purl vcs_url and an invalid SPDX download location. Anything that
    /// isn't decorated (including the unreplaced '#HASH#' placeholder) comes back
    /// unchanged apart from trimming.</summary>
    class function NormalizeCommitId(const value : string) : string; static;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils;

{ TGitUtils }

class function TGitUtils.NormalizeCommitId(const value : string) : string;
const
  cIdKeyword = 'Id:';
begin
  result := Trim(value);
  if result = '' then
    exit;

  //'$Id: <sha> $' - the git/svn keyword in its full form.
  if (result[1] = '$') and (result[Length(result)] = '$') then
  begin
    result := Trim(Copy(result, 2, Length(result) - 2));
    if result = '' then
      exit;
  end;

  //'Id' alone is not the keyword - only strip when the colon follows it, so a commit
  //ref that merely starts with those letters survives.
  if StartsText(cIdKeyword, result) then
    result := Trim(Copy(result, Length(cIdKeyword) + 1, MaxInt));
end;

end.
