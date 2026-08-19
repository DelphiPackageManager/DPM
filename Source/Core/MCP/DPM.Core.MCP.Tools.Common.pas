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

unit DPM.Core.MCP.Tools.Common;

interface

uses
  VSoft.CancellationToken,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.Core.MCP.Interfaces;

type
  ///<summary> What every tool needs: the feed, the config, and the session defaults. </summary>
  ///<remarks>
  ///  The session defaults exist because a model should not have to state the compiler on
  ///  every call when the answer is knowable - but a WRONG default is far worse than none,
  ///  so every tool result echoes the compiler it actually used.
  ///</remarks>
  IMCPToolContext = interface
    ['{4E2A7D19-8C36-4B51-9F04-6A8D3C1E5B72}']
    function GetRepositoryManager : IPackageRepositoryManager;
    function GetConfiguration : IConfiguration;
    function GetLogger : ILogger;
    function GetFallbackCompiler : TCompilerVersion;
    function GetDefaultProjectPath : string;
    property RepositoryManager : IPackageRepositoryManager read GetRepositoryManager;
    property Configuration : IConfiguration read GetConfiguration;
    property Logger : ILogger read GetLogger;
    ///<summary>
    ///  The --compiler command line value. A FALLBACK, not an override: it applies only when
    ///  no project can answer the question. A value frozen at registration time would
    ///  otherwise silently misreport every project targeting a different Delphi.
    ///</summary>
    property FallbackCompiler : TCompilerVersion read GetFallbackCompiler;
    property DefaultProjectPath : string read GetDefaultProjectPath;
  end;

  TMCPToolContext = class(TInterfacedObject, IMCPToolContext)
  private
    FRepositoryManager : IPackageRepositoryManager;
    FConfiguration : IConfiguration;
    FLogger : ILogger;
    FFallbackCompiler : TCompilerVersion;
    FDefaultProjectPath : string;
  protected
    function GetRepositoryManager : IPackageRepositoryManager;
    function GetConfiguration : IConfiguration;
    function GetLogger : ILogger;
    function GetFallbackCompiler : TCompilerVersion;
    function GetDefaultProjectPath : string;
  public
    constructor Create(const repositoryManager : IPackageRepositoryManager; const configuration : IConfiguration;
                       const logger : ILogger; const fallbackCompiler : TCompilerVersion;
                       const defaultProjectPath : string);
  end;
  ///<summary> Boilerplate for IMCPTool, plus a small JSON Schema builder. </summary>
  TMCPToolBase = class(TInterfacedObject, IMCPTool)
  protected
    function GetName : string; virtual; abstract;
    function GetTitle : string; virtual;
    function GetDescription : string; virtual; abstract;
    function GetIsOpenWorld : boolean; virtual;
    procedure BuildInputSchema(const target : IYAMLMapping); virtual; abstract;
    function Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string; virtual; abstract;

    //Schema helpers. Every property carries a description - the descriptions are the whole
    //interface as far as a model is concerned.
    function BeginSchema(const target : IYAMLMapping) : IYAMLMapping;
    procedure AddStringProperty(const properties : IYAMLMapping; const name, description : string);
    procedure AddBoolProperty(const properties : IYAMLMapping; const name, description : string; const default : boolean);
    procedure AddIntProperty(const properties : IYAMLMapping; const name, description : string;
                             const default, min, max : integer);
    procedure AddRequired(const target : IYAMLMapping; const names : array of string);

    //Deliberate shadows - see TMCPServer. A stray Write in a tool would land in the JSON-RPC
    //stream on stdout.
    procedure WriteLn(const s : string = '');
    procedure Write(const s : string = '');
  end;

///<summary> Returns the argument, raising a tool error naming it when absent or blank. </summary>
function RequireStringArg(const arguments : IYAMLMapping; const name : string) : string;
function OptionalStringArg(const arguments : IYAMLMapping; const name : string; const default : string = '') : string;
function OptionalBoolArg(const arguments : IYAMLMapping; const name : string; const default : boolean) : boolean;
function OptionalIntArg(const arguments : IYAMLMapping; const name : string; const default, min, max : integer) : integer;

///<summary>
///  Parses a compiler version, raising a tool error listing valid values when it does not
///  resolve.
///</summary>
///<remarks>
///  StringToCompilerVersion returns UnknownVersion rather than raising, so an unchecked call
///  turns a typo into a feed query for compiler "unknownversion" that quietly returns nothing.
///</remarks>
function ParseCompilerArg(const value : string) : TCompilerVersion;

///<summary> Parses one platform name, case insensitively. </summary>
///<remarks>
///  Not StringToDPMPlatform: that goes through GetEnumValue and returns UnknownPlatform for
///  anything it does not recognise, and StringToDPMPlatforms then drops unknown tokens
///  silently - so "Win32,Bogus" yields just Win32 with no complaint. A model will write
///  "win32", so this matches against DPMPlatformToString (which is lower case) and raises on
///  anything left over.
///</remarks>
function ParsePlatformArg(const value : string) : TDPMPlatform;

///<summary> Every compiler value we accept, for use in error messages and descriptions. </summary>
function ValidCompilerList : string;
function ValidPlatformList : string;

///<summary>
///  Resolves the projectPath argument, falling back to the session default project. Returns
///  empty when no project was named anywhere; raises when one was named but is unusable.
///</summary>
function TryResolveProjectFile(const arguments : IYAMLMapping; const context : IMCPToolContext) : string;

///<summary> As above, but a project is required, so an absent one is an error. </summary>
function RequireProjectFile(const arguments : IYAMLMapping; const context : IMCPToolContext) : string;

///<summary> Loads a project for reading. Raises a tool error when it cannot be read. </summary>
function LoadProjectEditor(const projectFile : string; const context : IMCPToolContext) : IProjectEditor;

///<summary>
///  Works out which Delphi compiler a call is about, in this order:
///    1. an explicit compiler argument,
///    2. the compiler declared by the projectPath argument's project,
///    3. the compiler declared by the session default project,
///    4. the --compiler fallback.
///  Raises when none of those answer, rather than guessing - packages are published per
///  compiler version, so a wrong guess returns a confidently empty answer.
///
///  Steps 2 and 3 happen per call, not once at startup. An agent can move between projects
///  within a single session, and a compiler cached at launch would go stale silently - which
///  is exactly the trap a --compiler baked into the client registration would create.
///</summary>
function ResolveCompilerArg(const arguments : IYAMLMapping; const context : IMCPToolContext) : TCompilerVersion;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Project.Editor;

function ValidCompilerList : string;
var
  compiler : TCompilerVersion;
begin
  result := '';
  for compiler := Low(TCompilerVersion) to High(TCompilerVersion) do
  begin
    if compiler = TCompilerVersion.UnknownVersion then
      continue;
    if result <> '' then
      result := result + ', ';
    result := result + CompilerToString(compiler);
  end;
end;

function ValidPlatformList : string;
var
  dpmPlatform : TDPMPlatform;
begin
  result := '';
  for dpmPlatform := Low(TDPMPlatform) to High(TDPMPlatform) do
  begin
    if dpmPlatform = TDPMPlatform.UnknownPlatform then
      continue;
    if result <> '' then
      result := result + ', ';
    result := result + DPMPlatformToString(dpmPlatform);
  end;
end;

function RequireStringArg(const arguments : IYAMLMapping; const name : string) : string;
begin
  result := '';
  if arguments <> nil then
    result := arguments.S[name];
  if Trim(result) = '' then
    raise EMCPToolError.Create('Missing required argument "' + name + '".');
  result := Trim(result);
end;

function OptionalStringArg(const arguments : IYAMLMapping; const name : string; const default : string) : string;
begin
  result := default;
  if (arguments <> nil) and arguments.ContainsKey(name) then
  begin
    result := Trim(arguments.S[name]);
    if result = '' then
      result := default;
  end;
end;

function OptionalBoolArg(const arguments : IYAMLMapping; const name : string; const default : boolean) : boolean;
begin
  result := default;
  if (arguments <> nil) and arguments.ContainsKey(name) then
    result := arguments.B[name];
end;

function OptionalIntArg(const arguments : IYAMLMapping; const name : string; const default, min, max : integer) : integer;
begin
  result := default;
  if (arguments <> nil) and arguments.ContainsKey(name) then
    result := arguments.I[name];
  //Clamp rather than reject. A model asking for 1000 results wants "as many as you can give
  //me", and failing the whole call over it helps nobody.
  if result < min then
    result := min;
  if result > max then
    result := max;
end;

function ResolveProjectPath(const candidate : string) : string;
var
  found : TArray<string>;
  names : string;
  i : integer;
begin
  result := candidate;
  if DirectoryExists(result) then
  begin
    found := TDirectory.GetFiles(result, '*.dproj');
    if Length(found) = 0 then
      raise EMCPToolError.Create('No .dproj file found in "' + result +
        '". Pass projectPath with the path to the project file.');
    if Length(found) > 1 then
    begin
      //Naming the candidates is the difference between a dead end and a next step.
      names := '';
      for i := 0 to Length(found) - 1 do
      begin
        if names <> '' then
          names := names + ', ';
        names := names + ExtractFileName(found[i]);
      end;
      raise EMCPToolError.Create('More than one .dproj in "' + result +
        '". Pass projectPath naming one of: ' + names);
    end;
    result := found[0];
  end;

  if not FileExists(result) then
    raise EMCPToolError.Create('Project file not found: "' + result + '".');
end;

function TryResolveProjectFile(const arguments : IYAMLMapping; const context : IMCPToolContext) : string;
var
  candidate : string;
begin
  candidate := OptionalStringArg(arguments, 'projectPath');
  if candidate = '' then
    candidate := context.DefaultProjectPath;
  if candidate = '' then
    exit('');
  result := ResolveProjectPath(candidate);
end;

function RequireProjectFile(const arguments : IYAMLMapping; const context : IMCPToolContext) : string;
var
  candidate : string;
begin
  candidate := OptionalStringArg(arguments, 'projectPath');
  if candidate = '' then
    candidate := context.DefaultProjectPath;
  //Only fall back to the working directory when a project is genuinely required. Guessing one
  //for a package search would be worse than leaving the search unscoped.
  if candidate = '' then
    candidate := GetCurrentDir;
  result := ResolveProjectPath(candidate);
end;

function LoadProjectEditor(const projectFile : string; const context : IMCPToolContext) : IProjectEditor;
begin
  //UnknownVersion on purpose - passing a compiler in suppresses the inference, and what the
  //project itself declares is exactly what is being asked for.
  result := TProjectEditor.Create(context.Logger, context.Configuration, TCompilerVersion.UnknownVersion);
  if not result.LoadProject(projectFile, [TProjectElement.PackageRefs, TProjectElement.DPMCompiler,
                                          TProjectElement.ProjectVersion, TProjectElement.Platforms,
                                          TProjectElement.AppType]) then
    raise EMCPToolError.Create('Could not read the Delphi project "' + projectFile +
      '". It may be malformed or not a .dproj.');
end;

function TryInferCompilerFromProject(const projectFile : string; const context : IMCPToolContext) : TCompilerVersion;
var
  editor : IProjectEditor;
begin
  result := TCompilerVersion.UnknownVersion;
  if projectFile = '' then
    exit;
  try
    editor := TProjectEditor.Create(context.Logger, context.Configuration, TCompilerVersion.UnknownVersion);
    if editor.LoadProject(projectFile, [TProjectElement.DPMCompiler, TProjectElement.ProjectVersion]) then
      result := editor.CompilerVersion;
  except
    //Not fatal here: the caller still has the fallback, and a tool that genuinely needs the
    //project will report the real error when it loads it properly.
    on E : Exception do
      result := TCompilerVersion.UnknownVersion;
  end;
end;

function ResolveCompilerArg(const arguments : IYAMLMapping; const context : IMCPToolContext) : TCompilerVersion;
var
  explicit : string;
  projectFile : string;
begin
  //1. What the caller actually asked for.
  explicit := OptionalStringArg(arguments, 'compiler');
  if explicit <> '' then
    exit(ParseCompilerArg(explicit));

  //2 and 3. Ask the project - per call, so moving between projects works.
  projectFile := '';
  try
    projectFile := TryResolveProjectFile(arguments, context);
  except
    //An unusable projectPath is only fatal for tools that need the project itself; here it
    //just means there is nothing to infer from.
    on E : EMCPToolError do
      projectFile := '';
  end;
  result := TryInferCompilerFromProject(projectFile, context);
  if result <> TCompilerVersion.UnknownVersion then
    exit;

  //4. The --compiler fallback, for when there is no project in play at all.
  result := context.FallbackCompiler;
  if result = TCompilerVersion.UnknownVersion then
    raise EMCPToolError.Create('No Delphi compiler version was given and none could be inferred ' +
      'from a project. Either pass the compiler argument (e.g. "12.0"), or pass projectPath ' +
      'so it can be read from the project. Known compilers: ' + ValidCompilerList + '.');
end;

function ParseCompilerArg(const value : string) : TCompilerVersion;
begin
  result := StringToCompilerVersion(Trim(value));
  if result = TCompilerVersion.UnknownVersion then
    raise EMCPToolError.Create('Unknown Delphi compiler version "' + value + '". ' +
      'Accepted forms include XE2..XE8, 10.0..10.4, 11, 12, 12.3, 13, or the canonical form ' +
      'delphi12.0. Known values: ' + ValidCompilerList + '.');
end;

function ParsePlatformArg(const value : string) : TDPMPlatform;
var
  dpmPlatform : TDPMPlatform;
  wanted : string;
begin
  wanted := LowerCase(Trim(value));
  for dpmPlatform := Low(TDPMPlatform) to High(TDPMPlatform) do
  begin
    if dpmPlatform = TDPMPlatform.UnknownPlatform then
      continue;
    if LowerCase(DPMPlatformToString(dpmPlatform)) = wanted then
      exit(dpmPlatform);
  end;
  raise EMCPToolError.Create('Unknown platform "' + value + '". Valid platforms: ' + ValidPlatformList + '.');
end;

{ TMCPToolContext }

constructor TMCPToolContext.Create(const repositoryManager : IPackageRepositoryManager; const configuration : IConfiguration;
                                   const logger : ILogger; const fallbackCompiler : TCompilerVersion;
                                   const defaultProjectPath : string);
begin
  inherited Create;
  FRepositoryManager := repositoryManager;
  FConfiguration := configuration;
  FLogger := logger;
  FFallbackCompiler := fallbackCompiler;
  FDefaultProjectPath := defaultProjectPath;
end;

function TMCPToolContext.GetRepositoryManager : IPackageRepositoryManager;
begin
  result := FRepositoryManager;
end;

function TMCPToolContext.GetConfiguration : IConfiguration;
begin
  result := FConfiguration;
end;

function TMCPToolContext.GetLogger : ILogger;
begin
  result := FLogger;
end;

function TMCPToolContext.GetFallbackCompiler : TCompilerVersion;
begin
  result := FFallbackCompiler;
end;

function TMCPToolContext.GetDefaultProjectPath : string;
begin
  result := FDefaultProjectPath;
end;

{ TMCPToolBase }

function TMCPToolBase.GetTitle : string;
begin
  result := '';
end;

function TMCPToolBase.GetIsOpenWorld : boolean;
begin
  result := false;
end;

procedure TMCPToolBase.WriteLn(const s : string);
begin
  raise Exception.Create('System.WriteLn called inside an MCP tool - that would corrupt stdout');
end;

procedure TMCPToolBase.Write(const s : string);
begin
  raise Exception.Create('System.Write called inside an MCP tool - that would corrupt stdout');
end;

function TMCPToolBase.BeginSchema(const target : IYAMLMapping) : IYAMLMapping;
begin
  target.AddOrSetValue('type', 'object');
  result := target.AddOrSetMapping('properties');
end;

procedure TMCPToolBase.AddStringProperty(const properties : IYAMLMapping; const name, description : string);
var
  prop : IYAMLMapping;
begin
  prop := properties.AddOrSetMapping(name);
  prop.AddOrSetValue('type', 'string');
  prop.AddOrSetValue('description', description);
end;

procedure TMCPToolBase.AddBoolProperty(const properties : IYAMLMapping; const name, description : string; const default : boolean);
var
  prop : IYAMLMapping;
begin
  prop := properties.AddOrSetMapping(name);
  prop.AddOrSetValue('type', 'boolean');
  prop.AddOrSetValue('description', description);
  prop.AddOrSetValue('default', default);
end;

procedure TMCPToolBase.AddIntProperty(const properties : IYAMLMapping; const name, description : string;
                                      const default, min, max : integer);
var
  prop : IYAMLMapping;
begin
  prop := properties.AddOrSetMapping(name);
  prop.AddOrSetValue('type', 'integer');
  prop.AddOrSetValue('description', description);
  prop.AddOrSetValue('default', default);
  prop.AddOrSetValue('minimum', min);
  prop.AddOrSetValue('maximum', max);
end;

procedure TMCPToolBase.AddRequired(const target : IYAMLMapping; const names : array of string);
var
  seq : IYAMLSequence;
  i : integer;
begin
  seq := target.AddOrSetSequence('required');
  for i := Low(names) to High(names) do
    seq.AddValue(names[i]);
end;

end.
