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

unit DPM.Console.Command.MCP;

interface

uses
  VSoft.CancellationToken,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  ///<summary>
  ///  Runs a read only MCP server over stdio, so an AI coding agent can search DPM packages
  ///  and understand a Delphi project.
  ///</summary>
  ///<remarks>
  ///  Read only by construction: no tool here installs, uninstalls, restores or writes
  ///  anything. That removes trust prompts, multi minute MSBuild runs against client timeouts,
  ///  and any possibility of corrupting a .dproj. When the agent decides to act it runs
  ///  dpm install in its own shell.
  ///</remarks>
  TMCPCommand = class(TBaseCommand)
  private
    FRepositoryManager : IPackageRepositoryManager;
    function ResolveDefaultProjectPath : string;
    function BuildInstructions(const fallbackCompiler : TCompilerVersion; const projectPath : string) : string;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
    function ForceNoBanner : boolean; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager;
                       const repositoryManager : IPackageRepositoryManager); reintroduce;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Options.Common,
  DPM.Core.Options.MCP,
  DPM.Core.Version,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.Editor,
  DPM.Core.MCP.Interfaces,
  DPM.Core.MCP.Server,
  DPM.Core.MCP.Tools.Common,
  DPM.Core.MCP.Tools.Packages,
  DPM.Core.MCP.Tools.Project,
  DPM.Core.MCP.Transport.Logging,
  DPM.Console.MCP.StdioTransport;

{ TMCPCommand }

constructor TMCPCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager;
                               const repositoryManager : IPackageRepositoryManager);
begin
  inherited Create(logger, configurationManager);
  FRepositoryManager := repositoryManager;
end;

function TMCPCommand.ForceNoBanner : boolean;
begin
  //Unconditional. stdout carries JSON-RPC and nothing else for the life of the process.
  result := true;
end;

function TMCPCommand.ResolveDefaultProjectPath : string;
var
  found : TArray<string>;
begin
  result := TMCPOptions.Default.ProjectPath;
  if result <> '' then
    exit;

  //Only when it is unambiguous. Picking one of several would be a guess the caller could not
  //see, and the project tools name the candidates when asked without one.
  found := TDirectory.GetFiles(GetCurrentDir, '*.dproj');
  if Length(found) = 1 then
    result := found[0];
end;

function TMCPCommand.BuildInstructions(const fallbackCompiler : TCompilerVersion; const projectPath : string) : string;
begin
  //Cheap and high leverage: this sits in the model's context for the whole session, so it is
  //the right place for the things it would otherwise have to guess or spend a tool call on.
  result :=
    'DPM is the package manager for Embarcadero Delphi (Object Pascal) - it is to Delphi what ' +
    'npm is to Node or NuGet is to .NET. ' + sLineBreak +
    sLineBreak +
    'Key things to know:' + sLineBreak +
    '- Packages are published separately for each Delphi compiler version. Every query is ' +
    'scoped to one, and every result states which compiler it used.' + sLineBreak +
    '- Compiler versions may be written as "12.0", "12", "XE2", or canonically "delphi12.0".' + sLineBreak +
    '- Platforms are names like Win32, Win64, Linux64, Android64, iOS64, MacOS64.' + sLineBreak +
    '- Start with dpm_project_info when working in a project: it reports the compiler version ' +
    'and platforms the other tools should use.' + sLineBreak +
    sLineBreak +
    'Every tool here is READ ONLY - nothing installs, downloads or modifies a project. ' +
    'To actually add a package, run the CLI in a shell:' + sLineBreak +
    '  dpm install <PackageId> <Project.dproj> --compiler=<version>' + sLineBreak;

  result := result +
    'How the compiler version is chosen, in order: the compiler argument if you pass one; ' +
    'otherwise the compiler declared by projectPath; otherwise the project this server was ' +
    'started in. Pass projectPath when you are working on a specific project and it will ' +
    'always match, even across several projects in one session.' + sLineBreak;

  if projectPath <> '' then
    result := result + sLineBreak + 'Project this server was started in: ' + ExtractFileName(projectPath) + '.';

  if fallbackCompiler <> TCompilerVersion.UnknownVersion then
    result := result + sLineBreak +
      'Fallback compiler when no project is in play: ' + CompilerToString(fallbackCompiler) + '.';
end;

function TMCPCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TMCPOptions;
  config : IConfiguration;
  registry : IMCPToolRegistry;
  context : IMCPToolContext;
  server : TMCPServer;
  reader : IMCPMessageReader;
  writer : IMCPMessageWriter;
  frameLog : TMCPFrameLog;
  defaultProject : string;
  fallbackCompiler : TCompilerVersion;
begin
  result := TExitCode.Error;

  options := TMCPOptions.Default;
  options.ApplyCommon(TCommonOptions.Default);
  if not options.Validate(Logger) then
    exit(TExitCode.InvalidArguments);

  config := FConfigurationManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit(TExitCode.InitException);

  if not FRepositoryManager.Initialize(config) then
  begin
    Logger.Error('mcp: unable to initialize package sources');
    exit(TExitCode.InitException);
  end;

  defaultProject := ResolveDefaultProjectPath;
  //Only the --compiler flag. Anything project derived is resolved per call, so that
  //moving between projects in one session cannot pick up a stale value.
  fallbackCompiler := TMCPOptions.Default.CompilerVersion;

  //Diagnostics go to stderr - see TStdErrConsole. Safe to log freely here.
  Logger.Information('dpm mcp server starting');
  if fallbackCompiler <> TCompilerVersion.UnknownVersion then
    Logger.Information('  fallback compiler : ' + CompilerToString(fallbackCompiler));
  if defaultProject <> '' then
    Logger.Information('  default project  : ' + defaultProject);

  context := TMCPToolContext.Create(FRepositoryManager, config, Logger, fallbackCompiler, defaultProject);

  registry := TMCPToolRegistry.Create;
  //Registration order is the order tools/list reports them, which the spec asks to be stable.
  registry.Add(TMCPSearchPackagesTool.Create(context));
  registry.Add(TMCPPackageInfoTool.Create(context));
  registry.Add(TMCPPackageVersionsTool.Create(context));
  registry.Add(TMCPProjectInfoTool.Create(context));
  registry.Add(TMCPWhyPackageTool.Create(context));

  reader := TMCPStdioReader.Create;
  writer := TMCPStdioWriter.Create;

  //Wrapping the transport rather than logging inside the server keeps the raw frames
  //exactly as they crossed the wire - before parsing on the way in, after serialising on
  //the way out - which is what makes the log worth reading.
  frameLog := nil;
  if options.LogFile <> '' then
  begin
    frameLog := TMCPFrameLog.Create(options.LogFile, Logger);
    frameLog.LogNote('dpm mcp starting, version ' + TDPMVersion.CurrentVersionString);
    reader := TMCPLoggingReader.Create(reader, frameLog);
    writer := TMCPLoggingWriter.Create(writer, frameLog);
    //The expanded path, not what was typed - they differ whenever %VARS% were used.
    Logger.Information('mcp: logging frames to ' + frameLog.FileName);
  end;

  server := TMCPServer.Create(reader, writer, registry, Logger,
                              BuildInstructions(fallbackCompiler, defaultProject));
  try
    try
      server.Run(cancellationToken);
      result := TExitCode.OK;
    except
      //Nothing may escape into the RTL exception handler in dpm.dpr, which writes to stdout.
      on E : Exception do
      begin
        Logger.Error('mcp: ' + E.ClassName + ': ' + E.Message);
        result := TExitCode.Error;
      end;
    end;
  finally
    server.Free;
    //After the server, so nothing can still be writing to it.
    reader := nil;
    writer := nil;
    frameLog.Free;
  end;
end;

end.
