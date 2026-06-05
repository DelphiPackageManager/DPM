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

// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program dpm;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IFDEF JCLDEBUG}
  JclDebug,
  {$ENDIF }
  System.SysUtils,
  System.Diagnostics,
  WinApi.ActiveX,
  DPM.Console.Types in 'Cmdline\DPM.Console.Types.pas',
  DPM.Console.Reg in 'Cmdline\DPM.Console.Reg.pas',
  DPM.Console.Application in 'Cmdline\DPM.Console.Application.pas',
  DPM.Console.Banner in 'Cmdline\DPM.Console.Banner.pas',
  DPM.Console.Options in 'Cmdline\Options\DPM.Console.Options.pas',
  DPM.Console.Options.Reg in 'Cmdline\Options\DPM.Console.Options.Reg.pas',
  DPM.Console.Writer in 'Cmdline\Writer\DPM.Console.Writer.pas',
  DPM.Console.Writer.Windows in 'Cmdline\Writer\DPM.Console.Writer.Windows.pas',
  DPM.Console.ExitCodes in 'Cmdline\DPM.Console.ExitCodes.pas',
  DPM.Console.Logger in 'Cmdline\Logging\DPM.Console.Logger.pas',
  DPM.Core.Cache.Interfaces in 'Core\Cache\DPM.Core.Cache.Interfaces.pas',
  DPM.Core.Cache in 'Core\Cache\DPM.Core.Cache.pas',
  DPM.Core.Compiler.EnvironmentProvider in 'Core\Compiler\DPM.Core.Compiler.EnvironmentProvider.pas',
  DPM.Core.Compiler.Factory in 'Core\Compiler\DPM.Core.Compiler.Factory.pas',
  DPM.Core.Compiler.Interfaces in 'Core\Compiler\DPM.Core.Compiler.Interfaces.pas',
  DPM.Core.Compiler.MSBuild in 'Core\Compiler\DPM.Core.Compiler.MSBuild.pas',
  DPM.Core.Configuration.Classes in 'Core\Configuration\DPM.Core.Configuration.Classes.pas',
  DPM.Core.Configuration.Interfaces in 'Core\Configuration\DPM.Core.Configuration.Interfaces.pas',
  DPM.Core.Configuration.Manager in 'Core\Configuration\DPM.Core.Configuration.Manager.pas',
  DPM.Core.Dependency.Context in 'Core\Dependency\DPM.Core.Dependency.Context.pas',
  DPM.Core.Dependency.Reference in 'Core\Dependency\DPM.Core.Dependency.Reference.pas',
  DPM.Core.Dependency.Interfaces in 'Core\Dependency\DPM.Core.Dependency.Interfaces.pas',
  DPM.Core.Dependency.Resolution in 'Core\Dependency\DPM.Core.Dependency.Resolution.pas',
  DPM.Core.Dependency.Resolver in 'Core\Dependency\DPM.Core.Dependency.Resolver.pas',
  DPM.Core.Dependency.Version in 'Core\Dependency\DPM.Core.Dependency.Version.pas',
  DPM.Core.Logging in 'Core\Logging\DPM.Core.Logging.pas',
  DPM.Core.Options.Base in 'Core\Options\DPM.Core.Options.Base.pas',
  DPM.Core.Options.Common in 'Core\Options\DPM.Core.Options.Common.pas',
  DPM.Core.Options.Config in 'Core\Options\DPM.Core.Options.Config.pas',
  DPM.Core.Options.Install in 'Core\Options\DPM.Core.Options.Install.pas',
  DPM.Core.Options.List in 'Core\Options\DPM.Core.Options.List.pas',
  DPM.Core.Options.Pack in 'Core\Options\DPM.Core.Options.Pack.pas',
  DPM.Core.Options.Prepare in 'Core\Options\DPM.Core.Options.Prepare.pas',
  DPM.Core.Options.Push in 'Core\Options\DPM.Core.Options.Push.pas',
  DPM.Core.Options.Restore in 'Core\Options\DPM.Core.Options.Restore.pas',
  DPM.Core.Options.Search in 'Core\Options\DPM.Core.Options.Search.pas',
  DPM.Core.Options.Sources in 'Core\Options\DPM.Core.Options.Sources.pas',
  DPM.Core.Options.Spec in 'Core\Options\DPM.Core.Options.Spec.pas',
  DPM.Core.Options.UnInstall in 'Core\Options\DPM.Core.Options.UnInstall.pas',
  DPM.Core.Options.Why in 'Core\Options\DPM.Core.Options.Why.pas',
  DPM.Core.Package.Dependency in 'Core\Package\DPM.Core.Package.Dependency.pas',
  DPM.Core.Package.Installer in 'Core\Package\DPM.Core.Package.Installer.pas',
  DPM.Core.Package.InstallerContext in 'Core\Package\DPM.Core.Package.InstallerContext.pas',
  DPM.Core.Package.Interfaces in 'Core\Package\DPM.Core.Package.Interfaces.pas',
  DPM.Core.Package.Classes in 'Core\Package\DPM.Core.Package.Classes.pas',
  DPM.Core.Package.SearchResults in 'Core\Package\DPM.Core.Package.SearchResults.pas',
  DPM.Core.Packaging.Archive in 'Core\Packaging\DPM.Core.Packaging.Archive.pas',
  DPM.Core.Packaging.Archive.Reader in 'Core\Packaging\DPM.Core.Packaging.Archive.Reader.pas',
  DPM.Core.Packaging.Archive.Writer in 'Core\Packaging\DPM.Core.Packaging.Archive.Writer.pas',
  DPM.Core.Packaging.IdValidator in 'Core\Packaging\DPM.Core.Packaging.IdValidator.pas',
  DPM.Core.Packaging in 'Core\Packaging\DPM.Core.Packaging.pas',
  DPM.Core.Packaging.Writer in 'Core\Packaging\DPM.Core.Packaging.Writer.pas',
  DPM.Core.Project.Configuration in 'Core\Project\DPM.Core.Project.Configuration.pas',
  DPM.Core.Project.Editor in 'Core\Project\DPM.Core.Project.Editor.pas',
  DPM.Core.Project.GroupProjReader in 'Core\Project\DPM.Core.Project.GroupProjReader.pas',
  DPM.Core.Project.Interfaces in 'Core\Project\DPM.Core.Project.Interfaces.pas',
  DPM.Core.Project.Prepare in 'Core\Project\DPM.Core.Project.Prepare.pas',
  DPM.Core.Project.Prepare.Templates in 'Core\Project\DPM.Core.Project.Prepare.Templates.pas',
  DPM.Core.Project.Transformer in 'Core\Project\DPM.Core.Project.Transformer.pas',
  DPM.Core.Repository.Base in 'Core\Repository\DPM.Core.Repository.Base.pas',
  DPM.Core.Repository.Directory in 'Core\Repository\DPM.Core.Repository.Directory.pas',
  DPM.Core.Repository.Factory in 'Core\Repository\DPM.Core.Repository.Factory.pas',
  DPM.Core.Repository.Http in 'Core\Repository\DPM.Core.Repository.Http.pas',
  DPM.Core.Repository.Interfaces in 'Core\Repository\DPM.Core.Repository.Interfaces.pas',
  DPM.Core.Repository.Manager in 'Core\Repository\DPM.Core.Repository.Manager.pas',
  DPM.Core.Git.Interfaces in 'Core\Git\DPM.Core.Git.Interfaces.pas',
  DPM.Core.Git.Client in 'Core\Git\DPM.Core.Git.Client.pas',
  DPM.Core.Registry.Interfaces in 'Core\Registry\DPM.Core.Registry.Interfaces.pas',
  DPM.Core.Registry.Catalog in 'Core\Registry\DPM.Core.Registry.Catalog.pas',
  DPM.Core.Repository.GitRegistry in 'Core\Repository\DPM.Core.Repository.GitRegistry.pas',
  DPM.Core.Sources.Interfaces in 'Core\Sources\DPM.Core.Sources.Interfaces.pas',
  DPM.Core.Sources.Manager in 'Core\Sources\DPM.Core.Sources.Manager.pas',
  DPM.Core.Sources.Types in 'Core\Sources\DPM.Core.Sources.Types.pas',
  DPM.Core.Spec.DesignEntry in 'Core\Spec\DPM.Core.Spec.DesignEntry.pas',
  DPM.Core.Spec.BuildEntry in 'Core\Spec\DPM.Core.Spec.BuildEntry.pas',
  DPM.Core.Spec.Dependency in 'Core\Spec\DPM.Core.Spec.Dependency.pas',
  DPM.Core.Spec.SourceEntry in 'Core\Spec\DPM.Core.Spec.SourceEntry.pas',
  DPM.Core.Spec.Interfaces in 'Core\Spec\DPM.Core.Spec.Interfaces.pas',
  DPM.Core.Spec.MetaData in 'Core\Spec\DPM.Core.Spec.MetaData.pas',
  DPM.Core.Spec.Node in 'Core\Spec\DPM.Core.Spec.Node.pas',
  DPM.Core.Spec in 'Core\Spec\DPM.Core.Spec.pas',
  DPM.Core.Spec.Reader in 'Core\Spec\DPM.Core.Spec.Reader.pas',
  DPM.Core.Spec.TargetPlatform in 'Core\Spec\DPM.Core.Spec.TargetPlatform.pas',
  DPM.Core.Utils.Config in 'Core\Utils\DPM.Core.Utils.Config.pas',
  DPM.Core.Utils.Directory in 'Core\Utils\DPM.Core.Utils.Directory.pas',
  DPM.Core.Utils.Enum in 'Core\Utils\DPM.Core.Utils.Enum.pas',
  DPM.Core.Utils.Numbers in 'Core\Utils\DPM.Core.Utils.Numbers.pas',
  DPM.Core.Utils.Path in 'Core\Utils\DPM.Core.Utils.Path.pas',
  DPM.Core.Utils.Process in 'Core\Utils\DPM.Core.Utils.Process.pas',
  DPM.Core.Utils.Strings in 'Core\Utils\DPM.Core.Utils.Strings.pas',
  DPM.Core.Utils.System in 'Core\Utils\DPM.Core.Utils.System.pas',
  DPM.Core.Utils.XML in 'Core\Utils\DPM.Core.Utils.XML.pas',
  DPM.Core.MSXML in 'Core\Xml\DPM.Core.MSXML.pas',
  DPM.Core.XML.NodeBase in 'Core\Xml\DPM.Core.XML.NodeBase.pas',
  DPM.Core.Constants in 'Core\DPM.Core.Constants.pas',
  DPM.Core.Init in 'Core\DPM.Core.Init.pas',
  DPM.Core.TargetPlatform in 'Core\DPM.Core.TargetPlatform.pas',
  DPM.Core.Types in 'Core\DPM.Core.Types.pas',
  DPM.Core.Package.Icon in 'Core\Package\DPM.Core.Package.Icon.pas',
  DPM.Core.Compiler.BOM in 'Core\Compiler\DPM.Core.Compiler.BOM.pas',
  DPM.Core.Compiler.ProjectSettings in 'Core\Compiler\DPM.Core.Compiler.ProjectSettings.pas',
  DPM.Core.Utils.Files in 'Core\Utils\DPM.Core.Utils.Files.pas',
  DPM.Core.Package.Installer.Interfaces in 'Core\Package\DPM.Core.Package.Installer.Interfaces.pas',
  DPM.Core.Options.Info in 'Core\Options\DPM.Core.Options.Info.pas',
  DPM.Core.Sources.ServiceIndex in 'Core\Sources\DPM.Core.Sources.ServiceIndex.pas',
  DPM.Core.Package.ListItem in 'Core\Package\DPM.Core.Package.ListItem.pas',
  DPM.Core.Package.PackageLatestVersionInfo in 'Core\Package\DPM.Core.Package.PackageLatestVersionInfo.pas',
  DPM.Core.Utils.Hash in 'Core\Utils\DPM.Core.Utils.Hash.pas',
  DPM.Core.Utils.DateTime in 'Core\Utils\DPM.Core.Utils.DateTime.pas',
  DPM.Core.Utils.Base64Url in 'Core\Utils\DPM.Core.Utils.Base64Url.pas',
  DPM.Core.Spec.Template in 'Core\Spec\DPM.Core.Spec.Template.pas',
  DPM.Core.Utils.Masks in 'Core\Utils\DPM.Core.Utils.Masks.pas',
  DPM.Console.Prompts in 'Cmdline\DPM.Console.Prompts.pas',
  DPM.Core.SBOM.Generator in 'Core\Sbom\DPM.Core.SBOM.Generator.pas',
  DPM.Core.SBOM.Interfaces in 'Core\Sbom\DPM.Core.SBOM.Interfaces.pas',
  DPM.Core.SBOM.Types in 'Core\Sbom\DPM.Core.SBOM.Types.pas',
  DPM.Core.SBOM.Writers in 'Core\Sbom\DPM.Core.SBOM.Writers.pas',
  DPM.Core.Project.MapFile in 'Core\Project\DPM.Core.Project.MapFile.pas',
  DPM.Core.Options.Sbom in 'Core\Options\DPM.Core.Options.Sbom.pas',
  DPM.Core.Project.SearchPaths in 'Core\Project\DPM.Core.Project.SearchPaths.pas',
  DPM.Core.SBOM.Writers.Reports in 'Core\Sbom\DPM.Core.SBOM.Writers.Reports.pas',
  DPM.Core.SBOM.Reader in 'Core\Sbom\DPM.Core.SBOM.Reader.pas',
  DPM.Core.Vuln.Types in 'Core\Vuln\DPM.Core.Vuln.Types.pas',
  DPM.Core.Vuln.Interfaces in 'Core\Vuln\DPM.Core.Vuln.Interfaces.pas',
  DPM.Core.Vuln.Cache in 'Core\Vuln\DPM.Core.Vuln.Cache.pas',
  DPM.Core.Vuln.OSV in 'Core\Vuln\DPM.Core.Vuln.OSV.pas',
  DPM.Core.Vuln.Scanner in 'Core\Vuln\DPM.Core.Vuln.Scanner.pas',
  DPM.Core.Vuln.Writer.CycloneDX in 'Core\Vuln\DPM.Core.Vuln.Writer.CycloneDX.pas',
  DPM.Core.Options.Scan in 'Core\Options\DPM.Core.Options.Scan.pas',
  DPM.Core.Crypto.Algorithms in 'Core\Crypto\DPM.Core.Crypto.Algorithms.pas',
  DPM.Core.Crypto.Cms.Interfaces in 'Core\Crypto\DPM.Core.Crypto.Cms.Interfaces.pas',
  DPM.Core.Crypto.Cms in 'Core\Crypto\DPM.Core.Crypto.Cms.pas',
  DPM.Core.Crypto.Hashing.Interfaces in 'Core\Crypto\DPM.Core.Crypto.Hashing.Interfaces.pas',
  DPM.Core.Crypto.Hashing in 'Core\Crypto\DPM.Core.Crypto.Hashing.pas',
  DPM.Core.Crypto.Provider.Interfaces in 'Core\Crypto\DPM.Core.Crypto.Provider.Interfaces.pas',
  DPM.Core.Crypto.Provider in 'Core\Crypto\DPM.Core.Crypto.Provider.pas',
  DPM.Core.Crypto.Provider.Azure in 'Core\Crypto\DPM.Core.Crypto.Provider.Azure.pas',
  DPM.Core.Crypto.Provider.Signotaur in 'Core\Crypto\DPM.Core.Crypto.Provider.Signotaur.pas',
  DPM.Core.Crypto.Provider.Factory in 'Core\Crypto\DPM.Core.Crypto.Provider.Factory.pas',
  DPM.Core.Crypto.Timestamping in 'Core\Crypto\DPM.Core.Crypto.Timestamping.pas',
  DPM.Core.Crypto.Win32 in 'Core\Crypto\DPM.Core.Crypto.Win32.pas',
  DPM.Core.Crypto.X509.Interfaces in 'Core\Crypto\DPM.Core.Crypto.X509.Interfaces.pas',
  DPM.Core.Crypto.X509 in 'Core\Crypto\DPM.Core.Crypto.X509.pas',
  DPM.Core.Trust.Interfaces in 'Core\Trust\DPM.Core.Trust.Interfaces.pas',
  DPM.Core.Trust.Policy in 'Core\Trust\DPM.Core.Trust.Policy.pas',
  DPM.Core.Trust.State in 'Core\Trust\DPM.Core.Trust.State.pas',
  DPM.Core.Trust.TrustSet in 'Core\Trust\DPM.Core.Trust.TrustSet.pas',
  DPM.Core.Trust.Prompt in 'Core\Trust\DPM.Core.Trust.Prompt.pas',
  DPM.Console.Trust.Prompt in 'Cmdline\DPM.Console.Trust.Prompt.pas',
  DPM.Core.Package.Archive in 'Core\Package\Signing\DPM.Core.Package.Archive.pas',
  DPM.Core.Package.Cache.Receipt in 'Core\Package\Signing\DPM.Core.Package.Cache.Receipt.pas',
  DPM.Core.Package.Manifest.Interfaces in 'Core\Package\Signing\DPM.Core.Package.Manifest.Interfaces.pas',
  DPM.Core.Package.Manifest in 'Core\Package\Signing\DPM.Core.Package.Manifest.pas',
  DPM.Core.Package.Signing.Interfaces in 'Core\Package\Signing\DPM.Core.Package.Signing.Interfaces.pas',
  DPM.Core.Package.Signing in 'Core\Package\Signing\DPM.Core.Package.Signing.pas',
  DPM.Console.Command.Add in 'Cmdline\Commands\DPM.Console.Command.Add.pas',
  DPM.Console.Command.Base in 'Cmdline\Commands\DPM.Console.Command.Base.pas',
  DPM.Console.Command.Delete in 'Cmdline\Commands\DPM.Console.Command.Delete.pas',
  DPM.Console.Command.Install in 'Cmdline\Commands\DPM.Console.Command.Install.pas',
  DPM.Console.Command in 'Cmdline\Commands\DPM.Console.Command.pas',
  DPM.Console.Command.Push in 'Cmdline\Commands\DPM.Console.Command.Push.pas',
  DPM.Console.Command.Restore in 'Cmdline\Commands\DPM.Console.Command.Restore.pas',
  DPM.Console.Command.Uninstall in 'Cmdline\Commands\DPM.Console.Command.Uninstall.pas',
  DPM.Console.Command.Pack in 'Cmdline\Commands\DPM.Console.Command.Pack.pas',
  DPM.Console.Command.Prepare in 'Cmdline\Commands\DPM.Console.Command.Prepare.pas',
  DPM.Console.Command.Update in 'Cmdline\Commands\DPM.Console.Command.Update.pas',
  DPM.Console.Command.Config in 'Cmdline\Commands\DPM.Console.Command.Config.pas',
  DPM.Console.Command.SetApiKey in 'Cmdline\Commands\DPM.Console.Command.SetApiKey.pas',
  DPM.Console.Command.Factory in 'Cmdline\Commands\DPM.Console.Command.Factory.pas',
  DPM.Console.Command.Sources in 'Cmdline\Commands\DPM.Console.Command.Sources.pas',
  DPM.Console.Command.List in 'Cmdline\Commands\DPM.Console.Command.List.pas',
  DPM.Console.Command.Spec.Discovery in 'Cmdline\Commands\DPM.Console.Command.Spec.Discovery.pas',
  DPM.Console.Command.ExitCodes in 'Cmdline\Commands\DPM.Console.Command.ExitCodes.pas',
  DPM.Console.Command.Help in 'Cmdline\Commands\DPM.Console.Command.Help.pas',
  DPM.Console.Command.Info in 'Cmdline\Commands\DPM.Console.Command.Info.pas',
  DPM.Console.Command.Spec in 'Cmdline\Commands\DPM.Console.Command.Spec.pas',
  DPM.Console.Command.Spec.Writer in 'Cmdline\Commands\DPM.Console.Command.Spec.Writer.pas',
  DPM.Console.Command.Spec.Scaffold in 'Cmdline\Commands\DPM.Console.Command.Spec.Scaffold.pas',
  DPM.Console.Command.Why in 'Cmdline\Commands\DPM.Console.Command.Why.pas',
  DPM.Console.Command.Sbom in 'Cmdline\Commands\DPM.Console.Command.Sbom.pas',
  DPM.Console.Command.Scan in 'Cmdline\Commands\DPM.Console.Command.Scan.pas',
  DPM.Core.Options.Cache in 'Core\Options\DPM.Core.Options.Cache.pas',
  DPM.Core.Options.Trust in 'Core\Options\DPM.Core.Options.Trust.pas',
  DPM.Core.Options.Verify in 'Core\Options\DPM.Core.Options.Verify.pas',
  DPM.Core.Options.Sign in 'Core\Options\DPM.Core.Options.Sign.pas',
  DPM.Console.Command.Cache in 'Cmdline\Commands\DPM.Console.Command.Cache.pas',
  DPM.Console.Command.Trust in 'Cmdline\Commands\DPM.Console.Command.Trust.pas',
  DPM.Console.Command.Verify in 'Cmdline\Commands\DPM.Console.Command.Verify.pas',
  DPM.Console.Command.Sign in 'Cmdline\Commands\DPM.Console.Command.Sign.pas';

procedure LogException(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
var
  s : string;
begin
  Writeln(ExceptObj.ClassName, ': ', Exception(ExceptObj).Message);
  s := TSystemUtils.GetStackTrace(Exception(ExceptObj), ExceptAddr);
  Write(s);
end;


begin
  CoInitializeEx(nil, COINIT_MULTITHREADED); //needed for msxml
  try
    try
{$IFDEF JCLDEBUG}
      JCLdebug.JclStackTrackingOptions:=[stStack, stRawMode];
      JclHookExcept.JclAddExceptNotifier(LogException);
      JclDebug.JclStartExceptionTracking;
{$ENDIF}
      System.ExitCode := Ord(TDPMConsoleApplication.Run);
      {$IFDEF DEBUG}
        //if running under the debugger, pause so we can see the output!
        {$WARN SYMBOL_PLATFORM OFF}
        if DebugHook <> 0 then
          ReadLn;
      {$ENDIF};
    except
      on E: Exception do
      begin
        System.ExitCode := Ord(TExitCode.UnhandledException);
        // Swallow any I/O failure from the WriteLn itself — when launched
        // without a usable stdout (e.g. from FinalBuilder's Execute Program)
        // the Pascal RTL would otherwise raise EInOutError(6) here and that
        // becomes the only thing the parent process sees.
        try
          WriteLn(e.Message);
        except
          // nothing we can do
        end;
      {$IFDEF DEBUG}
        if DebugHook <> 0 then
          ReadLn;
      {$ENDIF};
      end;
    end;
  finally
    CoUninitialize;
  end;
end.
