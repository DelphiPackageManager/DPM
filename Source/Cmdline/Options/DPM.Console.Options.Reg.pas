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

unit DPM.Console.Options.Reg;

interface

implementation

uses
  System.SysUtils,
  WinApi.Windows,
  DPM.Core.Types,
  DPM.Console.Types,
  DPM.Console.Options,
  DPM.Core.Sources.Types,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Common,
  DPM.Core.Options.Config,
  DPM.Core.Options.Install,
  DPM.Core.Options.List,
  DPM.Core.Options.Pack,
  DPM.Core.Options.Prepare,
  DPM.Core.Options.Push,
  DPM.Core.Options.Sources,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore,
  DPM.Core.Options.Sbom,
  DPM.Core.Options.CopyLocal,
  DPM.Core.Options.Scan,
  DPM.Core.Options.Sign,
  DPM.Core.Options.Spec,
  DPM.Core.Options.Trust,
  DPM.Core.Options.Verify,
  DPM.Core.Options.Why,
  DPM.Core.Vuln.Types,
  DPM.Core.Utils.Strings,
  VSoft.CommandLine.Options;

procedure RegisterConfigCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('config', '', 'Gets or sets DPM config values.','', 'config <-Set name=value | name>');

  option := cmd.RegisterOption<string>('Set','s','One on more key-value pairs to be set in config.',
   procedure(const value : string)
    begin
      TConfigOptions.Default.SetValues := value;
    end);

end;

procedure RegisterCacheCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('cache', '',
    'Manages the local package cache. `install` downloads a package and its dependencies into ' +
    'the cache and compiles them for the package''s supported platforms (the package can be given ' +
    'either as a package id resolved from the configured sources, or as the path to a local .dpkg ' +
    'file), ' +
    '`remove` evicts cached package versions (so the next install re-downloads), ' +
    'and `verify` re-checks every package in the cache.',
    '', 'cache <install|remove|verify> [packageId|packageFile] [options]');

  // Positional sub-command. HasValue := false so the enum name is the token.
  option := cmd.RegisterUnNamedOption<TCacheSubCommand>('Sub-command: install, remove or verify','command',
   procedure(const value : TCacheSubCommand)
    begin
      TCacheOptions.Default.Command := value;
      // Keep VerifyAll in sync for any other reader of the option.
      TCacheOptions.Default.VerifyAll := value = TCacheSubCommand.Verify;
    end);
  option.Required := true;
  option.HasValue := false;

  // Positional package id - required by install/remove, ignored by verify
  // (enforced in TCacheOptions.Validate). For install this may also be the path
  // to a local .dpkg file; Validate routes it to PackageFile in that case.
  option := cmd.RegisterUnNamedOption<string>('A valid package Id, or (for install) the path to a .dpkg file','packageId',
   procedure(const value : string)
    begin
      TCacheOptions.Default.PackageId := value;
    end);

  option := cmd.RegisterOption<string>('Version','', 'The package version. For install, the version to download (latest if omitted). For remove, the version to evict (all cached versions if omitted).',
   procedure(const value : string)
    begin
      TCacheOptions.Default.VersionString := value;
    end);

  // install requires --compiler; remove treats it as an optional filter (all
  // compilers when omitted); verify ignores it. Validate enforces per command.
  option := cmd.RegisterOption<string>('compiler','c', 'The compiler version. Required by `cache install`; an optional filter for `cache remove`; ignored by `cache verify`.',
   procedure(const value : string)
    begin
      TCacheOptions.Default.CompilerVersion := StringToCompilerVersion(value);
      if TCacheOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
        raise EArgumentException.Create('Invalid compiler version : ' + value);
    end);

  option := cmd.RegisterOption<boolean>('force','f', 'Skip the confirmation prompt for `cache remove`.',
   procedure(const value : boolean)
    begin
      TCacheOptions.Default.Force := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<string>('Sources','s','The sources from which to install packages',
    procedure(const value : string)
    begin
      if TCacheOptions.Default.Sources = '' then
        TCacheOptions.Default.Sources := value
      else
        TCacheOptions.Default.Sources := TCacheOptions.Default.Sources +',' + value;
    end);
  option.AllowMultiple := true;
end;


procedure RegisterDeleteCommand;
var
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('delete', '', 'Deletes a package from the server.','Specify the Id and version of the package to delete from the server.',
                                          'delete <package Id> <package version> [API Key] [options]');
end;


procedure RegisterHelpCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('help', '?', 'Get help','Pass a command name to display help information for that command.', 'help [command]');

  option := cmd.RegisterUnNamedOption<TDPMCommand>('The command to provide help on',
    procedure(const value : TDPMCommand)
    begin
        THelpOptions.HelpCommand := value;
    end);
  option.HasValue := false;

end;

procedure RegisterInstallCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin

  cmd := TOptionsRegistry.RegisterCommand('install', '', 'Installs a package using the specified sources. If no sources are specified, all ' +
                                                      'sources defined in the DPM configuration file are used. If the configuration ' +
                                                      'file specifies no sources, uses the default DPM feed.',
                                                       'Specify the id and optionally the version of the package to install, or the path to a ' +
                                                       'package file (.dpkg).',
                                                      'install <packageId> [options]');

  option := cmd.RegisterUnNamedOption<string>('The package to install','packageId|packagePath',
    procedure(const value : string)
    begin
      TInstallOptions.Default.PackageId := value;
    end);
  option.Required := true;




  option := cmd.RegisterUnNamedOption<string>('The project to install into','projectPath',
    procedure(const value : string)
    begin
      TInstallOptions.Default.ProjectPath := value;
    end);

  option := cmd.RegisterOption<string>('version','', 'The package version to install, if not specified the latest will be downloaded',
   procedure(const value : string)
    begin
      TInstallOptions.Default.VersionString := value;
    end);

  option := cmd.RegisterOption<string>('group','g', 'The project group the projects belong to. Used to ensure package compatibility.',
   procedure(const value : string)
    begin
      TInstallOptions.Default.ProjectGroup := value;
    end);



  option := cmd.RegisterOption<string>('platforms','p', 'The platforms to install for (comma separated). Default is to install for all platforms the project targets.',
   procedure(const value : string)
   var
     platformStrings : TArray<string>;
     platformString : string;
     platform : TDPMPlatform;
   begin
      platformStrings := TStringUtils.SplitStr(value, ',',TSplitStringOptions.ExcludeEmpty);
      for platformString in platformStrings do
      begin
        platform := StringToDPMPlatform(Trim(platformString));
        if platform <> TDPMPlatform.UnknownPlatform then
          TInstallOptions.Default.Platforms := TInstallOptions.Default.Platforms + [platform]
        else
          raise Exception.Create('Invalid platform [' + platformString + ']');
      end;
    end);


  option := cmd.RegisterOption<string>('compiler','c', 'The delphi compiler version to target. ',
   procedure(const value : string)
    begin
      TInstallOptions.Default.CompilerVersion := StringToCompilerVersion(value);
      if TInstallOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
        raise EArgumentException.Create('Invalid compiler version : ' + value);
    end);

  option := cmd.RegisterOption<boolean>('preRelease','pr', 'Allow installation of pre-release packages.',
   procedure(const value : boolean)
    begin
      TInstallOptions.Default.PreRelease := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<string>('Sources','s','The sources from which to install packages',
    procedure(const value : string)
    begin
      if TInstallOptions.Default.Sources = '' then
        TInstallOptions.Default.Sources := value
      else
        TInstallOptions.Default.Sources := TInstallOptions.Default.Sources +',' + value;
    end);
  option.AllowMultiple := true;


  option := cmd.RegisterOption<boolean>('force','', 'Force install of package.',
   procedure(const value : boolean)
    begin
      TInstallOptions.Default.Force := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('upgrade','', 'Set when installing a different version',
   procedure(const value : boolean)
    begin
      TInstallOptions.Default.IsUpgrade := value;
    end);
  option.HasValue := false;


  //'use source' is an IDE-session-only debugging feature (held in the installer context and never
  //persisted) - it is intentionally not exposed as a command line option, so CLI installs always
  //reference the precompiled lib folder.

  option := cmd.RegisterOption<boolean>('debugMode','dm', 'Compile Debug configuration.',
  procedure(const value : boolean)
    begin
      TInstallOptions.Default.DebugMode := value;
    end);
  option.HasValue := false;

  cmd.Examples.Add('install VSoft.CommandLine');
  cmd.Examples.Add('install VSoft.CommandLine -version=1.0.1 c:\myprojects\project1.dproj');
  cmd.Examples.Add('install Spring.Base c:\myprojects -compiler=10.3');
  cmd.Examples.Add('install DUnitX.Framework c:\myproject\tests\mytest.dproj -compiler=10.3 -platforms=Win32,Win64,MacOS32');
end;

procedure RegisterListCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('list', 'l', 'Displays a list of packages from a given source. If no sources are specified, all sources ' +
                                                        'defined in %AppData%\DPM\DPM.config are used. If DPM.config specifies no sources, ' +
                                                        'uses the default DPM feed.',
                                                        'Specify optional search term.',
                                                        'list [search term] [options]');

  option := cmd.RegisterUnNamedOption<string>('Specify optional search terms','searchTerms',
    procedure(const value : string)
    begin
      TListOptions.Default.SearchTerms := value;
    end);


  option := cmd.RegisterOption<string>('source','s','The source from which to list packages',
    procedure(const value : string)
    begin
      if TListOptions.Default.Sources = '' then
        TListOptions.Default.Sources := value
      else
        TListOptions.Default.Sources := TListOptions.Default.Sources +',' + value;
    end);
  option.AllowMultiple := true;

  option := cmd.RegisterOption<boolean>('exact','e','Search for exact package id match.',
    procedure(const value : boolean)
    begin
      TListOptions.Default.Exact := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('prerelease','pr','Allow prerelease packages to be shown.',
    procedure(const value : boolean)
    begin
      TListOptions.Default.Prerelease := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('includeDelisted','d','Allow delisted packages to be shown.',
    procedure(const value : boolean)
    begin
      TListOptions.Default.IncludeDelisted := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<integer>('skip','','Skip over x results before listing.',
    procedure(const value : integer)
    begin
      TListOptions.Default.Skip := value;;
    end);

  option := cmd.RegisterOption<integer>('take','','Take max x results.',
    procedure(const value : integer)
    begin
      TListOptions.Default.Take := value;;
    end);

  option := cmd.RegisterOption<string>('compiler','c','Compiler version. When not specified, all compiler versions found are listed.',
    procedure(const value : string)
    begin
      TListOptions.Default.CompilerVersion := StringToCompilerVersion(value);
      if TListOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
        raise EArgumentException.Create('Invalid compiler version : ' + value);


    end);

  option := cmd.RegisterOption<TDPMPlatforms>('platforms','p','Target platforms, comma separated. When not specified, all platforms found are listed.',
    procedure(const value : TDPMPlatforms)
    begin
      TListOptions.Default.Platforms := value;
    end);


  cmd.Examples.Add('list "commandline"');

  cmd.Examples.Add('list "semantic" -prerelease -skip=10 -take=10');

  cmd.Examples.Add('list "commandline" -compiler=10.2 -platforms=Win32,Win63,MacOS32 -source=VSoftInternal -prerelease');


end;

//procedure RegisterFeedCommand;
//var
//  option : IOptionDefinition;
//  cmd : TCommandDefinition;
//begin
//  cmd := TOptionsRegistry.RegisterCommand('feed', 'f', 'Displays a list of packages from a given source. If no sources are specified, all sources ' +
//                                                        'defined in %AppData%\DPM\DPM.config are used. If DPM.config specifies no sources, ' +
//                                                        'uses the default DPM feed.',
//                                                        'Specify optional search term.',
//                                                        'list [search term] [options]');
//
//
//  option := cmd.RegisterUnNamedOption<string>('Specify optional search terms','searchTerms',
//    procedure(const value : string)
//    begin
//      TFeedOptions.Default.SearchTerms := value;
//    end);
//
//
//  option := cmd.RegisterOption<string>('source','s','The source from which to list packages',
//    procedure(const value : string)
//    begin
//      if TFeedOptions.Default.Sources = '' then
//        TFeedOptions.Default.Sources := value
//      else
//        TFeedOptions.Default.Sources := TFeedOptions.Default.Sources +',' + value;
//    end);
//  option.AllowMultiple := true;
//
//  option := cmd.RegisterOption<boolean>('allVersions','a','List all versions of a package. By default, only the latest package version is displayed.',
//    procedure(const value : boolean)
//    begin
//      TFeedOptions.Default.AllVersions := value;
//    end);
//  option.HasValue := false;
//
//  option := cmd.RegisterOption<boolean>('exact','e','Search for exact package id match.',
//    procedure(const value : boolean)
//    begin
//      TFeedOptions.Default.Exact := value;
//    end);
//  option.HasValue := false;
//
//  option := cmd.RegisterOption<boolean>('prerelease','pr','Allow prerelease packages to be shown.',
//    procedure(const value : boolean)
//    begin
//      TFeedOptions.Default.Prerelease := value;
//    end);
//  option.HasValue := false;
//
//  option := cmd.RegisterOption<boolean>('includeDelisted','d','Allow delisted packages to be shown.',
//    procedure(const value : boolean)
//    begin
//      TFeedOptions.Default.IncludeDelisted := value;
//    end);
//  option.HasValue := false;
//
//  option := cmd.RegisterOption<integer>('skip','','Skip over x results before listing.',
//    procedure(const value : integer)
//    begin
//      TFeedOptions.Default.Skip := value;;
//    end);
//
//  option := cmd.RegisterOption<integer>('take','','Take max x results.',
//    procedure(const value : integer)
//    begin
//      TFeedOptions.Default.Take := value;;
//    end);
//
//  option := cmd.RegisterOption<string>('compiler','c','Compiler version. When not specified, all compiler versions found are listed.',
//    procedure(const value : string)
//    begin
//      TFeedOptions.Default.CompilerVersion := StringToCompilerVersion(value);
//      if TFeedOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
//        raise EArgumentException.Create('Invalid compiler version : ' + value);
//
//
//    end);
//
//  option := cmd.RegisterOption<TDPMPlatforms>('platforms','p','Target platforms, comma separated. When not specified, all platforms found are listed.',
//    procedure(const value : TDPMPlatforms)
//    begin
//      TFeedOptions.Default.Platforms := value;
//    end);
//
//
//  cmd.Examples.Add('list "commandline"');
//
//  cmd.Examples.Add('list "semantic" -prerelease -skip=10 -take=10');
//
//  cmd.Examples.Add('list "commandline" -compiler=10.2 -platforms=Win32,Win63,MacOS32 -source=VSoftInternal -prerelease');
//
//
//end;



procedure RegisterPackCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('pack', '', 'Creates a DPM package based on the specified .dspec file (.dspec.yaml is also accepted).',
                                                      'Specify the location of the dspec file to create a package.',
                                                      'pack <.dspec file> [options]');

  option := cmd.RegisterUnNamedOption<string>('The package spec file','specfile',
    procedure(const value : string)
    begin
      TPackOptions.Default.SpecFile := value;
    end);
  option.Required := true;

  //TODO : Not implemented yet.
  option := cmd.RegisterOption<boolean>('excludeEmptyDirectories','e','Prevent inclusion of empty directories when building the package.',
    procedure(const value : boolean)
    begin
      TPackOptions.Default.Exclude := value;
    end);
  option.HasValue := false;


  cmd.RegisterOption<string>('outputfolder','o','Specifies the directory for the created DelphGet package file. If not specified, uses the current directory.',
    procedure(const value : string)
    begin
      TPackOptions.Default.OutputFolder := value;
    end);


  cmd.RegisterOption<string>('basepath','b','The base path of the files defined in the dspec file. If not specified then the location of the dspec is used.',
    procedure(const value : string)
    begin
      TPackOptions.Default.BasePath := value;
    end);

  cmd.RegisterOption<string>('minclientversion','mc','Set the minClientVersion attribute for the created package.',
    procedure(const value : string)
    begin
      TPackOptions.Default.MinClientVersion := value;
    end);

  cmd.RegisterOption<string>('variables','p','Provides the ability to specify a semicolon ";" delimited list of variables when creating a package.',
    procedure(const value : string)
    begin
      TPackOptions.Default.Variables := value;
    end);

  cmd.RegisterOption<string>('version','','Overrides the version number from the dspec file.',
    procedure(const value : string)
    begin
      TPackOptions.Default.Version := value;
    end);

  cmd.Examples.Add('pack VSoft.CommandLine.dspec -version=1.0.1 -outputFolder=.\output');

end;

procedure RegisterPrepareCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('prepare', '',
    'Prepares per-Delphi-version subfolders under /packages with version-correct dpk/dproj files.',
    'Reads a .dspec file (.dspec.yaml is also accepted) or auto-discovers one in the current directory. For each ' +
    'supported compiler the command ensures a "RAD Studio XXX" subfolder under /packages, ' +
    'finds an existing dpk/dproj pair to use as the source-of-truth, and propagates that ' +
    'pair into the other folders with per-version transforms applied. If no pair exists ' +
    'anywhere, a minimal pair is scaffolded into the lowest supported compiler''s folder ' +
    'for the user to edit and then re-run prepare.',
    'prepare [<.dspec file>] [options]');

  option := cmd.RegisterUnNamedOption<string>('Optional path to the .dspec file. If omitted, the current directory is scanned for a single .dspec (or .dspec.yaml)',
    'specfile',
    procedure(const value : string)
    begin
      TPrepareOptions.Default.SpecFile := value;
    end);
  option.Required := false;

  option := cmd.RegisterOption<boolean>('force','f','Overwrite existing dpk/dproj files in target folders. Default is to skip existing files.',
    procedure(const value : boolean)
    begin
      TPrepareOptions.Default.Force := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('dryrun','dr','Preview what the command would do without creating folders or writing files.',
    procedure(const value : boolean)
    begin
      TPrepareOptions.Default.DryRun := value;
    end);
  option.HasValue := false;

  cmd.Examples.Add('prepare');
  cmd.Examples.Add('prepare MyPackage.dspec');
  cmd.Examples.Add('prepare MyPackage.dspec -force');
  cmd.Examples.Add('prepare MyPackage.dspec -dryrun');
end;

procedure RegisterPushCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('push', '', 'Pushes a package to a package source and publishes it.',
                                                       'Specify the path to the package and your API key to push the package to the server.',
                                                       'push <package path>  [options]');

  option := cmd.RegisterUnNamedOption<string>('The path to the package file','package',
    procedure(const value : string)
    begin
      TPushOptions.Default.PackagePath := value;
    end);
  option.Required := true;

  option := cmd.RegisterOption<string>('source','s','The Source to push the package too. Required.',
   procedure(const value : string)
    begin
        TPushOptions.Default.Source := value;
    end);
  option.Required := true;

  option := cmd.RegisterOption<string>('apiKey','a','Api Key for authenticated http source. Falls back to the DPM_API_KEY environment variable if not specified.',
   procedure(const value : string)
    begin
        TPushOptions.Default.ApiKey := value;
    end);

  option := cmd.RegisterOption<integer>('timeout','t','Specifies the timeout for pushing to a server in seconds. Defaults to 300 seconds (5 minutes).',
   procedure(const value : integer)
    begin
        TPushOptions.Default.Timeout := value;
    end);

  option := cmd.RegisterOption<boolean>('skipDuplicate','','If a package and version already exists, skip it.',
   procedure(const value : boolean)
    begin
        TPushOptions.Default.SkipDuplicate := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<integer>('retries','','Number of times to retry the push when the server is rate-limiting (429) or temporarily unavailable (502/503/504). Default 3. Use 0 to disable.',
   procedure(const value : integer)
    begin
        TPushOptions.Default.MaxRetries := value;
    end);

  option := cmd.RegisterOption<integer>('retryDelay','','Base delay in seconds between retries when the server does not send a Retry-After header. Default 2. Backoff is exponential with jitter.',
   procedure(const value : integer)
    begin
        TPushOptions.Default.RetryDelay := value;
    end);

  option := cmd.RegisterOption<boolean>('unlisted','','Push the package but do not list it in package search/listing results.',
   procedure(const value : boolean)
    begin
        TPushOptions.Default.Unlisted := value;
    end);
  option.HasValue := false;


  cmd.Examples.Add('push .\VSoft.SemanticVersion.1.0.1.dpkg -source=local');
  cmd.Examples.Add('push .\VSoft.SemanticVersion.1.0.1.dpkg -source=corporate -apiKey=abcdef');
  cmd.Examples.Add('push .\VSoft.SemanticVersion.1.0.1.dpkg -source=corporate -apiKey=abcdef -unlisted');
  cmd.Examples.Add('set DPM_API_KEY=abcdef && push .\VSoft.SemanticVersion.1.0.1.dpkg -source=corporate');

end;

procedure RegisterRestoreCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('restore', '', 'Restores package(s)  using the specified sources. If no sources are specified, all ' +
                                                      'sources defined in the DPM configuration file are used. If the configuration ' +
                                                      'file specifies no sources, uses the default DPM feed.',
                                                      'If a project group is specified, this command downloads and installs the missing DPM packages that are listed in each project in the project group.' + #13#10#13#10 +
                                                      'If a folder is specified, then this command applies to all .dproj files in the folder.' ,
                                                      'restore [project | grouproj | folder] [options]');

  option := cmd.RegisterUnNamedOption<string>('The path to the dproj, or a folder containing 1 or more dproj files. Defaults to current directory.', 'projectPath',
   procedure(const value : string)
    begin
      TRestoreOptions.Default.ProjectPath := value;
    end);


  option := cmd.RegisterOption<string>('Source','s','Optional source(s), if not specified all sources will be used.',
   procedure(const value : string)
    begin
      if TRestoreOptions.Default.Sources = '' then
        TRestoreOptions.Default.Sources := value
      else
        TRestoreOptions.Default.Sources := TRestoreOptions.Default.Sources +',' + value;
    end);
   option.AllowMultiple := true;

  option := cmd.RegisterOption<string>('compiler','c', 'The compiler version to restore.',
   procedure(const value : string)
    begin
      TRestoreOptions.Default.CompilerVersion := StringToCompilerVersion(value);
      if TRestoreOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
        raise EArgumentException.Create('Invalid compiler version : ' + value);
    end);

  option := cmd.RegisterOption<boolean>('debugMode','dm', 'Compile Debug configuration.',
  procedure(const value : boolean)
    begin
      TRestoreOptions.Default.DebugMode := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('ignoreHashLocks','ihl', 'Ignore the manifest hash locks recorded in the project - refresh them from the cache instead of failing on a mismatch.',
  procedure(const value : boolean)
    begin
      TRestoreOptions.Default.IgnoreHashLocks := value;
    end);
  option.HasValue := false;



  cmd.Examples.Add('restore .\MyProject.dproj');
  cmd.Examples.Add('restore c:\Projects\MyProject.dproj -source=local');
  cmd.Examples.Add('restore c:\Projects -source=local');
  cmd.Examples.Add('restore c:\Projects\AllProjects.groupproj -source=local');

  cmd.Examples.Add('restore c:\Projects\AllProjects.groupproj -configFile=.\dev.dpm.config');

end;

procedure RegisterSetApiCommand;
var
//  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('setapikey', '', 'Saves an API key for a given server URL. When no URL is provided API key is saved for the DPM gallery.',
                                                      'Specify the API key to save and an optional URL to the server that provided the API key.',
                                                      'setApiKey <API key> [options]');

end;

procedure RegisterSignCommand;
var
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('sign', '',
    'Signs one or more .dpkg packages. Three signing providers are supported: ' +
    'the Windows certificate store (smart cards / HSMs / YubiKey via CSP/KSP), ' +
    'a PFX file, Azure Key Vault, or VSoft Signotaur.',
    'Targets: a single .dpkg, a folder (signs every matching file), or a ' +
    'wildcard pattern (e.g. C:\packages\*.dpkg). Use --recursive to descend ' +
    'and --pattern to override the default *.dpkg mask. For smart-card / HSM ' +
    'signing the private-key session is held open for the whole run, so the ' +
    'PIN is only prompted once.' + sLineBreak + sLineBreak +
    'Choose a provider with --provider:' + sLineBreak +
    '  local      (default) Windows cert store via --thumbprint, or PFX via --pfx.' + sLineBreak +
    '  keyvault   Azure Key Vault. Requires --vault-url, --cert-name, --tenant-id,' + sLineBreak +
    '             --client-id, --client-secret-env.' + sLineBreak +
    '  signotaur  VSoft Signotaur. Requires --endpoint, --api-key-env (or --api-key),' + sLineBreak +
    '             and one of --thumbprint / --subject / --label as cert selector.',
    'sign <package|folder|pattern> [--provider <local|keyvault|signotaur>] [options]');

  cmd.RegisterUnNamedOption<string>(
    'Path to the .dpkg, a folder of .dpkg files, or a wildcard pattern.',
    'packageFile',
    procedure(const value : string)
    begin
      TSignOptions.Default.PackageFile := value;
    end).Required := true;

  cmd.RegisterOption<boolean>('recursive', 'r',
    'When the target is a folder, recurse into subdirectories.',
    procedure(const value : boolean)
    begin
      TSignOptions.Default.Recursive := value;
    end).HasValue := false;

  cmd.RegisterOption<string>('pattern', '',
    'File pattern to match when the target is a folder (default *.dpkg).',
    procedure(const value : string)
    begin
      if value <> '' then
        TSignOptions.Default.FilePattern := value;
    end);

  cmd.RegisterOption<boolean>('fail-fast', '',
    'Stop on the first failure. Default is to continue and report at the end.',
    procedure(const value : boolean)
    begin
      TSignOptions.Default.FailFast := value;
    end).HasValue := false;

  // --- provider selector -----------------------------------------------
  cmd.RegisterOption<string>('provider', '',
    'Signing provider: local (default), keyvault, or signotaur.',
    procedure(const value : string)
    begin
      if SameText(value, 'keyvault') then
        TSignOptions.Default.Provider := spKeyVault
      else if SameText(value, 'signotaur') then
        TSignOptions.Default.Provider := spSignotaur
      else
        TSignOptions.Default.Provider := spLocal;
    end);

  // --- [local] cert-store / PFX ----------------------------------------
  cmd.RegisterOption<string>('thumbprint', 't',
    '[local|signotaur] SHA-1 thumbprint of the signing cert. For local it is ' +
    'the cert in the Windows store (MY).',
    procedure(const value : string)
    begin
      TSignOptions.Default.Thumbprint := value;
    end);

  cmd.RegisterOption<string>('store', '',
    '[local] Certificate-store location: CurrentUser (default) or LocalMachine.',
    procedure(const value : string)
    begin
      if SameText(value, 'LocalMachine') then
        TSignOptions.Default.StoreLocation := sslLocalMachine
      else
        TSignOptions.Default.StoreLocation := sslCurrentUser;
    end);

  cmd.RegisterOption<string>('pfx', 'p',
    '[local] Path to a PFX file containing the signing certificate.',
    procedure(const value : string)
    begin
      TSignOptions.Default.PfxFile := value;
    end);

  cmd.RegisterOption<string>('pfx-password-env', '',
    '[local] Name of an environment variable holding the PFX password.',
    procedure(const value : string)
    begin
      TSignOptions.Default.PfxPasswordEnvVar := value;
    end);

  // --- timestamping + digest (all providers) ---------------------------
  cmd.RegisterOption<string>('timestamper', '',
    'RFC3161 timestamp authority URL (default: DigiCert).',
    procedure(const value : string)
    begin
      TSignOptions.Default.TimestampUrl := value;
    end);

  cmd.RegisterOption<string>('digest', 'd',
    'CMS digest algorithm: sha256, sha384, or sha512. Default: auto-select ' +
    'from the cert key (RSA -> SHA-256; ECDSA P-256 -> SHA-256; P-384 -> ' +
    'SHA-384; P-521 -> SHA-512). Override only if your HSM / cert chain ' +
    'requires it.',
    procedure(const value : string)
    begin
      TSignOptions.Default.Digest := value;
    end);

  // --- [keyvault] Azure Key Vault --------------------------------------
  cmd.RegisterOption<string>('vault-url', '',
    '[keyvault] Azure Key Vault URL (e.g. https://my-vault.vault.azure.net).',
    procedure(const value : string)
    begin
      TSignOptions.Default.VaultUrl := value;
    end);

  cmd.RegisterOption<string>('cert-name', '',
    '[keyvault] Key Vault certificate name.',
    procedure(const value : string)
    begin
      TSignOptions.Default.CertName := value;
    end);

  cmd.RegisterOption<string>('key-version', '',
    '[keyvault] Optional Key Vault key version. Defaults to latest.',
    procedure(const value : string)
    begin
      TSignOptions.Default.KeyVersion := value;
    end);

  cmd.RegisterOption<string>('tenant-id', '',
    '[keyvault] Azure AAD tenant id (GUID or domain).',
    procedure(const value : string)
    begin
      TSignOptions.Default.TenantId := value;
    end);

  cmd.RegisterOption<string>('client-id', '',
    '[keyvault] Azure AAD application (client) id used to authenticate to Key Vault.',
    procedure(const value : string)
    begin
      TSignOptions.Default.ClientId := value;
    end);

  cmd.RegisterOption<string>('client-secret-env', '',
    '[keyvault] Name of an environment variable holding the AAD client secret.',
    procedure(const value : string)
    begin
      TSignOptions.Default.ClientSecretEnv := value;
    end);

  // --- [signotaur] VSoft Signotaur -------------------------------------
  cmd.RegisterOption<string>('endpoint', '',
    '[signotaur] Service endpoint URL (e.g. https://signotaur.example.com).',
    procedure(const value : string)
    begin
      TSignOptions.Default.SignotaurEndpoint := value;
    end);

  cmd.RegisterOption<string>('api-key-env', '',
    '[signotaur] Name of an environment variable holding the API key (preferred over --api-key).',
    procedure(const value : string)
    begin
      TSignOptions.Default.SignotaurApiKeyEnv := value;
    end);

  cmd.RegisterOption<string>('api-key', '',
    '[signotaur] API key as a literal value. Discouraged — visible in process ' +
    'listings and shell history. Use --api-key-env instead when possible.',
    procedure(const value : string)
    begin
      TSignOptions.Default.SignotaurApiKey := value;
    end);

  cmd.RegisterOption<string>('subject', '',
    '[signotaur] Cert selector by certificate Subject (alternative to --thumbprint).',
    procedure(const value : string)
    begin
      TSignOptions.Default.SignotaurSubject := value;
    end);

  cmd.RegisterOption<string>('label', '',
    '[signotaur] Cert selector by user-assigned Label (alternative to --thumbprint).',
    procedure(const value : string)
    begin
      TSignOptions.Default.SignotaurLabel := value;
    end);

  cmd.RegisterOption<boolean>('allow-untrusted', '',
    '[signotaur] Trust TLS chains that fail validation (self-signed cert, ' +
    'untrusted CA, hostname mismatch). Dev / on-prem use only',
    procedure(const value : boolean)
    begin
      TSignOptions.Default.SignotaurAllowSelfSigned := value;
    end).HasValue := false;

  // --- examples --------------------------------------------------------
  cmd.Examples.Add('sign Foo.dpkg -thumbprint=AB12CD34EF                       # local cert store');
  cmd.Examples.Add('sign Foo.dpkg -pfx=cert.pfx -pfx-password-env=PFX_PWD      # local PFX');
  cmd.Examples.Add('sign C:\packages -thumbprint=AB12CD34EF                    # all *.dpkg in folder');
  cmd.Examples.Add('sign C:\packages -r                                        # recursive');
  cmd.Examples.Add('sign "C:\out\*-Win64-*.dpkg" -thumbprint=AB12CD34EF        # wildcard');
  cmd.Examples.Add('sign Foo.dpkg -provider=keyvault -vault-url=https://my.vault.azure.net -cert-name=codesign -tenant-id=GUID -client-id=GUID -client-secret-env=AAD_SECRET');
  cmd.Examples.Add('sign Foo.dpkg -provider=signotaur -endpoint=https://signotaur.example.com -api-key-env=SIGNOTAUR_KEY -label=CodeSign');
end;



procedure RegisterSourcesCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('sources', '', 'Provides the ability to manage list of sources located in %AppData%\DPM\DPM.config',
                                                      '',
                                                      'sources <List|Add|Remove|Enable|Disable|Update|Refresh> -Name [name] -Source [source] -type <Folder|GitRegistry|DPMServer>');
  option := cmd.RegisterUnNamedOption<TSourcesSubCommand>('<List|Add|Remove|Enable|Disable|Update|Refresh>','operation',
    procedure(const value : TSourcesSubCommand)
    begin
      TSourcesOptions.Default.Command := value;
    end);
//  option.Required := true;
  option.HasValue := false;

  cmd.RegisterOption<string>('name','n','The source Name.',
    procedure(const value : string)
    begin
      TSourcesOptions.Default.Name := value;
    end);

  cmd.RegisterOption<string>('source','s','The source.',
    procedure(const value : string)
    begin
      TSourcesOptions.Default.Source := value;
    end);

  cmd.RegisterOption<TSourceType>('type','t','The source type',
    procedure(const value : TSourceType)
    begin
      TSourcesOptions.Default.SourceType := value;
    end);


  cmd.RegisterOption<TSourcesFormat>('format','f','Applies to the list action. Accepts two values: Detailed (the default) and Short.',
    procedure(const value : TSourcesFormat)
    begin
      TSourcesOptions.Default.Format := value;
    end);

  cmd.RegisterOption<string>('userName','u','Specifies the user name for authenticating with the source.',
    procedure(const value : string)
    begin
      TSourcesOptions.Default.UserName := value;
    end);

  cmd.RegisterOption<string>('password','p',' Specifies the password for authenticating with the source.',
    procedure(const value : string)
    begin
      TSourcesOptions.Default.Password := value;
    end);



end;

procedure RegisterSpecCommand;
var
  cmd : TCommandDefinition;
  option : IOptionDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('spec', '', 'Generates a .dspec for a new package. If this command is run in the same folder as a ' +
                                                      'project file (.dproj), it will create a tokenized dspec file.',
                                                      '',
                                                      'spec [package id]');

  option := cmd.RegisterUnNamedOption<string>('Package Id (e.g VSoft.LibA)','packageId',
    procedure(const value : string)
    begin
      TSpecOptions.Default.PackageId := value;
    end);
  option.Required := false;

  option := cmd.RegisterOption<boolean>('overwrite', 'o', 'Overwrite an existing .dspec in the current folder without prompting',
    procedure(const value : boolean)
    begin
      TSpecOptions.Default.Overwrite := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('noflatten', 'n', 'Do not flatten the source folder structure',
    procedure(const value : boolean)
    begin
      TSpecOptions.Default.NoFlatten := value;
    end);
  option.HasValue := false;

end;


procedure RegisterUpdateCommand;
var
//  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin

  cmd := TOptionsRegistry.RegisterCommand('update', '', 'Updates a package using the specified sources. If no sources are specified, all ' +
                                                      'sources defined in the DPM configuration file are used. If the configuration ' +
                                                      'file specifies no sources, uses the default DPM feed.',
                                                      '',
                                                      'update packageId|pathToPackagesConfig [options]');

end;

procedure RegisterVerifyCommand;
var
  cmd : TCommandDefinition;
  option : IOptionDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('verify', '',
    'Verifies a .dpkg package using the active trust policy. Runs the full ' +
    'verification workflow: archive rules, manifest integrity, file-set equality, ' +
    'CMS signature, chain build, and RFC3161 timestamp.',
    '',
    'verify <package> [options]');

  cmd.RegisterUnNamedOption<string>('Path to the .dpkg to verify', 'packageFile',
    procedure(const value : string)
    begin
      TVerifyOptions.Default.PackageFile := value;
    end).Required := true;

  option := cmd.RegisterOption<boolean>('offline', '', 'Skip network operations (revocation, timestamp authority fetch).',
    procedure(const value : boolean)
    begin
      TVerifyOptions.Default.Offline := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('json-output', '',
    'Emit verification result as a single JSON object. Suppresses banner / human output.',
    procedure(const value : boolean)
    begin
      TVerifyOptions.Default.JsonOutput := value;
    end);
  option.HasValue := false;
end;

procedure RegisterTrustCommand;
var
  cmd : TCommandDefinition;
  option : IOptionDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('trust', '',
    'Manages the local trust policy: trusted publishers, trusted repositories, and ' +
    'related signing settings. Persists changes to dpm.config.yaml.',
    '',
    'trust <list|add|remove|show> [publisher|repository] [options]');

  option := cmd.RegisterUnNamedOption<TTrustSubCommand>('Action: list/add/remove/show', 'action',
    procedure(const value : TTrustSubCommand)
    begin
      TTrustOptions.Default.Command := value;
    end);
  option.Required := false;
  option.HasValue := false;

  option := cmd.RegisterUnNamedOption<TTrustKind>('Entry kind: publisher (default) or repository', 'kind',
    procedure(const value : TTrustKind)
    begin
      TTrustOptions.Default.Kind := value;
    end);
  option.Required := false;
  option.HasValue := false;

  cmd.RegisterOption<string>('spki', '', 'SHA-256 SPKI hash, e.g. sha256:AB12CD34...',
    procedure(const value : string)
    begin
      TTrustOptions.Default.Spki := value;
    end);

  cmd.RegisterOption<string>('name', 'n', 'Display name (publisher only).',
    procedure(const value : string)
    begin
      TTrustOptions.Default.Name := value;
    end);

  cmd.RegisterOption<string>('url', 'u', 'Repository URL (repository only).',
    procedure(const value : string)
    begin
      TTrustOptions.Default.Url := value;
    end);
end;


procedure RegisterExitCodesCommand;
var
//  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin

  cmd := TOptionsRegistry.RegisterCommand('exitcodes', '', 'Lists dpm exit codes with text description ',
                                                      '',
                                                      'exitcodes',false);


end;


procedure RegisterUninstallCommand;
var
  cmd : TCommandDefinition;
  option : IOptionDefinition;
begin

  cmd := TOptionsRegistry.RegisterCommand('uninstall', '', 'Removes a package from a project',
                                                      '',
                                                      'install <packageId> [options]',true);
  option := cmd.RegisterUnNamedOption<string>('The package to remove','packageId',
    procedure(const value : string)
    begin
      TUninstallOptions.Default.PackageId := value;
    end);
  option.Required := true;

  option := cmd.RegisterUnNamedOption<string>('The path to the dproj, or a folder containing 1 or more dproj files. Defaults to current directory.','projectPath',
   procedure(const value : string)
    begin
      TUninstallOptions.Default.ProjectPath := value;
    end);

  option := cmd.RegisterOption<string>('compiler','c', 'The delphi compiler version to target. ',
   procedure(const value : string)
    begin
      TUninstallOptions.Default.CompilerVersion := StringToCompilerVersion(value);
      if TUninstallOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
        raise EArgumentException.Create('Invalid compiler version : ' + value);
    end);

  option := cmd.RegisterOption<string>('platforms','p', 'The platforms to install for (comma separated). Default is to install for all platforms the project targets.',
   procedure(const value : string)
   var
     platformStrings : TArray<string>;
     platformString : string;
     platform : TDPMPlatform;
   begin
      platformStrings := TStringUtils.SplitStr(value, ',',TSplitStringOptions.ExcludeEmpty);
      for platformString in platformStrings do
      begin
        platform := StringToDPMPlatform(Trim(platformString));
        if platform <> TDPMPlatform.UnknownPlatform then
          TInstallOptions.Default.Platforms := TInstallOptions.Default.Platforms + [platform]
        else
          raise Exception.Create('Invalid platform [' + platformString + ']');
      end;
    end);


  cmd.Examples.Add('uninstall VSoft.CommandLine');
  cmd.Examples.Add('uninstall VSoft.CommandLine project1.dproj');
  cmd.Examples.Add('uninstall VSoft.CommandLine c:\myprojects\project1.dproj');
  cmd.Examples.Add('uninstall VSoft.CommandLine c:\myprojects\project1.dproj -platforms=Win64,MacOS64');
  cmd.Examples.Add('uninstall VSoft.CommandLine c:\myprojects\project1.groupproj');
end;



procedure RegisterWhyCommand;
var
  cmd : TCommandDefinition;
  option : IOptionDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('why', '', 'Shows the dependency chain(s) that pull a package into a project.',
                                                      'Specify the package id and optionally a project (.dproj) path.',
                                                      'why <packageId> [projectPath] [options]');

  option := cmd.RegisterUnNamedOption<string>('The package id to explain', 'packageId',
    procedure(const value : string)
    begin
      TWhyOptions.Default.PackageId := value;
    end);
  option.Required := true;

  option := cmd.RegisterUnNamedOption<string>('The path to the .dproj file. Defaults to a .dproj in the current directory.', 'projectPath',
    procedure(const value : string)
    begin
      TWhyOptions.Default.ProjectPath := value;
    end);

  option := cmd.RegisterOption<string>('compiler','c','The delphi compiler version.',
    procedure(const value : string)
    begin
      TWhyOptions.Default.CompilerVersion := StringToCompilerVersion(value);
      if TWhyOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
        raise EArgumentException.Create('Invalid compiler version : ' + value);
    end);

  cmd.Examples.Add('why VSoft.HttpClient c:\myprojects\project1.dproj');
  cmd.Examples.Add('why Spring4D.Base c:\myprojects\project1.dproj -compiler=12.0');
end;

procedure RegisterInfoCommand;
var
  cmd : TCommandDefinition;
begin

  cmd := TOptionsRegistry.RegisterCommand('info', '', 'Show info about dpm config',
                                                      '',
                                                      'info',true);

end;

procedure RegisterSbomCommand;
var
  cmd : TCommandDefinition;
  option : IOptionDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('sbom', '', 'Generates a Software Bill of Materials (SBOM) for the project.',
                                                      'Emits CycloneDX 1.5 JSON and / or SPDX 2.3 JSON for each enabled platform. ' +
                                                      'For non-DPM library detection, ensure the linker option "Map file = Detailed" is enabled in the build configuration you point at.',
                                                      'sbom <project> [-outdir=<dir>] [-format=cyclonedx|spdx|both] [-platforms=<csv>] [-config=<name>] [-map=<path>] [-no-runtime] [-strict]');

  option := cmd.RegisterUnNamedOption<string>('The .dproj or .groupproj. Defaults to the current directory.', 'projectPath',
    procedure(const value : string)
    begin
      TSBOMOptions.Default.ProjectPath := value;
    end);

  cmd.RegisterOption<string>('outdir', 'o', 'Output directory for SBOM files. Defaults to the project directory.',
    procedure(const value : string)
    begin
      TSBOMOptions.Default.OutputDir := value;
    end);

  cmd.RegisterOption<string>('format', 'f',
    'Comma-separated list of output formats: cyclonedx | spdx | html | markdown. ' +
    'Aliases: ''both'' = cyclonedx,spdx; ''all'' = every format. Default: cyclonedx.',
    procedure(const value : string)
    begin
      TSBOMOptions.Default.Formats := StringToSBOMFormats(value);
    end);

  cmd.RegisterOption<string>('platforms', 'p', 'Platforms to generate for (comma separated). Default: all enabled in the project.',
    procedure(const value : string)
    var
      platformStrings : TArray<string>;
      platformString : string;
      platform : TDPMPlatform;
    begin
      platformStrings := TStringUtils.SplitStr(value, ',', TSplitStringOptions.ExcludeEmpty);
      for platformString in platformStrings do
      begin
        platform := StringToDPMPlatform(Trim(platformString));
        if platform <> TDPMPlatform.UnknownPlatform then
          TSBOMOptions.Default.Platforms := TSBOMOptions.Default.Platforms + [platform]
        else
          raise Exception.Create('Invalid platform [' + platformString + ']');
      end;
    end);

  cmd.RegisterOption<string>('config', 'c', 'Build configuration to use for locating the MAP file (default: Release, else Debug, else first available).',
    procedure(const value : string)
    begin
      TSBOMOptions.Default.Config := value;
    end);

  cmd.RegisterOption<string>('map', 'm', 'Path to a specific MAP file. Overrides auto-detection (single-platform invocations only).',
    procedure(const value : string)
    begin
      TSBOMOptions.Default.MapFile := value;
    end);

  option := cmd.RegisterOption<boolean>('no-runtime', '', 'Exclude the Delphi RTL/VCL/FMX component from the SBOM (included by default).',
    procedure(const value : boolean)
    begin
      TSBOMOptions.Default.IncludeRuntime := not value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('strict', '', 'Fail with a non-zero exit code if a MAP file is missing (default: warn and emit a partial SBOM).',
    procedure(const value : boolean)
    begin
      TSBOMOptions.Default.Strict := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('per-project', '',
    'When the input is a .groupproj, emit one SBOM per dproj per platform (legacy behaviour). ' +
    'Default: one aggregated SBOM per platform spanning the whole group.',
    procedure(const value : boolean)
    begin
      TSBOMOptions.Default.PerProject := value;
    end);
  option.HasValue := false;

  cmd.Examples.Add('sbom .\MyProject.dproj');
  cmd.Examples.Add('sbom .\MyProject.dproj -outdir=c:\temp -format=cyclonedx');
  cmd.Examples.Add('sbom .\MyProject.dproj -format=html,markdown');
  cmd.Examples.Add('sbom .\MyProject.dproj -format=all');
  cmd.Examples.Add('sbom .\MyProject.dproj -platforms=Win32,Win64 -config=Release');
  cmd.Examples.Add('sbom .\MySolution.groupproj                    # one aggregated SBOM per platform');
  cmd.Examples.Add('sbom .\MySolution.groupproj -per-project       # one SBOM per dproj per platform');
end;


procedure RegisterCopyLocalCommand;
var
  cmd : TCommandDefinition;
  option : IOptionDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('copylocal', '', 'Copies package binaries (dlls/bpls) into the project build output folder.',
                                                      'Invoked by DPM.CopyLocal.targets from an AfterBuild MSBuild target. The output dir is only ' +
                                                      'reliably known at build time, so this runs then rather than at restore. dcc-direct/CI builds ' +
                                                      'can call it directly with the same arguments.',
                                                      'copylocal <project> -platform=<platform> -outputDir=<dir> [-config=<name>] [-compiler=<id>] [-usePackages=<bool>] [-runtimePackages=<list>]');

  cmd.RegisterUnNamedOption<string>('The .dproj being built. Defaults to the current directory.', 'projectPath',
    procedure(const value : string)
    begin
      TCopyLocalOptions.Default.ProjectPath := value;
    end);

  //All options below are passed by MSBuild as key=value where the value is an interpolated property
  //(e.g. -usePackages=$(UsePackages)). MSBuild emits an empty value when the property is blank, so
  //HasValue is set false on each - otherwise the parser rejects '-usePackages=' (empty) as an error,
  //which aborts copylocal during every build that doesn't set that property. Empty values are still
  //captured (an empty platform/outputDir is then reported cleanly by TCopyLocalOptions.Validate).

  option := cmd.RegisterOption<string>('platform', 'p', 'The platform being built (msbuild $(Platform), e.g. Win32, Win64).',
    procedure(const value : string)
    begin
      TCopyLocalOptions.Default.Platform := ProjectPlatformToDPMPlatform(Trim(value));
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<string>('config', 'c', 'The build configuration being built (msbuild $(Config)).',
    procedure(const value : string)
    begin
      TCopyLocalOptions.Default.Config := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<string>('outputDir', 'o', 'The build output folder to copy into (msbuild $(OutputPath)).',
    procedure(const value : string)
    begin
      TCopyLocalOptions.Default.OutputDir := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<string>('compiler', '', 'The compiler version the project targets (msbuild $(DPMCompiler)).',
    procedure(const value : string)
    begin
      TCopyLocalOptions.Default.CompilerVersion := StringToCompilerVersion(Trim(value));
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<string>('usePackages', '', 'true when the build links with runtime packages (msbuild $(UsePackages)).',
    procedure(const value : string)
    begin
      TCopyLocalOptions.Default.UsePackages := SameText(Trim(value), 'true');
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<string>('runtimePackages', '', 'The build''s runtime package link set (msbuild $(DCC_UsePackage)).',
    procedure(const value : string)
    begin
      TCopyLocalOptions.Default.RuntimePackages := value;
    end);
  option.HasValue := false;

  cmd.Examples.Add('copylocal .\MyApp.dproj -platform=Win64 -config=Release -outputDir=.\Win64\Release');
end;



procedure RegisterScanCommand;
var
  cmd : TCommandDefinition;
  option : IOptionDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('scan', '', 'Scans a CycloneDX SBOM or a .dproj/.groupproj for known vulnerabilities (OSV).',
                                                      'Reads the input SBOM (or generates one from the project), queries the Open Source Vulnerabilities database ' +
                                                      '(https://api.osv.dev), and writes a CycloneDX 1.5 VEX report. Responses are cached for 24 hours under ' +
                                                      '%APPDATA%\.dpm\vuln-cache. Use -fail-on=high (or critical/medium/low) to fail the build in CI when a serious vuln is found.',
                                                      'scan <sbom-or-project> [-output=<path>] [-fail-on=none|low|medium|high|critical] [-no-cache] [-source=osv] [-platforms=<csv>] [-config=<name>]');

  option := cmd.RegisterUnNamedOption<string>('The CycloneDX SBOM .json file, or the .dproj / .groupproj to generate and scan.', 'inputPath',
    procedure(const value : string)
    begin
      TScanOptions.Default.InputPath := value;
    end);

  cmd.RegisterOption<string>('output', 'o', 'Output .vex.json path (file for SBOM input, directory for project input). ' +
                                            'Default: <input>.vex.json next to the input, or per-platform files in the project folder.',
    procedure(const value : string)
    begin
      TScanOptions.Default.OutputPath := value;
    end);

  cmd.RegisterOption<string>('source', 's', 'Vulnerability database. Only ''osv'' is supported in v1.',
    procedure(const value : string)
    begin
      TScanOptions.Default.Source := value;
    end);

  cmd.RegisterOption<string>('fail-on', '',
    'Exit code 1 if any vulnerability of this severity (or higher) is found. ' +
    'Accepted: none | low | medium | high | critical. Default: none.',
    procedure(const value : string)
    begin
      TScanOptions.Default.FailOn := StringToSeverity(value);
    end);

  option := cmd.RegisterOption<boolean>('no-cache', '', 'Bypass the 24h response cache for this run (still writes fresh responses to the cache).',
    procedure(const value : boolean)
    begin
      TScanOptions.Default.NoCache := value;
    end);
  option.HasValue := false;

  cmd.RegisterOption<string>('platforms', 'p', 'Platforms to scan (only used when input is a .dproj/.groupproj). Default: all enabled.',
    procedure(const value : string)
    var
      platformStrings : TArray<string>;
      platformString : string;
      platform : TDPMPlatform;
    begin
      platformStrings := TStringUtils.SplitStr(value, ',', TSplitStringOptions.ExcludeEmpty);
      for platformString in platformStrings do
      begin
        platform := StringToDPMPlatform(Trim(platformString));
        if platform <> TDPMPlatform.UnknownPlatform then
          TScanOptions.Default.Platforms := TScanOptions.Default.Platforms + [platform]
        else
          raise Exception.Create('Invalid platform [' + platformString + ']');
      end;
    end);

  cmd.Examples.Add('scan MyProject.cdx.json');
  cmd.Examples.Add('scan MyProject.cdx.json -fail-on=high');
  cmd.Examples.Add('scan MyProject.dproj -platforms=Win32,Win64');
  cmd.Examples.Add('scan MySolution.groupproj -output=c:\reports');
  cmd.Examples.Add('scan MyProject.cdx.json -no-cache    # force fresh OSV queries');
end;


procedure RegisterOptions;
var
  option : IOptionDefinition;
begin
  TOptionsRegistry.NameValueSeparator := '=';
  TOptionsRegistry.DescriptionTab := 50;

  //Global options
  option := TOptionsRegistry.RegisterOption<boolean>('nobanner','nb','Disable showing the banner on the console',
    procedure(const value : boolean)
    begin
      TCommonOptions.Default.NoBanner := value;
    end);
  option.HasValue := false;

  //global help command, another way of getting help for a command
  option := TOptionsRegistry.RegisterOption<boolean>('help','h','Get Help.',
    procedure(const value : boolean)
    begin
      TCommonOptions.Default.Help := value;
    end);
  option.HasValue := false;

  option := TOptionsRegistry.RegisterOption<TVerbosity>('verbosity','v','Output verbosity (Quiet|Normal|Detailed)',
    procedure(const value : TVerbosity)
    begin
      TCommonOptions.Default.Verbosity := value;
    end);

  option := TOptionsRegistry.RegisterOption<string>('configFile','','The dpm.config file to use.',
    procedure(const value : string)
    begin
      TCommonOptions.Default.ConfigFile := value;
    end);

  RegisterCacheCommand;
  RegisterConfigCommand;
  RegisterDeleteCommand;
  RegisterHelpCommand;
  RegisterInstallCommand;
  RegisterListCommand;
  RegisterPackCommand;
  RegisterPrepareCommand;
  RegisterPushCommand;
  RegisterUninstallCommand;
  RegisterRestoreCommand;
  RegisterSetApiCommand;
  RegisterSignCommand;
  RegisterSourcesCommand;
  RegisterSpecCommand;
  RegisterTrustCommand;
  RegisterUpdateCommand;
  RegisterVerifyCommand;
  RegisterWhyCommand;
  RegisterExitCodesCommand;
  RegisterInfoCommand;
  RegisterSbomCommand;
  RegisterCopyLocalCommand;
  RegisterScanCommand;

end;

initialization
  //doing this because VSoft.Command line raises exceptions for dup options
  //if we don't handle the exceptions here then we will get a runtime error 217
  try
    RegisterOptions;
  except
    on e : Exception do
    begin
      Writeln('Error registering options : ' + e.Message);
      ExitProcess(999);
    end;
  end;


end.
