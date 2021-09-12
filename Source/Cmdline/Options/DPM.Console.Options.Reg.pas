{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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
  DPM.Core.Options.Feed,
  DPM.Core.Options.Pack,
  DPM.Core.Options.Push,
  DPM.Core.Options.Sources,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore,
  DPM.Core.Options.Spec,
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
  cmd := TOptionsRegistry.RegisterCommand('cache', '', 'Downloads a package to the local package cache using the specified sources. If no sources are specified, those listed in the global configuration file, `%appdata%\DPM\dpm.Config`','', 'cache <packageId> [options]');

  option := cmd.RegisterUnNamedOption<string>('A valid package Id','packageId',
   procedure(const value : string)
    begin
      TCacheOptions.Default.PackageId := value;
    end);
  option.Required := true;

  option := cmd.RegisterOption<string>('Version','', 'The package version to cache, if not specified the latest will be downloaded',
   procedure(const value : string)
    begin
      TCacheOptions.Default.VersionString := value;
    end);

  option := cmd.RegisterOption<string>('compiler','c', 'The compiler version of the package to cache.',
   procedure(const value : string)
    begin
      TCacheOptions.Default.CompilerVersion := StringToCompilerVersion(value);
      if TCacheOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
        raise EArgumentException.Create('Invalid compiler version : ' + value);
    end);
  option.Required := true;

  option := cmd.RegisterOption<string>('platforms','p', 'The platforms to cache for (comma separated). Default is to cachge for all platforms available.',
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
          TCacheOptions.Default.Platforms := TCacheOptions.Default.Platforms + [platform]
        else
          raise Exception.Create('Invalid platform [' + platformString + ']');
      end;
    end);

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
//  option : IOptionDefinition;
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

  option := cmd.RegisterOption<boolean>('useSource','us', 'Reference package source rather than compiling it.',
   procedure(const value : boolean)
    begin
      TInstallOptions.Default.UseSource := value;
    end);
  option.HasValue := false;


  cmd.Examples.Add('install VSoft.CommandLine');
  cmd.Examples.Add('install VSoft.CommandLine -version=1.0.1 c:\myprojects\project1.dproj');
  cmd.Examples.Add('install Spring.Base c:\myprojects -compiler=10.3');
  cmd.Examples.Add('install DUnitX.Framework c:\myproject\tests\mytest.dproj -compiler=10.3 -platforms=Win32,Win64,OSX32');
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

  option := cmd.RegisterOption<boolean>('allVersions','a','List all versions of a package. By default, only the latest package version is displayed.',
    procedure(const value : boolean)
    begin
      TListOptions.Default.AllVersions := value;
    end);
  option.HasValue := false;

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

  cmd.Examples.Add('list "commandline" -compiler=10.2 -platforms=Win32,Win63,OSX32 -source=VSoftInternal -prerelease');


end;

procedure RegisterFeedCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('feed', 'f', 'Displays a list of packages from a given source. If no sources are specified, all sources ' +
                                                        'defined in %AppData%\DPM\DPM.config are used. If DPM.config specifies no sources, ' +
                                                        'uses the default DPM feed.',
                                                        'Specify optional search term.',
                                                        'list [search term] [options]');


  option := cmd.RegisterUnNamedOption<string>('Specify optional search terms','searchTerms',
    procedure(const value : string)
    begin
      TFeedOptions.Default.SearchTerms := value;
    end);


  option := cmd.RegisterOption<string>('source','s','The source from which to list packages',
    procedure(const value : string)
    begin
      if TFeedOptions.Default.Sources = '' then
        TFeedOptions.Default.Sources := value
      else
        TFeedOptions.Default.Sources := TFeedOptions.Default.Sources +',' + value;
    end);
  option.AllowMultiple := true;

  option := cmd.RegisterOption<boolean>('allVersions','a','List all versions of a package. By default, only the latest package version is displayed.',
    procedure(const value : boolean)
    begin
      TFeedOptions.Default.AllVersions := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('exact','e','Search for exact package id match.',
    procedure(const value : boolean)
    begin
      TFeedOptions.Default.Exact := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('prerelease','pr','Allow prerelease packages to be shown.',
    procedure(const value : boolean)
    begin
      TFeedOptions.Default.Prerelease := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<boolean>('includeDelisted','d','Allow delisted packages to be shown.',
    procedure(const value : boolean)
    begin
      TFeedOptions.Default.IncludeDelisted := value;
    end);
  option.HasValue := false;

  option := cmd.RegisterOption<integer>('skip','','Skip over x results before listing.',
    procedure(const value : integer)
    begin
      TFeedOptions.Default.Skip := value;;
    end);

  option := cmd.RegisterOption<integer>('take','','Take max x results.',
    procedure(const value : integer)
    begin
      TFeedOptions.Default.Take := value;;
    end);

  option := cmd.RegisterOption<string>('compiler','c','Compiler version. When not specified, all compiler versions found are listed.',
    procedure(const value : string)
    begin
      TFeedOptions.Default.CompilerVersion := StringToCompilerVersion(value);
      if TFeedOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
        raise EArgumentException.Create('Invalid compiler version : ' + value);


    end);

  option := cmd.RegisterOption<TDPMPlatforms>('platforms','p','Target platforms, comma separated. When not specified, all platforms found are listed.',
    procedure(const value : TDPMPlatforms)
    begin
      TFeedOptions.Default.Platforms := value;
    end);


  cmd.Examples.Add('list "commandline"');

  cmd.Examples.Add('list "semantic" -prerelease -skip=10 -take=10');

  cmd.Examples.Add('list "commandline" -compiler=10.2 -platforms=Win32,Win63,OSX32 -source=VSoftInternal -prerelease');


end;



procedure RegisterPackCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('pack', '', 'Creates a DPM package based on the specified .dspec file.',
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

  cmd.RegisterOption<string>('properties','p','Provides the ability to specify a semicolon ";" delimited list of properties when creating a package.',
    procedure(const value : string)
    begin
      TPackOptions.Default.Properties := value;
    end);

  cmd.RegisterOption<string>('version','','Overrides the version number from the dspec file.',
    procedure(const value : string)
    begin
      TPackOptions.Default.Version := value;
    end);

  cmd.Examples.Add('pack VSoft.CommandLine.dspec -version=1.0.1 -outputFolder=.\output');

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

  option := cmd.RegisterOption<string>('apiKey','a','Api Key for authenticated http source.',
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


  cmd.Examples.Add('push .\VSoft.SemanticVersion.1.0.1.dpkg -source=local');
  cmd.Examples.Add('push .\VSoft.SemanticVersion.1.0.1.dpkg -source=corporate -apiKey=abcdef');

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
//  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('sign', '', 'Signs all the packages matching the first argument with a certificate.',
                                                      'Specify the package(s) to sign (comma separated) and the certificate options.',
                                                      'sign <package(s)> [options]');

end;



procedure RegisterSourcesCommand;
var
  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('sources', '', 'Provides the ability to manage list of sources located in %AppData%\DPM\DPM.config',
                                                      '',
                                                      'sources <List|Add|Remove|Enable|Disable|Update> -Name [name] -Source [source] -type <Folder|DMPServer|GitHubDPM|GitHubDN>');
  option := cmd.RegisterUnNamedOption<TSourcesSubCommand>('<List|Add|Remove|Enable|Disable|Update>','operation',
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
  cmd := TOptionsRegistry.RegisterCommand('spec', '', 'Generates a package.dspec for a new package. If this command is run in the same folder as a ' +
                                                      'project file (.dproj), it will create a tokenized dspec file.',
                                                      '',
                                                      'spec [package id]');

  option := cmd.RegisterUnNamedOption<string>('Package Id (e.g VSoft.LibA)','packageId',
    procedure(const value : string)
    begin
      TSpecOptions.Default.PackageId := value;
    end);
  option.Required := false;

  option := cmd.RegisterOption<string>('from','f','The .dproj or file to base the spec file on',
    procedure(const value : string)
    begin
      TSpecOptions.Default.FromProject := value;
    end);
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
//  option : IOptionDefinition;
  cmd : TCommandDefinition;
begin
  cmd := TOptionsRegistry.RegisterCommand('verify', '', 'Verifies the signature of the specified package files.',
                                                      'Specify the package(s) to verify (comma separated).',
                                                      'verify <package(s)>');

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
  cmd.Examples.Add('uninstall VSoft.CommandLine c:\myprojects\project1.dproj -platforms=Win64,OSX64');
  cmd.Examples.Add('uninstall VSoft.CommandLine c:\myprojects\project1.groupproj');
end;



procedure RegisterWhyCommand;
var
  cmd : TCommandDefinition;
begin

  cmd := TOptionsRegistry.RegisterCommand('why', '', 'Explains why a package is referenced',
                                                      '',
                                                      'why',true);

end;

procedure RegisterInfoCommand;
var
  cmd : TCommandDefinition;
begin

  cmd := TOptionsRegistry.RegisterCommand('info', '', 'Show info about dpm config',
                                                      '',
                                                      'info',true);

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
  {$IFDEF DEBUG}
  //this is just for testing.
  RegisterFeedCommand;
  {$ENDIF}
  RegisterPackCommand;
  RegisterPushCommand;
  RegisterUninstallCommand;
  RegisterRestoreCommand;
  RegisterSetApiCommand;
  RegisterSignCommand;
  RegisterSourcesCommand;
  RegisterSpecCommand;
  RegisterUpdateCommand;
  RegisterVerifyCommand;
  RegisterWhyCommand;
  RegisterExitCodesCommand;
  RegisterInfoCommand;

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
