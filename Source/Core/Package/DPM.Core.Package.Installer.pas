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

unit DPM.Core.Package.Installer;

// TODO : Lots of common code between install and restore - refactor!

interface

uses
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Search,
  DPM.Core.Options.Install,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore,
  DPM.Core.Project.Interfaces,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Manifest.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Compiler.Interfaces;

type
  TPackageInstaller = class(TInterfacedObject, IPackageInstaller)
  private
    FLogger: ILogger;
    FConfigurationManager: IConfigurationManager;
    FRepositoryManager: IPackageRepositoryManager;
    FPackageCache: IPackageCache;
    FDependencyResolver: IDependencyResolver;
    FContext: IPackageInstallerContext;
    FCompilerFactory: ICompilerFactory;
  protected
    function Init(const options : TSearchOptions) : IConfiguration;
    function GetPackageInfo(const cancellationToken: ICancellationToken; const packageId: IPackageIdentity): IPackageInfo;
    function CreateProjectRefs(const cancellationToken: ICancellationToken; const rootnode: IPackageReference; const projectReferences: IList<IPackageReference>): boolean;

    function CollectSearchPaths(const packageGraph: IPackageReference; const resolvedPackages: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                                const compilerVersion: TCompilerVersion; const platform: TDPMPlatform;  const searchPaths: IList<string>): boolean;

    procedure GenerateSearchPaths(const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const packageSpec: IPackageManifest; const searchPaths: IList<string>);

    function DownloadPackages(const cancellationToken: ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageManifest>): boolean;

    function CollectPlatformsFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration) : boolean;

    function GetCompilerVersionFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration) : boolean;

    function CompilePackage(const cancellationToken: ICancellationToken; const Compiler: ICompiler; const packageInfo: IPackageInfo; const packageReference: IPackageReference;
                            const packageSpec: IPackageManifest;  const force: boolean; const forceDebug : boolean): boolean;

    function BuildDependencies(const cancellationToken: ICancellationToken; const packageCompiler: ICompiler; const projectPackageGraph: IPackageReference;
                               const packagesToCompile: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                               const packageManifests: IDictionary<string, IPackageManifest>; const Options: TSearchOptions): boolean;

    function CopyLocal(const cancellationToken: ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageManifest>;
                       const projectEditor: IProjectEditor; const platform: TDPMPlatform): boolean;

    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const platform: TDPMPlatform; const packageManifests: IDictionary<string, IPackageManifest>) : boolean;

    function DoRestoreProjectForPlatform(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const projectEditor: IProjectEditor;
                              const platform: TDPMPlatform; const config: IConfiguration; const context: IPackageInstallerContext): boolean;

    function DoInstallPackageForPlatform(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const projectFile: string; const projectEditor: IProjectEditor;
                              const platform: TDPMPlatform; const config: IConfiguration; const context: IPackageInstallerContext): boolean;

    function DoUninstallFromProject(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions; const projectFile: string; const projectEditor: IProjectEditor;
                                    const platform: TDPMPlatform; const config: IConfiguration; const context: IPackageInstallerContext): boolean;

    function DoCachePackage(const cancellationToken: ICancellationToken; const Options: TCacheOptions; const platform: TDPMPlatform): boolean;

    // works out what compiler/platform then calls DoInstallPackage
    function InstallPackage(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const projectEditor: IProjectEditor; const config: IConfiguration;
                            const context: IPackageInstallerContext): boolean;

    // user specified a package file - will install for single compiler/platform - calls InstallPackageFromId
    function InstallPackageFromFile(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const projectFiles: IList<string>; const config: IConfiguration;
                                    const context: IPackageInstallerContext): boolean;

    // resolves package from id - calls InstallPackage
    function InstallPackageFromId(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const projectFiles: IList<string>; const config: IConfiguration;
                                  const context: IPackageInstallerContext): boolean;

    function UnInstallFromProject(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions; const projectFile: string; const config: IConfiguration;
                                  const context: IPackageInstallerContext): boolean;

    function RestoreProject(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const config: IConfiguration;
                            const context: IPackageInstallerContext): boolean;

    // calls either InstallPackageFromId or InstallPackageFromFile depending on options.
    function Install(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const context: IPackageInstallerContext): boolean;


    function Uninstall(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions; const context: IPackageInstallerContext): boolean;
    // calls restore project
    function Restore(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const context: IPackageInstallerContext): boolean;

    function Remove(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions): boolean;

    function Cache(const cancellationToken: ICancellationToken; const Options: TCacheOptions): boolean;

  public
    constructor Create(const logger: ILogger;
      const configurationManager: IConfigurationManager;
      const repositoryManager: IPackageRepositoryManager;
      const packageCache: IPackageCache;
      const dependencyResolver: IDependencyResolver;
      const context: IPackageInstallerContext;
      const compilerFactory: ICompilerFactory);
  end;

implementation

uses
  System.IOUtils,
  System.Types,
  System.SysUtils,
  Spring,
  Spring.Collections.Extensions,
  VSoft.AntPatterns,
  DPM.Core.Constants,
  DPM.Core.Compiler.BOM,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Files,
  DPM.Core.Utils.System,
  DPM.Core.Project.Editor,
  DPM.Core.Project.GroupProjReader,
  DPM.Core.Options.List,
  DPM.Core.Dependency.Reference,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Classes,
  DPM.Core.Spec.Reader,
  DPM.Core.Package.InstallerContext;

{ TPackageInstaller }

function TPackageInstaller.Cache(const cancellationToken: ICancellationToken;  const Options: TCacheOptions): boolean;
var
  config: IConfiguration;
  platform: TDPMPlatform;
  platforms: TDPMPlatforms;
begin
  result := false;
  config := Init(Options);
  //logged in init
  if config = nil then
      exit;

  platforms := Options.platforms;

  if platforms = [] then
    platforms := AllPlatforms(Options.compilerVersion);

  result := true;
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit;
    Options.platforms := [platform];
    result := DoCachePackage(cancellationToken, Options, platform) and result;
  end;
end;

function TPackageInstaller.CollectPlatformsFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration): boolean;
var
  projectFile: string;
  projectEditor: IProjectEditor;
begin
  result := true;
  for projectFile in projectFiles do
  begin
    projectEditor := TProjectEditor.Create(FLogger, config, Options.compilerVersion);
    result := result and projectEditor.LoadProject(projectFile);
    if result then
      Options.platforms := Options.platforms + projectEditor.platforms;
  end;

end;

function TPackageInstaller.CollectSearchPaths(const packageGraph: IPackageReference; const resolvedPackages: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                                              const compilerVersion: TCompilerVersion; const platform: TDPMPlatform;  const searchPaths: IList<string>): boolean;
var
  packageInfo: IPackageInfo;
  packageMetadata: IPackageMetadata;
  packageSearchPath: string;
  packageBasePath: string;
begin
  result := true;

  // we need to apply usesource from the graph to the package info's
  packageGraph.VisitDFS(
    procedure(const node: IPackageReference)
    var
      pkgInfo: IPackageInfo;
    begin
      // not the most efficient thing to do
      pkgInfo := resolvedPackages.FirstOrDefault(
        function(const pkg: IPackageInfo): boolean
        begin
          result := SameText(pkg.Id, node.Id);
          //FLogger.Debug('Testing pkg [' + pkg.Id + '] against [' + node.Id + ']');
        end);
      Assert(pkgInfo <> nil, 'pkgInfo is null for id [' + node.Id + '], but should never be');
      pkgInfo.UseSource := pkgInfo.UseSource or node.UseSource;
    end);

  // reverse the list so that we add the paths in reverse order, small optimisation for the compiler.
  resolvedPackages.Reverse;
  for packageInfo in resolvedPackages do
  begin
    //if we have already compiled the package and we are not using the source directly, use the compiled dcus
    if (not packageInfo.UseSource) and compiledPackages.Contains(packageInfo)  then
    begin
      packageBasePath := packageInfo.Id + PathDelim +  packageInfo.Version.ToStringNoMeta + PathDelim;
      searchPaths.Add(packageBasePath + 'lib');
    end
    else
    begin
      packageMetadata := FPackageCache.GetPackageMetadata(packageInfo);
      if packageMetadata = nil then
      begin
        FLogger.Error('Unable to get metadata for package ' + packageInfo.ToString);
        exit(false);
      end;
      packageBasePath := packageMetadata.Id + PathDelim + packageMetadata.Version.ToStringNoMeta + PathDelim;

      for packageSearchPath in packageMetadata.searchPaths do
        searchPaths.Add(packageBasePath + packageSearchPath);
    end;
  end;
end;

function TPackageInstaller.CompilePackage(const cancellationToken : ICancellationToken; const Compiler: ICompiler; const packageInfo: IPackageInfo; const packageReference: IPackageReference;
                                          const packageSpec: IPackageManifest; const force: boolean; const forceDebug : boolean): boolean;
var
  buildEntry: ISpecBuildEntry;
  packagePath: string;
  projectFile: string;
  searchPaths: IList<string>;
  dependency : IPackageReference;
  bomNode: IPackageReference;
  bomFile: string;
  childSearchPath: string;
  configuration : string;

  procedure DoCopyFiles(const entry: ISpecBuildEntry);
  var
    copyEntry: ISpecCopyEntry;
    antPattern: IAntPattern;
    fsPatterns: TArray<IFileSystemPattern>;
    fsPattern: IFileSystemPattern;
    searchBasePath: string;
    Files: TStringDynArray;
    f: string;
    destFile: string;
  begin
    for copyEntry in entry.CopyFiles do
    begin
      FLogger.Debug('Post Compile Copy [' + copyEntry.Source + ']..');
      try
        // note : this can throw if the source path steps outside of the base path.
        searchBasePath := TPathUtils.StripWildCard(TPathUtils.CompressRelativePath(packagePath, copyEntry.Source));
        searchBasePath := ExtractFilePath(searchBasePath);

        antPattern := TAntPattern.Create(packagePath);
        fsPatterns := antPattern.Expand(copyEntry.Source);
        for fsPattern in fsPatterns do
        begin
          ForceDirectories(fsPattern.Directory);
          Files := TDirectory.GetFiles(fsPattern.Directory, fsPattern.FileMask, TSearchOption.soTopDirectoryOnly);
          for f in Files do
          begin
            // copy file to lib directory.
            if copyEntry.flatten then
              destFile := Compiler.LibOutputDir + '\' + ExtractFileName(f)
            else
              destFile := Compiler.LibOutputDir + '\' +
                TPathUtils.StripBase(searchBasePath, f);

            ForceDirectories(ExtractFilePath(destFile));

            // FLogger.Debug('Copying "' + f + '" to "' + destFile + '"');

            TFile.Copy(f, destFile, true);

          end;
        end;
      except
        on e: Exception do
        begin
          FLogger.Error('Error copying files to lib folder : ' + e.Message);
          raise;
        end;
      end;
      FLogger.Debug('Post Compile Copy [' + copyEntry.Source + ']..');

    end;
  end;

begin
  result := true;

  packagePath := FPackageCache.GetPackagePath(packageInfo);
  bomFile := TPath.Combine(packagePath, 'package.bom');

  if (not force) and FileExists(bomFile) then
  begin
    // Compare Bill of materials file against node dependencies to determine if we need to compile or not.
    // if the bom file exists that means it was compiled before. We will check that the bom matchs the dependencies
    // in the graph
    bomNode := TBOMFile.LoadFromFile(FLogger, bomFile);

    if bomNode <> nil then
    begin
      if bomNode.AreEqual(packageReference) then
      begin
        exit;
      end;
    end;
  end;

  // if we get here the previous compliation was done with different dependency versions,
  // so we delete the bom and compile again
  DeleteFile(bomFile);

  for buildEntry in packageSpec.TargetPlatform.BuildEntries do
  begin
    FLogger.Information('Building project : ' + buildEntry.Project);

    projectFile := TPath.Combine(packagePath, buildEntry.Project);
    projectFile := TPathUtils.CompressRelativePath('', projectFile);

    // if it's a design time package then we need to do a lot more work.
    // design time packages are win32 (as the IDE is win32) - since we can
    // only have one copy of the design package installed we need to check if
    // it has already been installed via another platform.

    if forceDebug then
      configuration := 'Debug'
    else
      configuration := buildEntry.Config;

    if buildEntry.DesignOnly and (packageInfo.platform <> TDPMPlatform.Win32)  then
    begin
      Compiler.BPLOutputDir := TPath.Combine(packagePath, buildEntry.BPLOutputDir);
      Compiler.LibOutputDir := TPath.Combine(packagePath, buildEntry.LibOutputDir);
      Compiler.Configuration := buildEntry.config;

      if Compiler.platform <> TDPMPlatform.Win32 then
      begin
        Compiler.BPLOutputDir := TPath.Combine(Compiler.BPLOutputDir, 'win32');
        Compiler.LibOutputDir := TPath.Combine(Compiler.LibOutputDir, 'win32');
      end
      else
      begin
        packageReference.LibPath := Compiler.LibOutputDir;
        packageReference.BplPath := Compiler.BPLOutputDir;
      end;

      if packageReference.HasChildren then
      begin
        searchPaths := TCollections.CreateList<string>;
        for dependency in packageReference.Children do
        begin
          childSearchPath := FPackageCache.GetPackagePath(dependency.Id, dependency.Version.ToStringNoMeta, Compiler.compilerVersion, Compiler.platform);
          childSearchPath := TPath.Combine(childSearchPath, 'lib\win32');
          searchPaths.Add(childSearchPath);
        end;
        Compiler.SetSearchPaths(searchPaths);
      end
      else
        Compiler.SetSearchPaths(nil);

      FLogger.Information('Building project [' + projectFile + '] for design time...');

      result := Compiler.BuildProject(cancellationToken, packageInfo.Platform, projectFile, configuration, packageInfo.Version, true);
      if result then
        FLogger.Success('Project [' + buildEntry.Project + '] build succeeded.')
      else
      begin
        if cancellationToken.IsCancelled then
          FLogger.Error('Building project [' + buildEntry.Project + '] cancelled.')
        else
          FLogger.Error('Building project [' + buildEntry.Project + '] failed.');
        exit;
      end;
      FLogger.NewLine;

    end
    else
    begin
      // note we are assuming the build entry paths are all relative.
      Compiler.BPLOutputDir := TPath.Combine(packagePath, buildEntry.BPLOutputDir);
      Compiler.LibOutputDir := TPath.Combine(packagePath, buildEntry.LibOutputDir);
      Compiler.Configuration := buildEntry.config;

      packageReference.LibPath := Compiler.LibOutputDir;
      packageReference.BplPath := Compiler.BPLOutputDir;

      if packageReference.HasChildren then
      begin
        searchPaths := TCollections.CreateList<string>;
        for dependency in packageReference.Children do
        begin
          childSearchPath := FPackageCache.GetPackagePath(dependency.Id, dependency.Version.ToStringNoMeta, Compiler.compilerVersion, Compiler.platform);
          childSearchPath := TPath.Combine(childSearchPath, 'lib');
          searchPaths.Add(childSearchPath);
        end;
        Compiler.SetSearchPaths(searchPaths);
      end
      else
        Compiler.SetSearchPaths(nil);

      result := Compiler.BuildProject(cancellationToken, packageInfo.Platform, projectFile, configuration, packageInfo.Version);
      if result then
        FLogger.Success('Project [' + buildEntry.Project + '] build succeeded.')
      else
      begin
        if cancellationToken.IsCancelled then
          FLogger.Error('Building project [' + buildEntry.Project + '] cancelled.')
        else
          FLogger.Error('Building project [' + buildEntry.Project + '] failed.');
        exit;
      end;
      FLogger.NewLine;

      if buildEntry.BuildForDesign and (Compiler.platform <> TDPMPlatform.Win32)  then
      begin
        FLogger.Information('Building project [' + projectFile + '] for design time support...');
        // if buildForDesign is true, then it means the design time bpl's also reference
        // this bpl, so if the platform isn't win32 then we need to build it for win32
        Compiler.BPLOutputDir := TPath.Combine(Compiler.BPLOutputDir, 'win32');
        Compiler.LibOutputDir := TPath.Combine(Compiler.LibOutputDir, 'win32');
        if packageReference.HasChildren then
        begin
          searchPaths := TCollections.CreateList<string>;
          for dependency in packageReference.Children do
          begin
            childSearchPath := FPackageCache.GetPackagePath(dependency.Id, dependency.Version.ToStringNoMeta, Compiler.compilerVersion, Compiler.platform);
            childSearchPath := TPath.Combine(childSearchPath, 'lib\win32');
            searchPaths.Add(childSearchPath);
          end;
          Compiler.SetSearchPaths(searchPaths);
        end
        else
          Compiler.SetSearchPaths(nil);

        result := Compiler.BuildProject(cancellationToken, packageInfo.Platform, projectFile, buildEntry.config, packageInfo.Version, true);
        if result then
          FLogger.Success('Project [' + buildEntry.Project + '] Compiled for designtime Ok.')
        else
        begin
          if cancellationToken.IsCancelled then
            FLogger.Error('Building project [' + buildEntry.Project + '] cancelled.')
          else
            FLogger.Error('Building project [' + buildEntry.Project + '] failed.');
          exit;
        end;

      end;

      if buildEntry.CopyFiles.Any then
        DoCopyFiles(buildEntry);

    end;

  end;
  // save the bill of materials file for future reference.
  TBOMFile.SaveToFile(FLogger, bomFile, packageReference);

end;


function TPackageInstaller.CopyLocal(const cancellationToken : ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageManifest>;
                                     const projectEditor: IProjectEditor; const platform: TDPMPlatform): boolean;
var
  configName: string;
  projectConfig: IProjectConfiguration;
  packageSpec: IPackageManifest;
  resolvedPackage: IPackageInfo;
  configNames: IReadOnlyList<string>;
  outputDir: string;
  lastOutputDir: string;
  bplSourceFile: string;
  bplTargetFile: string;
  packageFolder: string;
  runtimeCopyLocalFiles: TArray<ISpecBPLEntry>;
  runtimeEntry: ISpecBPLEntry;

begin
  result := true;

  configNames := projectEditor.GetConfigNames;

  for resolvedPackage in resolvedPackages do
  begin
    packageSpec := packageManifests[LowerCase(resolvedPackage.Id)];
    Assert(packageSpec <> nil);
    // FLogger.Debug('Copylocal for package [' + resolvedPackage.Id + ']');

    // TODO : Is there any point in the copylocal option now.. shouldn't all runtime bpls be copied?
    runtimeCopyLocalFiles := packageSpec.TargetPlatform.RuntimeFiles.Where(
      function(const entry: ISpecBPLEntry): boolean
      begin
        result := entry.CopyLocal;
      end).ToArray;

    // if no runtime bpl's are defined with copylocal in the dspec then there is nothing to do.
    if Length(runtimeCopyLocalFiles) = 0 then
      continue;

    lastOutputDir := '';
    packageFolder := FPackageCache.GetPackagePath(resolvedPackage);
    // FLogger.Debug('Package folder [' + packageFolder + ']');

    for configName in configNames do
    begin
      if configName = 'Base' then
        continue;
      // FLogger.Debug('Config [' + configName + ']');

      projectConfig := projectEditor.GetProjectConfiguration(configName,
        platform);
      // we're only doing this for projects using runtime configs.
      if not projectConfig.UsesRuntimePackages then
        continue;

      // FLogger.Debug('uses runtime packages');

      outputDir := projectConfig.outputDir;
      if (outputDir <> '') and (not SameText(outputDir, lastOutputDir)) then
      begin
        lastOutputDir := outputDir;

        for runtimeEntry in runtimeCopyLocalFiles do
        begin
          bplSourceFile := TPath.Combine(packageFolder, runtimeEntry.Source);
          if not FileExists(bplSourceFile) then
          begin
            FLogger.Warning('Unabled to find runtime package [' + bplSourceFile + '] during copy local');
            continue;
          end;
          bplTargetFile := TPath.Combine(outputDir,
            ExtractFileName(bplSourceFile));

          if TPathUtils.IsRelativePath(bplTargetFile) then
          begin
            bplTargetFile :=
              TPath.Combine(ExtractFilePath(projectEditor.projectFile), bplTargetFile);
            bplTargetFile := TPathUtils.CompressRelativePath('', bplTargetFile);
          end;

          // if the file exists already, then we need to work out if they are the same or not.
          if FileExists(bplTargetFile) and
            TFileUtils.AreSameFiles(bplSourceFile, bplTargetFile) then
            continue;
          // now actually copy files.
          try
            ForceDirectories(ExtractFilePath(bplTargetFile));
            TFile.Copy(bplSourceFile, bplTargetFile, true);
          except
            on e: Exception do
            begin
              FLogger.Warning('Unable to copy runtime package [' + bplSourceFile + '] to [' + bplTargetFile + '] during copy local');
              FLogger.Warning('  ' + e.Message);
            end;
          end;
        end;
      end;
    end;
  end;
end;

constructor TPackageInstaller.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const repositoryManager: IPackageRepositoryManager;
                                     const packageCache: IPackageCache; const dependencyResolver: IDependencyResolver; const context: IPackageInstallerContext;
                                     const compilerFactory: ICompilerFactory);
begin
  FLogger := logger;
  FConfigurationManager := configurationManager;
  FRepositoryManager := repositoryManager;
  FPackageCache := packageCache;
  FDependencyResolver := dependencyResolver;
  FContext := context;
  FCompilerFactory := compilerFactory;
end;

function TPackageInstaller.GetPackageInfo(const cancellationToken: ICancellationToken; const packageId: IPackageIdentity): IPackageInfo;
begin
  result := FPackageCache.GetPackageInfo(cancellationToken, packageId);
  if result = nil then
    result := FRepositoryManager.GetPackageInfo(cancellationToken, packageId);
end;

function TPackageInstaller.DoCachePackage(const cancellationToken : ICancellationToken; const Options: TCacheOptions; const platform: TDPMPlatform): boolean;
var
  packageIdentity: IPackageIdentity;
  packageInfo : IPackageInfo;
  packageFileName: string;
begin
  result := false;
  if not Options.Version.IsEmpty then
  begin
    packageIdentity := TPackageIdentity.Create('', Options.packageId, Options.Version, Options.compilerVersion, platform);
    // sourceName will be empty if we are installing the package from a file
    packageInfo := FRepositoryManager.GetPackageInfo(cancellationToken, packageIdentity);
  end
  else
  begin
    // no version specified, so we need to get the latest version available;
    packageInfo := FRepositoryManager.FindLatestVersion(cancellationToken, options.PackageId, options.CompilerVersion, TPackageVersion.Empty, platform, Options.PreRelease, options.Sources);
  end;
  if packageInfo = nil then
  begin
    FLogger.Error('Package [' + Options.packageId + '] for platform [' + DPMPlatformToString(platform) + '] not found on any sources');
    exit;
  end;
  FLogger.Information('Caching package ' + packageInfo.ToString);

  if not FPackageCache.EnsurePackage(packageInfo) then
  begin
    // not in the cache, so we need to get it from the the repository
    if not FRepositoryManager.DownloadPackage(cancellationToken, packageInfo, FPackageCache.PackagesFolder, packageFileName) then
    begin
      if cancellationToken.IsCancelled then
        FLogger.Error('Downloading package [' + packageInfo.ToString + '] cancelled.')
      else
        FLogger.Error('Failed to download package [' + packageInfo.ToString + ']');
      exit;
    end;
    if not FPackageCache.InstallPackageFromFile(packageFileName) then
    begin
      FLogger.Error('Failed to cache package file [' + packageFileName + '] into the cache');
      exit;
    end;
  end;
  result := true;
end;

// convert the dependency graph to a flat list of IPackageinfo's (which has dependencies).
function TPackageInstaller.CreateProjectRefs(const cancellationToken : ICancellationToken; const rootnode: IPackageReference;  const projectReferences: IList<IPackageReference>): boolean;
var
  seenPackages: IDictionary<string, IPackageInfo>;
  packageRefLookup : IDictionary<string, IPackageReference>;

  function DoCreateProjectRefs(const cancellationToken : ICancellationToken; const rootnode: IPackageReference) : boolean;
  var
    dependency: IPackageReference;
    info: IPackageInfo;
    newPackageRef: IPackageReference;

  begin
    result := false;
    if cancellationToken.IsCancelled then
      exit;
    //breadth first is important here.. we want to process top level nodes first!
    for dependency in rootNode.Children do
    begin
      if cancellationToken.IsCancelled then
        exit;

      if seenPackages.TryGetValue(LowerCase(dependency.Id), info) then
      begin
        if not packageRefLookup.TryGetValue(LowerCase(dependency.Id), newPackageRef) then
          raise Exception.Create('Package reference not found in lookup!');
        if dependency.UseSource then
        begin
          newPackageRef.UseSource := true;
          newPackageRef.PackageInfo.UseSource := true;
        end;
      end
      else
      begin
        info := GetPackageInfo(cancellationToken, dependency);
        if info = nil then
        begin
          FLogger.Error('Unable to resolve package : ' + dependency.ToIdVersionString);
          exit;
        end;
        if cancellationToken.IsCancelled then
          exit;

        info.UseSource := dependency.UseSource;

        newPackageRef := TPackageReference.Create(rootnode, info.Id, info.Version, info.Platform,info.CompilerVersion, dependency.VersionRange, dependency.UseSource);
        newPackageRef.PackageInfo := info;

        if newPackageRef.VersionRange.IsEmpty then
          newPackageRef.VersionRange := TVersionRange.Create(info.Version);

        packageRefLookup[LowerCase(dependency.Id)] := newPackageRef;
        seenPackages[LowerCase(dependency.Id)] := info;
        projectReferences.Add(newPackageRef);
      end;
    end;
    result := true;
    for dependency in rootnode.Children do
    begin
      if dependency.HasChildren then
        result := DoCreateProjectRefs(cancellationToken, dependency);
      if not result then
        exit;
      if cancellationToken.IsCancelled then
        exit;
    end;

  end;



begin
  seenPackages := TCollections.CreateDictionary<string, IPackageInfo>;
  packageRefLookup := TCollections.CreateDictionary<string, IPackageReference>;
  result := DoCreateProjectRefs(cancellationToken,rootnode);
end;

function TPackageInstaller.DoInstallPackageForPlatform(const cancellationToken : ICancellationToken; const Options: TInstallOptions; const projectFile: string;
                                            const projectEditor: IProjectEditor; const platform: TDPMPlatform; const config: IConfiguration;
                                            const context: IPackageInstallerContext): boolean;
var
  newPackageIdentity: IPackageIdentity;
  packageFileName: string;
  packageInfo: IPackageInfo; // includes dependencies;
  existingPackageRef: IPackageReference;
  dependency : IPackageReference;
  projectPackageGraph: IPackageReference;

  packageManifests: IDictionary<string, IPackageManifest>;

  projectReferences: IList<IPackageReference>;

  resolvedPackages: IList<IPackageInfo>;
  packagesToCompile: IList<IPackageInfo>;

  compiledPackages: IList<IPackageInfo>;
  packageSearchPaths: IList<string>;

  packageCompiler: ICompiler;

  seenPackages: IDictionary<string, IPackageInfo>;


begin
  result := false;

  projectPackageGraph := projectEditor.GetPackageReferences(platform); // can return nil

  if projectPackageGraph = nil then
    projectPackageGraph := TPackageReference.CreateRoot(Options.compilerVersion, platform);

  // see if it's already installed.
  existingPackageRef := projectPackageGraph.FindTopLevelChild(Options.packageId);
  if (existingPackageRef <> nil) then
  begin
    // if it's installed already and we're not forcing it to install or upgrading the version then we're done.
    if (not (Options.force or Options.IsUpgrade)) and (not existingPackageRef.IsTransitive) then
    begin
      // Note this error won't show from the IDE as we always force install from the IDE.
      FLogger.Error('Package [' + Options.packageId +  '] is already installed. Use option -force to force reinstall, or -upgrade to install a different version.');
      exit;
    end;
    // remove it so we can force resolution to happen later.
    projectPackageGraph.RemoveTopLevelChild(existingPackageRef.Id);
    existingPackageRef := nil; // we no longer need it.
  end;

  // We could have a transitive dependency that is being promoted.
  // Since we want to control what version is installed, we will remove
  // any transitive references to that package so the newly installed version
  // will take precedence when resolving.
  dependency := projectPackageGraph.FindFirstChild(Options.packageId);
  while dependency <> nil do
  begin
    projectPackageGraph.RemoveChild(dependency);
    dependency := projectPackageGraph.FindFirstChild(Options.packageId);
  end;

  // if the user specified a version, either the on the command line or via a file then we will use that
  if not Options.Version.IsEmpty then
  begin
    // sourceName will be empty if we are installing the package from a file
    newPackageIdentity := TPackageIdentity.Create(options.Sources, Options.packageId,  Options.Version, Options.compilerVersion, platform);
    packageInfo := GetPackageInfo(cancellationToken, newPackageIdentity);
  end
  else
  begin
    // no version specified, so we need to get the latest version available;
    packageInfo := FRepositoryManager.FindLatestVersion(cancellationToken, options.PackageId, options.CompilerVersion, TPackageVersion.Empty, platform, Options.PreRelease, options.Sources);

  end;
  if packageInfo = nil then
  begin
    FLogger.Error('Package [' + Options.packageId + '] for platform [' + DPMPlatformToString(platform) + '] not found on any sources');
    exit;
  end;
  FLogger.Information('Installing package ' + packageInfo.ToString);

  if not FPackageCache.EnsurePackage(packageInfo) then
  begin
    // not in the cache, so we need to get it from the the repository
    if not FRepositoryManager.DownloadPackage(cancellationToken, packageInfo, FPackageCache.PackagesFolder, packageFileName) then
    begin
      FLogger.Error('Failed to download package [' + packageInfo.ToString + ']');
      exit;
    end;
    if not FPackageCache.InstallPackageFromFile(packageFileName) then
    begin
      FLogger.Error('Failed to install package file [' + packageFileName + '] into the cache');
      exit;
    end;
  end;

  packageInfo.UseSource := Options.UseSource;
  // we need this later when collecting search paths.

  seenPackages := TCollections.CreateDictionary<string, IPackageInfo>;
  projectReferences := TCollections.CreateList<IPackageReference>;

  //flatten and dedupe the graph to a list of package references.
  if not CreateProjectRefs(cancellationToken, projectPackageGraph, projectReferences) then
    exit;

  if not FDependencyResolver.ResolveForInstall(cancellationToken, Options.CompilerVersion, platform, projectFile, Options, packageInfo, projectReferences, projectPackageGraph, resolvedPackages) then
  begin
    FLogger.Debug('ResolveForInstall failed');
    // we need to carry on here as resolution may have failed for another package
    // not sure what the best solution is here.. a better resolver perhaps.
     exit;
  end;

  if resolvedPackages = nil then
  begin
    FLogger.Error('Resolver returned no packages!');
    exit(false);
  end;

  //record the resolved package graph so we can detect conflicts between project.
  FContext.RecordGraph(projectFile, platform, projectPackageGraph);

  // get the package we were installing.
  packageInfo := resolvedPackages.FirstOrDefault(
    function(const info: IPackageInfo): boolean
    begin
      result := SameText(info.Id, packageInfo.Id);
    end);

  // this is just a sanity check, should never happen.
  if packageInfo = nil then
  begin
    FLogger.Error('Something went wrong, resolution did not return installed package!');
    exit(false);
  end;

  packageManifests := TCollections.CreateDictionary<string, IPackageManifest>;
  // downloads the package files to the cache if they are not already there and
  // returns the deserialized dspec as we need it for search paths and design
  if not DownloadPackages(cancellationToken, resolvedPackages, packageManifests) then
    exit;

  compiledPackages := TCollections.CreateList<IPackageInfo>;
  packagesToCompile := TCollections.CreateList<IPackageInfo>(resolvedPackages);
  packageSearchPaths := TCollections.CreateList<string>;
  packageCompiler := FCompilerFactory.CreateCompiler(Options.compilerVersion, platform);

  if not BuildDependencies(cancellationToken, packageCompiler,  projectPackageGraph, packagesToCompile, compiledPackages, packageManifests, Options) then
    exit;

  if not CollectSearchPaths(projectPackageGraph, resolvedPackages, compiledPackages, projectEditor.compilerVersion, platform,  packageSearchPaths) then
    exit;

  if not CopyLocal(cancellationToken, resolvedPackages, packageManifests, projectEditor, platform) then
    exit;

  if not InstallDesignPackages(cancellationToken,  projectFile, platform, packageManifests) then
    exit;

  if not projectEditor.AddSearchPaths(platform, packageSearchPaths, config.PackageCacheLocation) then
    exit;

  projectEditor.UpdatePackageReferences(projectPackageGraph, platform);
  result := projectEditor.SaveProject();

end;

function TPackageInstaller.DoRestoreProjectForPlatform(const cancellationToken : ICancellationToken; const Options: TRestoreOptions; const projectFile: string;
                                            const projectEditor: IProjectEditor; const platform: TDPMPlatform; const config: IConfiguration;
                                            const context: IPackageInstallerContext): boolean;
var
  projectPackageGraph: IPackageReference;
  projectReferences: IList<IPackageReference>;
  resolvedPackages: IList<IPackageInfo>;
  packagesToCompile: IList<IPackageInfo>;
  compiledPackages: IList<IPackageInfo>;
  packageSearchPaths: IList<string>;
  packageCompiler: ICompiler;
  packageManifests: IDictionary<string, IPackageManifest>; //TODO : Try rtl dictionary.
begin
  result := false;

  projectPackageGraph := projectEditor.GetPackageReferences(platform);
  // can return nil
  // if there is no project package graph then there is nothing to do.
  if projectPackageGraph = nil then
    exit(true);

  projectReferences := TCollections.CreateList<IPackageReference>;

  // TODO : Can packagerefs be replaced by just adding the info to the nodes?
  if not CreateProjectRefs(cancellationToken, projectPackageGraph, projectReferences) then
    exit;

  projectPackageGraph := nil;

  if not FDependencyResolver.ResolveForRestore(cancellationToken, Options.CompilerVersion, platform, projectFile, Options, projectReferences, projectPackageGraph, resolvedPackages) then
    exit;

  projectReferences := nil;

  // TODO : The code from here on is the same for install/uninstall/restore - refactor!!!

  if resolvedPackages = nil then
  begin
    FLogger.Error('Resolver returned no packages!');
    exit(false);
  end;

  FContext.RecordGraph(projectFile, platform, projectPackageGraph);

  packageManifests := TCollections.CreateDictionary<string, IPackageManifest>;
  // downloads the package files to the cache if they are not already there and
  // returns the deserialized dspec as we need it for search paths and
  if not DownloadPackages(cancellationToken, resolvedPackages, packageManifests) then
    exit;

  compiledPackages := TCollections.CreateList<IPackageInfo>;
  packagesToCompile := TCollections.CreateList<IPackageInfo>(resolvedPackages);
  packageSearchPaths := TCollections.CreateList<string>;
  packageCompiler := FCompilerFactory.CreateCompiler(Options.compilerVersion, platform);

  if not BuildDependencies(cancellationToken, packageCompiler, projectPackageGraph, packagesToCompile, compiledPackages, packageManifests, Options) then
    exit;

  if not CollectSearchPaths(projectPackageGraph, resolvedPackages, compiledPackages, projectEditor.compilerVersion, platform, packageSearchPaths) then
    exit;

  if not CopyLocal(cancellationToken, resolvedPackages, packageManifests, projectEditor, platform) then
    exit;

  if not InstallDesignPackages(cancellationToken, projectFile, platform, packageManifests) then
    exit;

  if not projectEditor.AddSearchPaths(platform, packageSearchPaths, config.PackageCacheLocation) then
    exit;

  projectEditor.UpdatePackageReferences(projectPackageGraph, platform);

  //trying to get a better stack trace
  packageManifests := nil;
  projectPackageGraph := nil;
  resolvedPackages := nil;
  packagesToCompile := nil;
  compiledPackages := nil;
  packageSearchPaths := nil;
  packageCompiler := nil;

  // TODO : need to detect if anything has actually changed and only save if it has.
  // saving triggers the IDE to reload (although we do work around that) - would be good to avoid.
  result := projectEditor.SaveProject();
end;

function TPackageInstaller.DoUninstallFromProject(const cancellationToken : ICancellationToken; const Options: TUnInstallOptions;const projectFile: string;
                                                  const projectEditor: IProjectEditor; const platform: TDPMPlatform; const config: IConfiguration;
                                                  const context: IPackageInstallerContext): boolean;
var
  projectPackageGraph: IPackageReference;
  foundReference: IPackageReference;
begin
  projectPackageGraph := projectEditor.GetPackageReferences(platform);
  // can return nil
  // if there is no project package graph then there is nothing to do.
  if projectPackageGraph = nil then
  begin
    FLogger.Information('Package [' + Options.packageId +  '] was not referenced in project [' + projectFile + '] for platform [' + DPMPlatformToString(platform) + '] - nothing to do.');
    exit(true);
  end;

  foundReference := projectPackageGraph.FindTopLevelChild(Options.packageId);

  if foundReference = nil then
  begin
    FLogger.Information('Package [' + Options.packageId + '] was not referenced in project [' + projectFile + '] for platform [' + DPMPlatformToString(platform) + '] - nothing to do.');
    // TODO : Should this fail with an error? It's a noop
    exit(true);
  end;

  //remove the node from the graph
  projectPackageGraph.RemoveTopLevelChild(foundReference.Id);

  //TODO : Context - Remove Design Packages if no longer referenced.

  //TODO : Tell context to upgrade graph.
//  FContext.PackageGraphTrimmed(projectFile, platform, projectPackageGraph)


  //now work out which search paths we can remove;
  //walk the package reference tree and check transitive dependencies
  foundReference.VisitDFS(
      procedure(const node: IPackageReference)
      begin
        //if there is no other transitive dependency then we can remove
        //from the search path.
        if not projectPackageGraph.HasAnyChild(node.Id) then
            projectEditor.RemoveFromSearchPath(platform, node.Id);
      end);


  projectEditor.UpdatePackageReferences(projectPackageGraph, platform);
  result := projectEditor.SaveProject();
end;

function TPackageInstaller.BuildDependencies(const cancellationToken : ICancellationToken; const packageCompiler: ICompiler; const projectPackageGraph: IPackageReference;
                                             const packagesToCompile: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                                             const packageManifests: IDictionary<string, IPackageManifest>; const Options: TSearchOptions): boolean;

begin
  result := false;
  try
    // build the dependency graph in the correct order.
    projectPackageGraph.VisitDFS(
      procedure(const packageReference: IPackageReference)
      var
        pkgInfo: IPackageInfo;
        Spec: IPackageManifest;
        otherNodes: IList<IPackageReference>;
        forceCompile: boolean;
      begin
        Assert(packageReference.IsRoot = false, 'graph should not visit root node');

        pkgInfo := packagesToCompile.FirstOrDefault(
          function(const value: IPackageInfo): boolean
          begin
            result := SameText(value.Id, packageReference.Id);
          end);
        // if it's not found that means we have already processed the package elsewhere in the graph
        if pkgInfo = nil then
          exit;

        // do we need an option to force compilation when restoring?
        forceCompile := Options.force and SameText(pkgInfo.Id, Options.SearchTerms); // searchterms backs packageid

        // removing it so we don't process it again
        packagesToCompile.Remove(pkgInfo);

        Spec := packageManifests[LowerCase(packageReference.Id)];
        Assert(Spec <> nil);

        if Spec.TargetPlatform.BuildEntries.Any then
        begin
          // we need to build the package.
          if not CompilePackage(cancellationToken, packageCompiler, pkgInfo, packageReference, Spec, forceCompile, Options.DebugMode) then
          begin
            if cancellationToken.IsCancelled then
              raise Exception.Create('Compiling package [' + pkgInfo.ToIdVersionString + '] cancelled.')
            else
              raise Exception.Create('Compiling package [' +  pkgInfo.ToIdVersionString + '] failed.');
          end;
          compiledPackages.Add(pkgInfo);
          // compiling updates the node searchpaths and libpath, so just copy to any same package nodes
          otherNodes := projectPackageGraph.FindChildren(packageReference.Id);
          if otherNodes.Count > 1 then
            otherNodes.ForEach(
              procedure(const otherNode: IPackageReference)
              begin
                otherNode.searchPaths.Clear;
                otherNode.searchPaths.AddRange(packageReference.searchPaths);
                otherNode.LibPath := packageReference.LibPath;
                otherNode.BplPath := packageReference.BplPath;
              end);
        end;
      end);
    result := true;

  except
    on e: Exception do
    begin
      FLogger.Error(e.Message);
      exit;
    end;
  end;

end;


function TPackageInstaller.DownloadPackages(const cancellationToken : ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageManifest>): boolean;
var
  packageInfo: IPackageInfo;
  packageFileName: string;
  manifest: IPackageManifest;
begin
  result := false;
  //TODO : Download in parallel
  for packageInfo in resolvedPackages do
  begin
    if cancellationToken.IsCancelled then
      exit;

    if not FPackageCache.EnsurePackage(packageInfo) then
    begin
      // not in the cache, so we need to get it from the the repository
      if not FRepositoryManager.DownloadPackage(cancellationToken, packageInfo, FPackageCache.PackagesFolder, packageFileName) then
      begin
        FLogger.Error('Failed to download package [' + packageInfo.ToString + ']');
        exit;
      end;
      if not FPackageCache.InstallPackageFromFile(packageFileName) then
      begin
        FLogger.Error('Failed to install package file [' + packageFileName + '] into the cache');
        exit;
      end;
      if cancellationToken.IsCancelled then
        exit;
    end;
    if not packageManifests.ContainsKey(LowerCase(packageInfo.Id)) then
    begin
      manifest := FPackageCache.GetPackageManifest(packageInfo);
      Assert(manifest <> nil);
      packageManifests[LowerCase(packageInfo.Id)] := manifest;
    end;

  end;
  result := true;

end;

function TPackageInstaller.InstallPackage(const cancellationToken : ICancellationToken; const Options: TInstallOptions; const projectEditor: IProjectEditor;
                                          const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  platforms: TDPMPlatforms;
  platform: TDPMPlatform;
  platformResult: boolean;
  ambiguousProjectVersion: boolean;
  ambiguousVersions: string;
  platformOptions: TInstallOptions;
  erroredPlatforms : TDPMPlatforms;
begin
  result := false;

  ambiguousProjectVersion := IsAmbigousProjectVersion(projectEditor.ProjectVersion, ambiguousVersions) and (not projectEditor.HasDPM);

  if ambiguousProjectVersion and (Options.compilerVersion = TCompilerVersion.UnknownVersion) then
    FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] is ambiguous (' + ambiguousVersions + '), recommend specifying compiler version on command line.');

  // if the compiler version was specified (either on the command like or through a package file)
  // then make sure our dproj is actually for that version.
  if Options.compilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    if projectEditor.compilerVersion <> Options.compilerVersion then
    begin
      if not ambiguousProjectVersion then
        FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] does not match the compiler version.');
      projectEditor.compilerVersion := Options.compilerVersion;
    end;
  end
  else
    Options.compilerVersion := projectEditor.compilerVersion;

  // if the platform was specified (either on the command like or through a package file)
  // then make sure our dproj is actually for that platform.
  if Options.platforms <> [] then
  begin
    platforms := Options.platforms * projectEditor.platforms; // get the intersection of the two sets.
    if platforms = [] then // no intersection
    begin
      FLogger.Warning('Skipping project file [' + projectEditor.ProjectFile + '] as it does not match target specified platforms.');
      exit;
    end;
    // TODO : what if only some of the platforms are supported, what should we do?
  end
  else
    platforms := projectEditor.platforms;

  result := true;
  erroredPlatforms := [];
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit;

    // do not modify the passed in options!
    platformOptions := Options.Clone;
    platformOptions.platforms := [platform];

    FLogger.Information('Installing [' + platformOptions.SearchTerms + '-' +  DPMPlatformToString(platform) + '] into [' + projectEditor.ProjectFile + ']', true);
    platformResult := DoInstallPackageForPlatform(cancellationToken, platformOptions, projectEditor.projectFile, projectEditor, platform, config, context);

    if not platformResult then
    begin
      FLogger.Error('Install failed for [' + platformOptions.SearchTerms + '-' + DPMPlatformToString(platform) + ']');
      Include(erroredPlatforms, platform);
    end
    else
      FLogger.Success('Install succeeded for [' + platformOptions.SearchTerms + '-' + DPMPlatformToString(platform) + ']', true);

    result := platformResult and result;
    FLogger.Information('');
  end;

  if not result then
    FLogger.Error('Install failed for [' + Options.SearchTerms + '] on platforms [' + DPMPlatformsToString(erroredPlatforms) + ']')

end;

procedure TPackageInstaller.GenerateSearchPaths(const compilerVersion : TCompilerVersion; const platform: TDPMPlatform; const packageSpec: IPackageManifest; const searchPaths: IList<string>);
var
  packageBasePath: string;
  packageSearchPath: ISpecSearchPath;
begin
  packageBasePath := packageSpec.Metadata.Id + PathDelim + packageSpec.Metadata.Version.ToStringNoMeta + PathDelim;

  for packageSearchPath in packageSpec.TargetPlatform.SearchPaths do
    searchPaths.Add(packageBasePath + packageSearchPath.Path);
end;

function TPackageInstaller.GetCompilerVersionFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration): boolean;
var
  projectFile: string;
  projectEditor: IProjectEditor;
  compilerVersion: TCompilerVersion;
  bFirst: boolean;
begin
  result := true;
  compilerVersion := TCompilerVersion.UnknownVersion;
  bFirst := true;
  for projectFile in projectFiles do
  begin
    projectEditor := TProjectEditor.Create(FLogger, config, Options.compilerVersion);
    result := result and projectEditor.LoadProject(projectFile);
    if result then
    begin
      if not bFirst then
      begin
        if projectEditor.compilerVersion <> compilerVersion then
        begin
          FLogger.Error('Projects are not all the for same compiler version.');
          result := false;
        end;
      end;
      compilerVersion := Options.compilerVersion;
      Options.compilerVersion := projectEditor.compilerVersion;
      bFirst := false;
    end;
  end;
end;

function TPackageInstaller.Init(const options: TSearchOptions): IConfiguration;
begin
  result := nil;
  if (not Options.Validated) and (not Options.Validate(FLogger)) then
    exit
  else if not Options.IsValid then
    exit;
  FConfigurationManager.EnsureDefaultConfig;
  result := FConfigurationManager.LoadConfig(Options.ConfigFile);
  if result = nil then
    exit;

  FPackageCache.Location := result.PackageCacheLocation;

  //also inits the repository manager.
  if not FDependencyResolver.Initialize(result) then
  begin
    FLogger.Error('Unable to initialize the dependency manager.');
    exit(nil);
  end;

  if not FRepositoryManager.HasSources then
  begin
    FLogger.Error
      ('No package sources are defined. Use `dpm sources add` command to add a package source.');
    exit(nil);
  end;


end;

function TPackageInstaller.Install(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const context: IPackageInstallerContext): boolean;
var
  config: IConfiguration;
  groupProjReader: IGroupProjectReader;
  projectList: IList<string>;
  i: integer;
  projectRoot: string;
  isGroup : boolean;
begin
  result := false;
  try
    config := Init(Options);
    //logged in Init
    if config = nil then
      exit;

    projectRoot := ExtractFilePath(Options.ProjectPath);
    projectList := TCollections.CreateList<string>;
    isGroup := false;
    if Length(options.Projects) > 0 then
    begin
      //validate
      for i := 0 to Length(options.Projects) -1 do
      begin
        if FileExists(options.Projects[i]) then
          projectList.Add(options.Projects[i])
        else
          FLogger.Warning('Project [' + options.Projects[i] + '] does not exist', true);
      end;
      if projectList.Count = 0 then
      begin
        FLogger.Error('No dproj files found in projectPath : ' + Options.ProjectPath);
        exit;
      end;
    end
    else if FileExists(Options.ProjectPath) then
    begin
      if ExtractFileExt(Options.ProjectPath) = '.groupproj' then
      begin
        isGroup := true;
        groupProjReader := TGroupProjectReader.Create(FLogger);
        if not groupProjReader.LoadGroupProj(Options.ProjectPath) then
          exit;

        if not groupProjReader.ExtractProjects(projectList) then
          exit;

        // projects in a project group are likely to be relative, so make them full paths
        for i := 0 to projectList.Count - 1 do
        begin
          // sysutils.IsRelativePath returns false with paths starting with .\
          if TPathUtils.IsRelativePath(projectList[i]) then
            // TPath.Combine really should do this but it doesn't
            projectList[i] := TPathUtils.CompressRelativePath(projectRoot, projectList[i])
        end;
      end
      else
        projectList.Add(Options.ProjectPath);
    end
    else if DirectoryExists(Options.ProjectPath) then
    begin
      //projectFiles := TArray<string>(TDirectory.GetFiles(Options.ProjectPath, '*.dproj'));
      projectList.AddRange(TDirectory.GetFiles(Options.ProjectPath, '*.dproj'));
      if projectList.Count = 0 then
      begin
        FLogger.Error('No dproj files found in projectPath : ' + Options.ProjectPath);
        exit;
      end;
      FLogger.Information('Found ' + IntToStr(projectList.Count) + ' dproj file(s) to install into.');
    end
    else
    begin
      // should never happen when called from the commmand line, but might from the IDE plugin.
      FLogger.Error('The projectPath provided does no exist, no project to install to');
      exit;
    end;

    if (Options.ProjectGroup <> '') and (not isGroup) then
    begin
      //TODO : load the project group and record the package graphs for the projects in the group
      //in the installer context so that package conflicts can be taken into account when installing
      //in a single project from the command line.

    end;

    if Options.PackageFile <> '' then
    begin
      if not FileExists(Options.PackageFile) then
      begin
        // should never happen if validation is called on the options.
        FLogger.Error('The specified packageFile [' + Options.PackageFile + '] does not exist.');
        exit;
      end;
      result := InstallPackageFromFile(cancellationToken, Options, projectList, config, context);
    end
    else
      result := InstallPackageFromId(cancellationToken, Options, projectList, config, context);
  except
    on e: Exception do
    begin
      FLogger.Error(e.Message);
      result := false;
    end;
  end;

end;

function TPackageInstaller.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const platform: TDPMPlatform; const packageManifests: IDictionary<string, IPackageManifest>): boolean;
begin
  //Note : we delegate this to the context as this is a no-op in the command line tool, the IDE plugin provides it's own context implementation.
  result := FContext.InstallDesignPackages(cancellationToken, projectFile, platform, packageManifests);
end;

function TPackageInstaller.InstallPackageFromFile(const cancellationToken : ICancellationToken; const Options: TInstallOptions;const projectFiles: IList<string>;
                                                  const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  packageIdString: string;
  packageIdentity: IPackageIdentity;
begin
  // get the package into the cache first then just install as normal
  result := FPackageCache.InstallPackageFromFile(Options.PackageFile);
  if not result then
    exit;

  // get the identity so we can get the compiler version
  packageIdString := ExtractFileName(Options.PackageFile);
  packageIdString := ChangeFileExt(packageIdString, '');
  if not TPackageIdentity.TryCreateFromString(FLogger, packageIdString, '', packageIdentity) then
    exit;

  // update options so we can install from the packageid.
  Options.PackageFile := '';
  Options.packageId := packageIdentity.Id + '.' +  packageIdentity.Version.ToStringNoMeta;
  Options.compilerVersion := packageIdentity.compilerVersion;
  // package file is for single compiler version
  Options.platforms := [packageIdentity.platform];
  // package file is for single platform.

  result := InstallPackageFromId(cancellationToken, options, projectFiles, config, context);
end;

function TPackageInstaller.InstallPackageFromId(const cancellationToken  : ICancellationToken; const Options: TInstallOptions; const projectFiles: IList<string>;
                                                const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  projectFile: string;
  i: integer;
  projectEditor : IProjectEditor;
  projectEditors : IList<IProjectEditor>;
  existingPackageRef: IPackageReference;
  projectPackageGraph: IPackageReference;
  dependency : IPackageReference;
  platform : TDPMPlatform;
begin
  result := true;

  projectEditors := TCollections.CreateList<IProjectEditor>;

  for i := 0 to projectFiles.Count - 1 do
  begin
    if cancellationToken.IsCancelled then
      exit;
    projectFile := projectFiles[i];
    if TPathUtils.IsRelativePath(projectFile) then
    begin
      projectFile := TPath.Combine(GetCurrentDir, projectFile);
      projectFile := TPathUtils.CompressRelativePath(projectFile);
    end;

    projectEditor := TProjectEditor.Create(FLogger, config, Options.compilerVersion);
    if not projectEditor.LoadProject(projectFile) then
    begin
      FLogger.Error('Unable to load project file, cannot continue');
      exit;
    end;

    //the command line might specify multiple platforms, the IDE UI always specifies a single platform;
    if Options.platforms <> [] then
    begin
      Options.platforms := Options.platforms * projectEditor.platforms; // get the intersection of the two sets.
      if Options.platforms = [] then // no intersection
      begin
        FLogger.Warning('Skipping project file [' + projectEditor.ProjectFile + '] as it does not match target specified platforms.');
        continue;
      end;
    end;

    projectEditors.Add(projectEditor);
  end;

  if cancellationToken.IsCancelled then
    exit;


  //if we are upgrading, remove package references and clean up the resolutions.
  for i := 0 to projectEditors.Count -1 do
  begin
    projectEditor := projectEditors[i];

    for platform in options.Platforms do
    begin
      projectPackageGraph := projectEditor.GetPackageReferences(platform); // can return nil

      if projectPackageGraph = nil then
        projectPackageGraph := TPackageReference.CreateRoot(Options.compilerVersion, platform);

      // see if it's already installed.
      existingPackageRef := projectPackageGraph.FindTopLevelChild(Options.packageId);
      if (existingPackageRef <> nil) then
      begin
        // if it's installed already and we're not forcing it to install or upgrading the version then we're done.
        if (not (Options.force or Options.IsUpgrade)) and (not existingPackageRef.IsTransitive) then
        begin
          // Note this error won't show from the IDE as we always force install from the IDE.
          FLogger.Error('Package [' + Options.packageId +  '] is already installed. Use option -force to force reinstall, or -upgrade to install a different version.');
          exit;
        end;
        // remove it so we can force resolution to happen later.
        projectPackageGraph.RemoveTopLevelChild(existingPackageRef.Id);
        FContext.RemoveResolution(platform, Options.PackageId); //this doesn't work due to how the context stores resolutions.
        existingPackageRef := nil; // we no longer need it.
      end;

      // We could have a transitive dependency that is being promoted.
      // Since we want to control what version is installed, we will remove
      // any transitive references to that package so the newly installed version
      // will take precedence when resolving.
      dependency := projectPackageGraph.FindFirstChild(Options.packageId);
      while dependency <> nil do
      begin
        projectPackageGraph.RemoveChild(dependency);
        dependency := projectPackageGraph.FindFirstChild(Options.packageId);
      end;
    end;
  end;

  for i := 0 to projectEditors.Count -1 do
  begin
    //TODO : Change to pass editor in rather than projectfile.
    result := InstallPackage(cancellationToken, Options, projectEditors[i], config, context) and result;
    projectEditors[i] := nil;
  end;





end;

function TPackageInstaller.Remove(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions): boolean;
begin
  result := false;
end;

function TPackageInstaller.Restore(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const context: IPackageInstallerContext): boolean;
var
  projectFiles: TArray<string>;
  projectFile: string;
  config: IConfiguration;
  GroupProjReader: IGroupProjectReader;
  projectList: IList<string>;
  i: integer;
  projectRoot: string;
  stopwatch : TStopwatch;
begin
  result := false;
  stopwatch := TStopwatch.Create;
  stopwatch.Start;
  try
    config := Init(Options);
    //logged in init
    if config = nil then
      exit;

    if FileExists(Options.ProjectPath) then
    begin
      // TODO : If we are using a groupProj then we shouldn't allow different versions of a package in different projects
      // need to work out how to detect this.

      if ExtractFileExt(Options.ProjectPath) = '.groupproj' then
      begin
        GroupProjReader := TGroupProjectReader.Create(FLogger);
        if not GroupProjReader.LoadGroupProj(Options.ProjectPath) then
          exit;

        projectList := TCollections.CreateList<string>;
        if not GroupProjReader.ExtractProjects(projectList) then
          exit;

        // projects in a project group are likely to be relative, so make them full paths
        projectRoot := ExtractFilePath(Options.ProjectPath);
        for i := 0 to projectList.Count - 1 do
        begin
          // sysutils.IsRelativePath returns false with paths starting with .\
          if TPathUtils.IsRelativePath(projectList[i]) then
            // TPath.Combine really should do this but it doesn't
            projectList[i] := TPathUtils.CompressRelativePath(projectRoot, projectList[i])
        end;
        projectFiles := projectList.ToArray;
      end
      else
      begin
        SetLength(projectFiles, 1);
        projectFiles[0] := Options.ProjectPath;
      end;
    end
    else if DirectoryExists(Options.ProjectPath) then
    begin
      // todo : add groupproj support!
      projectFiles := TArray<string>(TDirectory.GetFiles(Options.ProjectPath,
        '*.dproj'));
      if Length(projectFiles) = 0 then
      begin
        FLogger.Error('No project files found in projectPath : ' + Options.ProjectPath);
        exit;
      end;
      FLogger.Information('Found ' + IntToStr(Length(projectFiles)) +' project file(s) to restore.');
    end
    else
    begin
      // should never happen when called from the commmand line, but might from the IDE plugin.
      FLogger.Error('The projectPath provided does no exist, no project to install to');
      exit;
    end;

    result := true;
    for i := 0 to Length(projectFiles) - 1 do
    begin
      if cancellationToken.IsCancelled then
        exit;
      projectFile := projectFiles[i];

      if TPathUtils.IsRelativePath(projectFile) then
      begin
        projectFile := TPath.Combine(GetCurrentDir, projectFile);
        projectFile := TPathUtils.CompressRelativePath(projectFile);
      end;
      // order is important so we avoid shortcut boolean eval
      result := RestoreProject(cancellationToken, Options, projectFile, config, context) and result;
    end;
    stopwatch.Stop;
    if result then
      FLogger.Success('Restore succeeded in [' + IntToStr(stopwatch.ElapsedMilliseconds) + '] ms', true)
    else
      FLogger.Error('Restore failed in [' + IntToStr(stopwatch.ElapsedMilliseconds) + '] ms');


  except
    on e: Exception do
    begin
      FLogger.Error(e.Message);
      result := false;
    end;
  end;
end;

function TPackageInstaller.RestoreProject(const cancellationToken : ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const config: IConfiguration;
                                          const context: IPackageInstallerContext): boolean;
var
  projectEditor: IProjectEditor;
  platforms: TDPMPlatforms;
  platform: TDPMPlatform;
  errorPlatforms : TDPMPlatforms;
  platformResult: boolean;
  ambiguousProjectVersion: boolean;
  ambiguousVersions: string;
  platformOptions: TRestoreOptions;
begin
  result := false;

  // make sure we can parse the dproj
  projectEditor := TProjectEditor.Create(FLogger, config, Options.compilerVersion);

  if not projectEditor.LoadProject(projectFile) then
  begin
    FLogger.Error('Unable to load project file, cannot continue');
    exit;
  end;

  if cancellationToken.IsCancelled then
    exit;

  ambiguousProjectVersion := IsAmbigousProjectVersion(projectEditor.ProjectVersion, ambiguousVersions) and (not projectEditor.HasDPM);

  if ambiguousProjectVersion and
    (Options.compilerVersion = TCompilerVersion.UnknownVersion) then
    FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] is ambiguous (' + ambiguousVersions + '), recommend specifying compiler version on command line.');

  // if the compiler version was specified (either on the command like or through a package file)
  // then make sure our dproj is actually for that version.
  if Options.compilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    if projectEditor.compilerVersion <> Options.compilerVersion then
    begin
      if not ambiguousProjectVersion then
        FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] does not match the compiler version.');
      projectEditor.compilerVersion := Options.compilerVersion;
    end;
  end
  else
    Options.compilerVersion := projectEditor.compilerVersion;

  FLogger.Information('Restoring for compiler version  [' + CompilerToString(Options.compilerVersion) + '].', true);

  // if the platform was specified (either on the command like or through a package file)
  // then make sure our dproj is actually for that platform.
  if Options.platforms <> [] then
  begin
    platforms := Options.platforms * projectEditor.platforms;
    // gets the intersection of the two sets.
    if platforms = [] then // no intersection
    begin
      FLogger.Warning('Skipping project file [' + projectFile + '] as it does not match specified platforms.');
      exit;
    end;
    // TODO : what if only some of the platforms are supported, what should we do?
  end
  else
    platforms := projectEditor.platforms;

  result := true;
  errorPlatforms := [];
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit(false);

    // do not modify options here!
    platformOptions := Options.Clone;
    platformOptions.platforms := [platform];
    FLogger.Information('Restoring project [' + projectFile + '] for [' + DPMPlatformToString(platform) + ']', true);
    platformResult := DoRestoreProjectForPlatform(cancellationToken, platformOptions, projectFile, projectEditor, platform, config, context);
    if not platformResult then
    begin
      if cancellationToken.IsCancelled then
        FLogger.Error('Restore Cancelled.')
      else
      begin
        FLogger.Error('Restore failed for ' + DPMPlatformToString(platform));
        Include(errorPlatforms, platform);
      end;
    end
    else
      FLogger.Success('Restore succeeded for ' + DPMPlatformToString(platform), true);
    result := platformResult and result;
    FLogger.Information('');
  end;
  if not result then
    FLogger.Error('Restore failed for [' + projectFile + '] on platforms [' + DPMPlatformsToString(errorPlatforms) + ']')


end;

function TPackageInstaller.Uninstall(const cancellationToken : ICancellationToken; const Options: TUnInstallOptions; const context: IPackageInstallerContext): boolean;
var
  projectFiles: TArray<string>;
  projectFile: string;
  config: IConfiguration;
  GroupProjReader: IGroupProjectReader;
  projectList: IList<string>;
  i: integer;
  projectRoot: string;
begin
  result := false;
  // commandline would have validated already, but IDE probably not.
  if (not Options.Validated) and (not Options.Validate(FLogger)) then
    exit
  else if not Options.IsValid then
    exit;

  config := FConfigurationManager.LoadConfig(Options.ConfigFile);
  if config = nil then // no need to log, config manager will
    exit;

  FDependencyResolver.Initialize(config);

  FPackageCache.Location := config.PackageCacheLocation;
  if not FRepositoryManager.Initialize(config) then
  begin
    FLogger.Error('Unable to initialize the repository manager.');
    exit;
  end;

  if Length(options.Projects) > 0 then
  begin
    projectFiles := options.Projects;
  end
  else if FileExists(Options.ProjectPath) then
  begin
    // TODO : If we are using a groupProj then we shouldn't allow different versions of a package in different projects
    // need to work out how to detect this.

    if ExtractFileExt(Options.ProjectPath) = '.groupproj' then
    begin
      GroupProjReader := TGroupProjectReader.Create(FLogger);
      if not GroupProjReader.LoadGroupProj(Options.ProjectPath) then
        exit;

      projectList := TCollections.CreateList<string>;
      if not GroupProjReader.ExtractProjects(projectList) then
        exit;

      // projects in a project group are likely to be relative, so make them full paths
      projectRoot := ExtractFilePath(Options.ProjectPath);
      for i := 0 to projectList.Count - 1 do
      begin
        // sysutils.IsRelativePath returns false with paths starting with .\
        if TPathUtils.IsRelativePath(projectList[i]) then
          // TPath.Combine really should do this but it doesn't
          projectList[i] := TPathUtils.CompressRelativePath(projectRoot, projectList[i])
      end;
      projectFiles := projectList.ToArray;
    end
    else
    begin
      SetLength(projectFiles, 1);
      projectFiles[0] := Options.ProjectPath;
    end;
  end
  else if DirectoryExists(Options.ProjectPath) then
  begin
    // todo : add groupproj support!
    projectFiles := TArray<string>(TDirectory.GetFiles(Options.ProjectPath, '*.dproj'));
    if Length(projectFiles) = 0 then
    begin
      FLogger.Error('No project files found in projectPath : ' + Options.ProjectPath);
      exit;
    end;
    FLogger.Information('Found ' + IntToStr(Length(projectFiles)) + ' project file(s) to uninstall from.');
  end
  else
  begin
    // should never happen when called from the commmand line, but might from the IDE plugin.
    FLogger.Error('The projectPath provided does no exist, no project to install to');
    exit;
  end;

  result := true;
  // TODO : create some sort of context object here to pass in so we can collect runtime/design time packages
  for projectFile in projectFiles do
  begin
    if cancellationToken.IsCancelled then
      exit;
    result := UnInstallFromProject(cancellationToken, Options, projectFile, config, context) and result;
  end;

end;

function TPackageInstaller.UnInstallFromProject(const cancellationToken : ICancellationToken; const Options: TUnInstallOptions; const projectFile: string;
                                                const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  projectEditor: IProjectEditor;
  platforms: TDPMPlatforms;
  platform: TDPMPlatform;
  platformResult: boolean;
  ambiguousProjectVersion: boolean;
  ambiguousVersions: string;
begin
  result := false;

  // make sure we can parse the dproj
  projectEditor := TProjectEditor.Create(FLogger, config,
    Options.compilerVersion);

  if not projectEditor.LoadProject(projectFile) then
  begin
    FLogger.Error('Unable to load project file, cannot continue');
    exit;
  end;

  if cancellationToken.IsCancelled then
    exit;

  ambiguousProjectVersion := IsAmbigousProjectVersion(projectEditor.ProjectVersion, ambiguousVersions) and (not projectEditor.HasDPM);

  if ambiguousProjectVersion and (Options.compilerVersion = TCompilerVersion.UnknownVersion) then
    FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] is ambiguous (' + ambiguousVersions + '), recommend specifying compiler version on command line.');

  // if the compiler version was specified (either on the command like or through a package file)
  // then make sure our dproj is actually for that version.
  if Options.compilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    if projectEditor.compilerVersion <> Options.compilerVersion then
    begin
      if not ambiguousProjectVersion then
        FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] does not match the compiler version.');
      projectEditor.compilerVersion := Options.compilerVersion;
    end;
  end
  else
    Options.compilerVersion := projectEditor.compilerVersion;

  FLogger.Information('Uninstalling for compiler version  [' + CompilerToString(Options.compilerVersion) + '].', true);

  // if the platform was specified (either on the command like or through a package file)
  // then make sure our dproj is actually for that platform.
  if Options.platforms <> [] then
  begin
    platforms := Options.platforms * projectEditor.platforms;
    // gets the intersection of the two sets.
    if platforms = [] then // no intersection
    begin
      FLogger.Warning('Skipping project file [' + projectFile + '] as it does not match specified platforms.');
      exit;
    end;
    // TODO : what if only some of the platforms are supported, what should we do?
  end
  else
    platforms := projectEditor.platforms;

  result := true;
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit(false);

    Options.platforms := [platform];
    FLogger.Information('Uninstalling from project [' + projectFile + '] for ['  + DPMPlatformToString(platform) + ']', true);
    platformResult := DoUninstallFromProject(cancellationToken, Options, projectFile, projectEditor, platform, config, context);
    if not platformResult then
      FLogger.Error('Uninstall failed for ' + DPMPlatformToString(platform))
    else
      FLogger.Success('Uninstall succeeded for ' + DPMPlatformToString(platform), true);
    result := platformResult and result;
    FLogger.Information('');
  end;
end;

end.
