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

    function DownloadPackages(const cancellationToken: ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageSpec>): boolean;

    function CollectPlatformsFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration) : boolean;

    function GetCompilerVersionFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration) : boolean;

    function CompilePackage(const cancellationToken: ICancellationToken; const Compiler: ICompiler; const packageInfo: IPackageInfo; const packageReference: IPackageReference;
                            const packageSpec: IPackageSpec;  const force: boolean; const forceDebug : boolean): boolean;

    function BuildDependencies(const cancellationToken: ICancellationToken; const packageCompiler: ICompiler; const projectPackageGraph: IPackageReference;
                               const packagesToCompile: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                               const packageManifests: IDictionary<string, IPackageSpec>; const Options: TSearchOptions): boolean;

    function CopyLocal(const cancellationToken: ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageSpec>;
                       const projectEditor: IProjectEditor; const platform: TDPMPlatform): boolean;

    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageManifests: IDictionary<string, IPackageSpec>) : boolean;

    //Builds a {lowercase id -> manifest} dict by walking the resolved graph and reading manifests from the cache.
    //Used after the per-platform install/restore loop so design packages can be loaded once per project.
    function BuildPackageManifestsFromGraph(const graph: IPackageReference; const compilerVersion: TCompilerVersion): IDictionary<string, IPackageSpec>;

    //Restore fast path: validate the existing graph against each package's declared dependencies.
    //If every transient is present and within its parent's version range, the graph is good and we
    //can skip resolution. resolvedPackages is populated in topological order (deps before dependers).
    //Returns false if any violation is found - caller should fall back to a full re-resolve.
    function TryValidateRestoreGraph(const cancellationToken: ICancellationToken; const projectPackageGraph: IPackageReference;
                                     const compilerVersion: TCompilerVersion; const resolvedPackages: IList<IPackageInfo>): boolean;

    //Looks up an IPackageInfo for (id, version, compiler), caching the result in the supplied dict.
    //Hits cache first, then repo. Pulled out as a method because Delphi anonymous methods cannot
    //capture nested functions (see TryValidateRestoreGraph).
    function GetOrLoadPackageInfo(const cancellationToken: ICancellationToken; const id: string; const version: TPackageVersion;
                                  const compilerVersion: TCompilerVersion; const cache: IDictionary<string, IPackageInfo>): IPackageInfo;

    //Builds a fresh root with the top-level references cloned (no transients). Used by the restore
    //slow path so the resolver derives transients fresh from each top-level's manifest, ignoring any
    //stale or out-of-range transient versions the user may have left in the dproj.
    function BuildTopLevelOnlyGraph(const fullGraph: IPackageReference; const compilerVersion: TCompilerVersion): IPackageReference;

    //Restore slow-path: iterate top-level packages and call ResolveForInstall for each, accumulating
    //resolved refs across iterations so shared transients are seen by later top-levels and not
    //double-resolved. Returns the final graph + flat resolvedPackages list ready for FinalizePackageConfiguration.
    function RestoreUsingInstallResolver(const cancellationToken: ICancellationToken; const options: TRestoreOptions; const projectFile: string;
                                         const topLevelGraph: IPackageReference; out resultGraph: IPackageReference; out resolvedPackages: IList<IPackageInfo>): boolean;

    // Calculates the intersection of project platforms with platforms supported by ALL resolved packages
    function GetEffectivePlatforms(const projectPlatforms: TDPMPlatforms; const packageManifests: IDictionary<string, IPackageSpec>): TDPMPlatforms;

    // Common configuration logic for install/restore - builds, collects search paths, copies local, installs design packages
    function DoConfigurePackageForPlatform(const cancellationToken: ICancellationToken; const options: TSearchOptions;
      const projectFile: string; const projectEditor: IProjectEditor; const platform: TDPMPlatform;
      const config: IConfiguration; const projectPackageGraph: IPackageReference;
      const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageSpec>): boolean;

    // Resolves project files from path (handles .dproj, .groupproj, directory, or explicit list)
    function ResolveProjectFiles(const projectPath: string; const explicitProjects: TArray<string>;
      const projectList: IList<string>): boolean;

    // Validates and reconciles compiler version and platforms between options and project
    procedure ValidateAndSetCompilerPlatforms(const options: TSearchOptions; const projectEditor: IProjectEditor;
      out effectivePlatforms: TDPMPlatforms; out shouldSkip: boolean);

    function DoRestoreProjectForPlatform(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const projectEditor: IProjectEditor;
                              const platform: TDPMPlatform; const config: IConfiguration; const context: IPackageInstallerContext;
                              out resultGraph: IPackageReference): boolean;

    function DoInstallPackageForPlatform(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const projectFile: string; const projectEditor: IProjectEditor;
                              const platform: TDPMPlatform; const config: IConfiguration; const context: IPackageInstallerContext;
                              out resultGraph: IPackageReference): boolean;

    // Shared post-resolution logic: downloads packages, configures for platform, records graph
    function FinalizePackageConfiguration(const cancellationToken: ICancellationToken; const options: TSearchOptions;
      const projectFile: string; const projectEditor: IProjectEditor; const platform: TDPMPlatform;
      const config: IConfiguration; const projectPackageGraph: IPackageReference;
      const resolvedPackages: IList<IPackageInfo>; out resultGraph: IPackageReference): boolean;

    // Helper to create and load a project editor
    function LoadProjectEditor(const projectFile: string; const config: IConfiguration;
      const compilerVersion: TCompilerVersion; out projectEditor: IProjectEditor): boolean;

    function DoCachePackage(const cancellationToken: ICancellationToken; const Options: TCacheOptions): boolean;

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
begin
  result := false;
  config := Init(Options);
  //logged in init
  if config = nil then
      exit;

  //One .dpkg per compiler in the new model - it bundles every platform the package supports,
  //so a single download covers them all.
  result := DoCachePackage(cancellationToken, Options);
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
  packageManifest: IPackageSpec;
  template: ISpecTemplate;
  sourceEntry: ISpecSourceEntry;
  packageBasePath: string;
  destination: string;
  platformLibPath: string;
  seenPaths: IDictionary<string, boolean>;

  procedure AddUnique(const path: string);
  begin
    if not seenPaths.ContainsKey(LowerCase(path)) then
    begin
      seenPaths[LowerCase(path)] := true;
      searchPaths.Add(path);
    end;
  end;

begin
  result := true;
  seenPaths := TCollections.CreateDictionary<string, boolean>;

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
        end);
      Assert(pkgInfo <> nil, 'pkgInfo is null for id [' + node.Id + '], but should never be');
      pkgInfo.UseSource := pkgInfo.UseSource or node.UseSource;
    end);

  // reverse the list so that we add the paths in reverse order, small optimisation for the compiler.
  resolvedPackages.Reverse;
  for packageInfo in resolvedPackages do
  begin
    packageManifest := FPackageCache.GetPackageManifest(packageInfo);
    if packageManifest = nil then
    begin
      FLogger.Error('Unable to get manifest for package ' + packageInfo.ToString);
      exit(false);
    end;

    if (packageManifest.TargetPlatform = nil) or (packageManifest.TargetPlatform.TemplateName = '') then
    begin
      FLogger.Warning('Package [' + packageInfo.Id + '] manifest has no target platform / template - no search paths added');
      continue;
    end;

    template := packageManifest.FindTemplate(packageManifest.TargetPlatform.TemplateName);
    if template = nil then
    begin
      FLogger.Warning('Package [' + packageInfo.Id + '] template [' + packageManifest.TargetPlatform.TemplateName + '] not found - no search paths added');
      continue;
    end;

    packageBasePath := packageInfo.Id + PathDelim + packageInfo.Version.ToStringNoMeta + PathDelim;

    //If the package has build entries (compiled), point at the per-platform lib folder.
    //Otherwise it's a source-only package - add each source entry's destination folder so the
    //consumer can compile against the .pas files directly.
    if (template.BuildEntries.Count > 0) and (not packageInfo.UseSource) and compiledPackages.Contains(packageInfo) then
    begin
      platformLibPath := 'lib' + PathDelim + DPMPlatformToBDString(platform);
      AddUnique(packageBasePath + platformLibPath);
    end
    else
    begin
      for sourceEntry in template.SourceEntries do
      begin
        destination := sourceEntry.Destination;
        if destination = '' then
          destination := 'src';
        AddUnique(packageBasePath + destination);
      end;
    end;
  end;
end;

function TPackageInstaller.CompilePackage(const cancellationToken : ICancellationToken; const Compiler: ICompiler; const packageInfo: IPackageInfo; const packageReference: IPackageReference;
                                          const packageSpec: IPackageSpec; const force: boolean; const forceDebug : boolean): boolean;
var
  template: ISpecTemplate;
  buildEntry: ISpecBuildEntry;
  designEntry: ISpecDesignEntry;
  packagePath: string;
  projectFile: string;
  bomFile: string;
  configuration: string;
  searchPaths: IList<string>;
  dependency: IPackageReference;
  childSearchPath: string;
  bomNode: IPackageReference;
  supportedByCompiler: TDPMPlatforms;
  candidates: TDPMPlatforms;
  designPlatforms: TDPMPlatforms;
  designProjectEditor: IProjectEditor;
begin
  result := true;
  packagePath := FPackageCache.GetPackagePath(packageInfo);
  //BOM is per-platform: lib/{platform} folder is per-platform, so the compile-skip check must be too.
  //Otherwise a Win32 install leaves a BOM that incorrectly short-circuits a later Win64 install.
  bomFile := TPath.Combine(packagePath, 'package.' + DPMPlatformToBDString(Compiler.Platform) + '.bom');

  // BOM optimization: skip if dependencies unchanged
  if (not force) and FileExists(bomFile) then
  begin
    bomNode := TBOMFile.LoadFromFile(FLogger, bomFile);
    if (bomNode <> nil) and bomNode.AreEqual(packageReference) then
    begin
      FLogger.Information('Package [' + packageInfo.Id + '] [' + DPMPlatformToString(Compiler.Platform) + '] - dependencies unchanged, skipping compilation.');
      exit;
    end;
  end;
  DeleteFile(bomFile); // Dependencies changed or no BOM, rebuild

  // Get template for this package
  template := packageSpec.FindTemplate(packageSpec.TargetPlatform.TemplateName);
  if template = nil then
    exit(true); // No template = nothing to build

  if (not template.BuildEntries.Any) and (not template.DesignEntries.Any) then
    exit(true); // Nothing to compile

  // Setup configuration
  if forceDebug then
    configuration := 'Debug'
  else
    configuration := 'Release';

  // Setup compiler output directories (standardized paths)
  Compiler.BPLOutputDir := TPath.Combine(packagePath, 'bpl' + PathDelim + DPMPlatformToBDString(Compiler.Platform));
  Compiler.LibOutputDir := TPath.Combine(packagePath, 'lib' + PathDelim + DPMPlatformToBDString(Compiler.Platform));
  Compiler.Configuration := configuration;

  // Set library paths from dependencies
  if packageReference.HasChildren then
  begin
    searchPaths := TCollections.CreateList<string>;
    for dependency in packageReference.Children do
    begin
      childSearchPath := FPackageCache.GetPackagePath(dependency.Id, dependency.Version.ToStringNoMeta, Compiler.compilerVersion);
      childSearchPath := TPath.Combine(childSearchPath, 'lib' + PathDelim + DPMPlatformToBDString(Compiler.Platform));
      searchPaths.Add(childSearchPath);
    end;
    Compiler.SetSearchPaths(searchPaths);
  end
  else
    Compiler.SetSearchPaths(nil);

  // Compile build entries
  for buildEntry in template.BuildEntries do
  begin
    // Check platform filter if specified
    if (buildEntry.Platforms <> []) and (not (Compiler.Platform in buildEntry.Platforms)) then
      continue;

    FLogger.Information('Building project: ' + buildEntry.Project);
    projectFile := TPath.Combine(packagePath, buildEntry.Project);
    projectFile := TPathUtils.CompressRelativePath('', projectFile);

    result := Compiler.BuildProject(cancellationToken, Compiler.Platform, projectFile, configuration, packageInfo.Version, false);
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
  end;

  // Compile design entries
  // A design BPL can only be built when the current runtime platform (Compiler.Platform) is
  // both a supported design host for this compiler version AND declared by the manifest
  // (if explicit) or by the design .dproj's enabled platforms (if manifest silent). The design
  // .dpk references the runtime .dcp that was just built in lib\{Compiler.Platform}, so design
  // and runtime platforms must match. The Win32/Win64 matrix is produced across the series of
  // per-platform install/restore calls - each call contributes its matching design BPL.
  for designEntry in template.DesignEntries do
  begin
    projectFile := TPath.Combine(packagePath, designEntry.Project);

    supportedByCompiler := DesignTimePlatforms(Compiler.compilerVersion);

    if designEntry.Platforms <> [] then
      //manifest explicit - authoritative
      candidates := designEntry.Platforms
    else
    begin
      //defer to the design dproj - FConfig is only used by AddPackageReference which we don't call.
      designProjectEditor := TProjectEditor.Create(FLogger, nil, Compiler.compilerVersion);
      if designProjectEditor.LoadProject(projectFile) then
        candidates := designProjectEditor.Platforms
      else
      begin
        FLogger.Warning('Could not inspect design project [' + designEntry.Project + '] - defaulting to Win32 only.');
        candidates := [TDPMPlatform.Win32];
      end;
    end;

    designPlatforms := supportedByCompiler * candidates;

    if not (Compiler.Platform in designPlatforms) then
    begin
      FLogger.Debug('Skipping design package [' + designEntry.Project + '] - ' + DPMPlatformToString(Compiler.Platform) + ' is not a supported design platform for this entry.');
      continue;
    end;

    FLogger.Information('Building design package: ' + designEntry.Project + ' (' + DPMPlatformToString(Compiler.Platform) + ')');

    //output dirs and search paths are already set for Compiler.Platform by the runtime build above - nothing to change.
    result := Compiler.BuildProject(cancellationToken, Compiler.Platform, projectFile, configuration, packageInfo.Version, true);
    if not result then
    begin
      if cancellationToken.IsCancelled then
        FLogger.Error('Building design package [' + designEntry.Project + '] cancelled.')
      else
        FLogger.Error('Building design package [' + designEntry.Project + '] failed.');
      exit;
    end;
    FLogger.Success('Design package [' + designEntry.Project + '] build succeeded.');
  end;

  // Save the bill of materials file for future reference
  TBOMFile.SaveToFile(FLogger, bomFile, packageReference);
end;


function TPackageInstaller.CopyLocal(const cancellationToken : ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageSpec>;
                                     const projectEditor: IProjectEditor; const platform: TDPMPlatform): boolean;
//var
//  configName: string;
//  projectConfig: IProjectConfiguration;
//  packageSpec: IPackageSpec;
//  resolvedPackage: IPackageInfo;
//  configNames: IReadOnlyList<string>;
//  outputDir: string;
//  lastOutputDir: string;
//  bplSourceFile: string;
//  bplTargetFile: string;
//  packageFolder: string;
//  runtimeCopyLocalFiles: TArray<ISpecBPLEntry>;
//  runtimeEntry: ISpecBPLEntry;

begin
  result := true;

//  configNames := projectEditor.GetConfigNames;
//
//  for resolvedPackage in resolvedPackages do
//  begin
//    packageSpec := packageManifests[LowerCase(resolvedPackage.Id)];
//    Assert(packageSpec <> nil);
//    // FLogger.Debug('Copylocal for package [' + resolvedPackage.Id + ']');
//
//    // TODO : Is there any point in the copylocal option now.. shouldn't all runtime bpls be copied?
//    runtimeCopyLocalFiles := packageSpec.TargetPlatform.RuntimeFiles.Where(
//      function(const entry: ISpecBPLEntry): boolean
//      begin
//        result := entry.CopyLocal;
//      end).ToArray;
//
//    // if no runtime bpl's are defined with copylocal in the dspec then there is nothing to do.
//    if Length(runtimeCopyLocalFiles) = 0 then
//      continue;
//
//    lastOutputDir := '';
//    packageFolder := FPackageCache.GetPackagePath(resolvedPackage);
//    // FLogger.Debug('Package folder [' + packageFolder + ']');
//
//    for configName in configNames do
//    begin
//      if configName = 'Base' then
//        continue;
//      // FLogger.Debug('Config [' + configName + ']');
//
//      projectConfig := projectEditor.GetProjectConfiguration(configName,
//        platform);
//      // we're only doing this for projects using runtime configs.
//      if not projectConfig.UsesRuntimePackages then
//        continue;
//
//      // FLogger.Debug('uses runtime packages');
//
//      outputDir := projectConfig.outputDir;
//      if (outputDir <> '') and (not SameText(outputDir, lastOutputDir)) then
//      begin
//        lastOutputDir := outputDir;
//
//        for runtimeEntry in runtimeCopyLocalFiles do
//        begin
//          bplSourceFile := TPath.Combine(packageFolder, runtimeEntry.Source);
//          if not FileExists(bplSourceFile) then
//          begin
//            FLogger.Warning('Unabled to find runtime package [' + bplSourceFile + '] during copy local');
//            continue;
//          end;
//          bplTargetFile := TPath.Combine(outputDir,
//            ExtractFileName(bplSourceFile));
//
//          if TPathUtils.IsRelativePath(bplTargetFile) then
//          begin
//            bplTargetFile :=
//              TPath.Combine(ExtractFilePath(projectEditor.projectFile), bplTargetFile);
//            bplTargetFile := TPathUtils.CompressRelativePath('', bplTargetFile);
//          end;
//
//          // if the file exists already, then we need to work out if they are the same or not.
//          if FileExists(bplTargetFile) and
//            TFileUtils.AreSameFiles(bplSourceFile, bplTargetFile) then
//            continue;
//          // now actually copy files.
//          try
//            ForceDirectories(ExtractFilePath(bplTargetFile));
//            TFile.Copy(bplSourceFile, bplTargetFile, true);
//          except
//            on e: Exception do
//            begin
//              FLogger.Warning('Unable to copy runtime package [' + bplSourceFile + '] to [' + bplTargetFile + '] during copy local');
//              FLogger.Warning('  ' + e.Message);
//            end;
//          end;
//        end;
//      end;
//    end;
//  end;
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

function TPackageInstaller.DoCachePackage(const cancellationToken : ICancellationToken; const Options: TCacheOptions): boolean;
var
  packageIdentity: IPackageIdentity;
  packageInfo : IPackageInfo;
  packageFileName: string;
begin
  result := false;
  if not Options.Version.IsEmpty then
  begin
    packageIdentity := TPackageIdentity.Create('', Options.packageId, Options.Version, Options.compilerVersion);
    // sourceName will be empty if we are installing the package from a file
    packageInfo := FRepositoryManager.GetPackageInfo(cancellationToken, packageIdentity);
  end
  else
  begin
    // no version specified, so we need to get the latest version available;
    packageInfo := FRepositoryManager.FindLatestVersion(cancellationToken, options.PackageId, options.CompilerVersion, TPackageVersion.Empty, Options.PreRelease, options.Sources);
  end;
  if packageInfo = nil then
  begin
    FLogger.Error('Package [' + Options.packageId + '] for compiler [' + CompilerToString(Options.compilerVersion) + '] not found on any sources');
    exit;
  end;
  FLogger.Information('Caching package ' + packageInfo.ToString);

  if not FPackageCache.EnsurePackage(packageInfo) then
  begin
    // not in the cache, so we need to get it from the the repository
    if not FRepositoryManager.DownloadPackage(cancellationToken, packageInfo, FPackageCache.GetPackageFileFolder(packageInfo), packageFileName) then
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

        newPackageRef := TPackageReference.Create(rootnode, info.Id, info.Version, info.CompilerVersion, dependency.VersionRange, dependency.UseSource);
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
                                            const context: IPackageInstallerContext; out resultGraph: IPackageReference): boolean;
var
  newPackageIdentity: IPackageIdentity;
  packageFileName: string;
  packageInfo: IPackageInfo; // includes dependencies;
  existingPackageRef: IPackageReference;
  dependency : IPackageReference;
  projectPackageGraph: IPackageReference;

  projectReferences: IList<IPackageReference>;

  resolvedPackages: IList<IPackageInfo>;

  seenPackages: IDictionary<string, IPackageInfo>;


begin
  result := false;
  resultGraph := nil;

  projectPackageGraph := projectEditor.GetPackageReferences; // can return nil

  if projectPackageGraph = nil then
    projectPackageGraph := TPackageReference.CreateRoot(Options.compilerVersion);

  //If the package is already in the project graph, remove it so the resolver re-resolves below.
  //Install is idempotent: repeats for an already-built platform are short-circuited by the
  //per-platform BOM check; new platforms get compiled fresh. -force is still honored downstream
  //by CompilePackage (forces a rebuild even when the BOM would otherwise skip).
  existingPackageRef := projectPackageGraph.FindTopLevelChild(Options.packageId);
  if existingPackageRef <> nil then
  begin
    projectPackageGraph.RemoveTopLevelChild(existingPackageRef.Id);
    existingPackageRef := nil;
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
    newPackageIdentity := TPackageIdentity.Create(options.Sources, Options.packageId,  Options.Version, Options.compilerVersion);
    packageInfo := GetPackageInfo(cancellationToken, newPackageIdentity);
  end
  else
  begin
    // no version specified, so we need to get the latest version available;
    packageInfo := FRepositoryManager.FindLatestVersion(cancellationToken, options.PackageId, options.CompilerVersion, TPackageVersion.Empty,  Options.PreRelease, options.Sources);

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
    if not FRepositoryManager.DownloadPackage(cancellationToken, packageInfo, FPackageCache.GetPackageFileFolder(packageInfo), packageFileName) then
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

  if not FDependencyResolver.ResolveForInstall(cancellationToken, Options.CompilerVersion, projectFile, Options, packageInfo, projectReferences, projectPackageGraph, resolvedPackages) then
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

  // Install-specific sanity check: verify the package we were installing is in the resolved list
  packageInfo := resolvedPackages.FirstOrDefault(
    function(const info: IPackageInfo): boolean
    begin
      result := SameText(info.Id, Options.packageId);
    end);

  if packageInfo = nil then
  begin
    FLogger.Error('Something went wrong, resolution did not return installed package!');
    exit(false);
  end;

  // Finalize configuration (download, build, search paths, design packages, etc.)
  result := FinalizePackageConfiguration(cancellationToken, Options, projectFile, projectEditor, platform, config, projectPackageGraph, resolvedPackages, resultGraph);
end;

function TPackageInstaller.DoRestoreProjectForPlatform(const cancellationToken : ICancellationToken; const Options: TRestoreOptions; const projectFile: string;
                                            const projectEditor: IProjectEditor; const platform: TDPMPlatform; const config: IConfiguration;
                                            const context: IPackageInstallerContext; out resultGraph: IPackageReference): boolean;
var
  projectPackageGraph: IPackageReference;
  projectReferences: IList<IPackageReference>;
  resolvedPackages: IList<IPackageInfo>;
begin
  result := false;
  resultGraph := nil;

  projectPackageGraph := projectEditor.GetPackageReferences;
  // can return nil
  // if there is no project package graph then there is nothing to do.
  if projectPackageGraph = nil then
    exit(true);

  //Fast path: validate the existing graph against each package's declared dependencies.
  //If it's intact, skip the resolver entirely and proceed straight to download+configure.
  resolvedPackages := TCollections.CreateList<IPackageInfo>;
  if TryValidateRestoreGraph(cancellationToken, projectPackageGraph, Options.CompilerVersion, resolvedPackages) then
  begin
    FLogger.Verbose('Restore: existing graph is consistent with package manifests - skipping resolution');
    result := FinalizePackageConfiguration(cancellationToken, Options, projectFile, projectEditor, platform, config, projectPackageGraph, resolvedPackages, resultGraph);
    exit;
  end;

  //Slow path: graph has missing transients or out-of-range versions. Re-resolve from top-level
  //constraints by treating each top-level package as a fresh install, via ResolveForInstall. Each
  //iteration's resolved graph feeds the next as projectReferences so shared transients are reused
  //and not double-resolved.
  if not RestoreUsingInstallResolver(cancellationToken, Options, projectFile,
                                     BuildTopLevelOnlyGraph(projectPackageGraph, Options.CompilerVersion),
                                     projectPackageGraph, resolvedPackages) then
    exit;

  projectReferences := nil;

  // Finalize configuration (download, build, search paths, design packages, etc.)
  result := FinalizePackageConfiguration(cancellationToken, Options, projectFile, projectEditor, platform, config, projectPackageGraph, resolvedPackages, resultGraph);
end;

function TPackageInstaller.BuildDependencies(const cancellationToken : ICancellationToken; const packageCompiler: ICompiler; const projectPackageGraph: IPackageReference;
                                             const packagesToCompile: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                                             const packageManifests: IDictionary<string, IPackageSpec>; const Options: TSearchOptions): boolean;

begin
  result := false;
  try
    // build the dependency graph in the correct order.
    projectPackageGraph.VisitDFS(
      procedure(const packageReference: IPackageReference)
      var
        pkgInfo: IPackageInfo;
        Spec: IPackageSpec;
        template: ISpecTemplate;
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

        // removing it so we don't process it again
        packagesToCompile.Remove(pkgInfo);

        Spec := packageManifests[LowerCase(packageReference.Id)];
        Assert(Spec <> nil);

        // Get the template for this package
        if Spec.TargetPlatform = nil then
          exit;
        template := Spec.FindTemplate(Spec.TargetPlatform.TemplateName);
        if template = nil then
          exit;

        // Check if package has build entries that require compilation
        if template.BuildEntries.Any or template.DesignEntries.Any then
        begin
          // Determine if force compilation needed (only for explicitly requested package)
          forceCompile := Options.force and SameText(pkgInfo.Id, Options.SearchTerms);

          if not CompilePackage(cancellationToken, packageCompiler, pkgInfo, packageReference, Spec, forceCompile, false) then
          begin
            if not cancellationToken.IsCancelled then
              raise Exception.Create('Compiling package [' + pkgInfo.ToIdVersionString + '] failed.');
            exit;
          end;

          compiledPackages.Add(pkgInfo);
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


function TPackageInstaller.DownloadPackages(const cancellationToken : ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageSpec>): boolean;
var
  packageInfo: IPackageInfo;
  packageFileName: string;
  manifest: IPackageSpec;
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
      if not FRepositoryManager.DownloadPackage(cancellationToken, packageInfo, FPackageCache.GetPackageFileFolder(packageInfo), packageFileName) then
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

function TPackageInstaller.GetEffectivePlatforms(const projectPlatforms: TDPMPlatforms; const packageManifests: IDictionary<string, IPackageSpec>): TDPMPlatforms;
var
  packageId: string;
  manifest: IPackageSpec;
  packagePlatforms: TDPMPlatforms;
begin
  // Start with all project platforms
  result := projectPlatforms;

  // Intersect with each package's supported platforms
  for packageId in packageManifests.Keys do
  begin
    manifest := packageManifests[packageId];
    if (manifest <> nil) and (manifest.TargetPlatform <> nil) then
    begin
      packagePlatforms := manifest.TargetPlatform.Platforms;
      // Only intersect if the package actually declares platform support
      if packagePlatforms <> [] then
        result := result * packagePlatforms;  // Set intersection
    end;
  end;
end;

function TPackageInstaller.DoConfigurePackageForPlatform(const cancellationToken: ICancellationToken; const options: TSearchOptions;
  const projectFile: string; const projectEditor: IProjectEditor; const platform: TDPMPlatform;
  const config: IConfiguration; const projectPackageGraph: IPackageReference;
  const resolvedPackages: IList<IPackageInfo>; const packageManifests: IDictionary<string, IPackageSpec>): boolean;
var
  packagesToCompile: IList<IPackageInfo>;
  compiledPackages: IList<IPackageInfo>;
  packageSearchPaths: IList<string>;
  packageCompiler: ICompiler;
begin
  result := false;

  // Check if this platform is supported by all resolved packages
  // If not, skip configuration for this platform (not an error)
  if not (platform in GetEffectivePlatforms([platform], packageManifests)) then
  begin
    FLogger.Information('Platform [' + DPMPlatformToString(platform) + '] skipped - not supported by one or more packages', true);
    result := true;
    exit;
  end;

  compiledPackages := TCollections.CreateList<IPackageInfo>;
  packagesToCompile := TCollections.CreateList<IPackageInfo>(resolvedPackages);
  packageSearchPaths := TCollections.CreateList<string>;
  packageCompiler := FCompilerFactory.CreateCompiler(options.compilerVersion, platform);

  if not BuildDependencies(cancellationToken, packageCompiler, projectPackageGraph, packagesToCompile, compiledPackages, packageManifests, options) then
    exit;

  if not CollectSearchPaths(projectPackageGraph, resolvedPackages, compiledPackages, projectEditor.compilerVersion, platform, packageSearchPaths) then
    exit;

  if not CopyLocal(cancellationToken, resolvedPackages, packageManifests, projectEditor, platform) then
    exit;

  //Design packages are per-IDE/compiler, not per-platform - the call is hoisted out to the
  //post-platform-loop site in InstallPackage / RestoreProject.

  if not projectEditor.AddSearchPaths(platform, packageSearchPaths, config.PackageCacheLocation) then
    exit;

  result := true;
end;

function TPackageInstaller.FinalizePackageConfiguration(const cancellationToken: ICancellationToken; const options: TSearchOptions;
  const projectFile: string; const projectEditor: IProjectEditor; const platform: TDPMPlatform;
  const config: IConfiguration; const projectPackageGraph: IPackageReference;
  const resolvedPackages: IList<IPackageInfo>; out resultGraph: IPackageReference): boolean;
var
  packageManifests: IDictionary<string, IPackageSpec>;
begin
  result := false;
  resultGraph := nil;

  if resolvedPackages = nil then
  begin
    FLogger.Error('Resolver returned no packages!');
    exit;
  end;

  // Record the resolved package graph so we can detect conflicts between projects
  FContext.RecordGraph(projectFile, projectPackageGraph);

  packageManifests := TCollections.CreateDictionary<string, IPackageSpec>;
  // Downloads the package files to the cache if they are not already there and
  // returns the deserialized dspec as we need it for search paths and design
  if not DownloadPackages(cancellationToken, resolvedPackages, packageManifests) then
    exit;

  // Configure the package for this platform (build, search paths, copy local, design packages)
  if not DoConfigurePackageForPlatform(cancellationToken, options, projectFile, projectEditor, platform, config, projectPackageGraph, resolvedPackages, packageManifests) then
    exit;

  // Return the graph for caller to handle UpdatePackageReferences and SaveProject
  resultGraph := projectPackageGraph;
  result := true;
end;

function TPackageInstaller.LoadProjectEditor(const projectFile: string; const config: IConfiguration;
  const compilerVersion: TCompilerVersion; out projectEditor: IProjectEditor): boolean;
var
  fullPath: string;
begin
  result := false;
  projectEditor := nil;

  fullPath := projectFile;
  if TPathUtils.IsRelativePath(fullPath) then
  begin
    fullPath := TPath.Combine(GetCurrentDir, fullPath);
    fullPath := TPathUtils.CompressRelativePath(fullPath);
  end;

  projectEditor := TProjectEditor.Create(FLogger, config, compilerVersion);
  if not projectEditor.LoadProject(fullPath) then
  begin
    FLogger.Error('Unable to load project file [' + fullPath + '], cannot continue');
    exit;
  end;

  result := true;
end;

function TPackageInstaller.ResolveProjectFiles(const projectPath: string; const explicitProjects: TArray<string>;
  const projectList: IList<string>): boolean;
var
  groupProjReader: IGroupProjectReader;
  projectRoot: string;
  i: integer;
begin
  result := false;

  // If explicit projects were provided, use them
  if Length(explicitProjects) > 0 then
  begin
    for i := 0 to Length(explicitProjects) - 1 do
    begin
      if FileExists(explicitProjects[i]) then
        projectList.Add(explicitProjects[i])
      else
        FLogger.Warning('Project [' + explicitProjects[i] + '] does not exist', true);
    end;
    if projectList.Count = 0 then
    begin
      FLogger.Error('No valid project files found in the provided list');
      exit;
    end;
    result := true;
    exit;
  end;

  // Handle file path (single .dproj or .groupproj)
  if FileExists(projectPath) then
  begin
    if ExtractFileExt(projectPath) = '.groupproj' then
    begin
      groupProjReader := TGroupProjectReader.Create(FLogger);
      if not groupProjReader.LoadGroupProj(projectPath) then
        exit;
      if not groupProjReader.ExtractProjects(projectList) then
        exit;
      // Projects in a group are likely relative, so make them full paths
      projectRoot := ExtractFilePath(projectPath);
      for i := 0 to projectList.Count - 1 do
      begin
        if TPathUtils.IsRelativePath(projectList[i]) then
          projectList[i] := TPathUtils.CompressRelativePath(projectRoot, projectList[i]);
      end;
    end
    else
      projectList.Add(projectPath);
    result := true;
    exit;
  end;

  // Handle directory path
  if DirectoryExists(projectPath) then
  begin
    projectList.AddRange(TDirectory.GetFiles(projectPath, '*.dproj'));
    if projectList.Count = 0 then
    begin
      FLogger.Error('No dproj files found in projectPath : ' + projectPath);
      exit;
    end;
    FLogger.Information('Found ' + IntToStr(projectList.Count) + ' project file(s).');
    result := true;
    exit;
  end;

  FLogger.Error('The projectPath provided does not exist');
end;

procedure TPackageInstaller.ValidateAndSetCompilerPlatforms(const options: TSearchOptions; const projectEditor: IProjectEditor;
  out effectivePlatforms: TDPMPlatforms; out shouldSkip: boolean);
var
  ambiguousProjectVersion: boolean;
  ambiguousVersions: string;
begin
  shouldSkip := false;
  effectivePlatforms := [];

  ambiguousProjectVersion := IsAmbigousProjectVersion(projectEditor.ProjectVersion, ambiguousVersions) and (not projectEditor.HasDPM);

  if ambiguousProjectVersion and (options.compilerVersion = TCompilerVersion.UnknownVersion) then
    FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] is ambiguous (' + ambiguousVersions + '), recommend specifying compiler version on command line.');

  // If the compiler version was specified (either on the command line or through a package file)
  // then make sure our dproj is actually for that version.
  if options.compilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    if projectEditor.compilerVersion <> options.compilerVersion then
    begin
      if not ambiguousProjectVersion then
        FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] does not match the compiler version.');
      projectEditor.compilerVersion := options.compilerVersion;
    end;
  end
  else
    options.compilerVersion := projectEditor.compilerVersion;

  // If the platform was specified (either on the command line or through a package file)
  // then make sure our dproj is actually for that platform.
  if options.platforms <> [] then
  begin
    effectivePlatforms := options.platforms * projectEditor.platforms; // get the intersection
    if effectivePlatforms = [] then
    begin
      FLogger.Warning('Skipping project file [' + projectEditor.ProjectFile + '] as it does not match specified platforms.');
      shouldSkip := true;
    end;
  end
  else
    effectivePlatforms := projectEditor.platforms;
end;

function TPackageInstaller.InstallPackage(const cancellationToken : ICancellationToken; const Options: TInstallOptions; const projectEditor: IProjectEditor;
                                          const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  platforms: TDPMPlatforms;
  platform: TDPMPlatform;
  platformResult: boolean;
  shouldSkip: boolean;
  platformOptions: TInstallOptions;
  erroredPlatforms : TDPMPlatforms;
  finalGraph: IPackageReference;
  platformGraph: IPackageReference;
begin
  result := false;
  finalGraph := nil;

  ValidateAndSetCompilerPlatforms(Options, projectEditor, platforms, shouldSkip);
  if shouldSkip then
    exit;

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
    platformResult := DoInstallPackageForPlatform(cancellationToken, platformOptions, projectEditor.projectFile, projectEditor, platform, config, context, platformGraph);

    if not platformResult then
    begin
      FLogger.Error('Install failed for [' + platformOptions.SearchTerms + '-' + DPMPlatformToString(platform) + ']');
      Include(erroredPlatforms, platform);
    end
    else
    begin
      FLogger.Success('Install succeeded for [' + platformOptions.SearchTerms + '-' + DPMPlatformToString(platform) + ']', true);
      // Capture the graph from successful platform configuration (graph is same for all platforms)
      if finalGraph = nil then
        finalGraph := platformGraph;
    end;

    result := platformResult and result;
    FLogger.Information('');
  end;

  if not result then
    FLogger.Error('Install failed for [' + Options.SearchTerms + '] on platforms [' + DPMPlatformsToString(erroredPlatforms) + ']')
  else if finalGraph <> nil then
  begin
    // Update package references once using the new platform-independent format
    projectEditor.UpdatePackageReferences(finalGraph);
    result := projectEditor.SaveProject();
    //Load design-time packages once after all platforms are installed - design BPLs are per-IDE,
    //not per-platform, so this runs at the project level. CLI context is a no-op.
    if result then
      InstallDesignPackages(cancellationToken, projectEditor.ProjectFile, BuildPackageManifestsFromGraph(finalGraph, Options.compilerVersion));
  end;

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
  projectList: IList<string>;
  isGroup: boolean;
begin
  result := false;
  try
    config := Init(Options);
    if config = nil then
      exit;

    projectList := TCollections.CreateList<string>;
    if not ResolveProjectFiles(Options.ProjectPath, Options.Projects, projectList) then
      exit;

    isGroup := ExtractFileExt(Options.ProjectPath) = '.groupproj';

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

function TPackageInstaller.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageManifests: IDictionary<string, IPackageSpec>): boolean;
begin
  //Note : we delegate this to the context as this is a no-op in the command line tool, the IDE plugin provides it's own context implementation.
  result := FContext.InstallDesignPackages(cancellationToken, projectFile, packageManifests);
end;

function TPackageInstaller.BuildPackageManifestsFromGraph(const graph: IPackageReference; const compilerVersion: TCompilerVersion): IDictionary<string, IPackageSpec>;
var
  manifests: IDictionary<string, IPackageSpec>;
begin
  manifests := TCollections.CreateDictionary<string, IPackageSpec>;
  graph.VisitDFS(
    procedure(const node: IPackageReference)
    var
      identity: IPackageIdentity;
      manifest: IPackageSpec;
      key: string;
    begin
      key := LowerCase(node.Id);
      if manifests.ContainsKey(key) then
        exit;
      identity := TPackageIdentity.Create('', node.Id, node.Version, compilerVersion);
      manifest := FPackageCache.GetPackageManifest(identity);
      if manifest <> nil then
        manifests[key] := manifest;
    end);
  result := manifests;
end;

function TPackageInstaller.GetOrLoadPackageInfo(const cancellationToken: ICancellationToken; const id: string; const version: TPackageVersion;
                                                const compilerVersion: TCompilerVersion; const cache: IDictionary<string, IPackageInfo>): IPackageInfo;
var
  identity: IPackageIdentity;
  key: string;
begin
  key := LowerCase(id);
  if cache.TryGetValue(key, result) then
    exit;
  identity := TPackageIdentity.Create('', id, version, compilerVersion);
  result := GetPackageInfo(cancellationToken, identity); //tries cache then repo
  if result <> nil then
    cache[key] := result;
end;

function TPackageInstaller.BuildTopLevelOnlyGraph(const fullGraph: IPackageReference; const compilerVersion: TCompilerVersion): IPackageReference;
var
  child: IPackageReference;
  cloned: IPackageReference;
begin
  result := TPackageReference.CreateRoot(compilerVersion);
  for child in fullGraph.Children do
  begin
    cloned := child.Clone; //clone strips transient sub-children
    result.AddExistingChild(cloned.Id, cloned);
  end;
end;

function TPackageInstaller.RestoreUsingInstallResolver(const cancellationToken: ICancellationToken; const options: TRestoreOptions; const projectFile: string;
                                                       const topLevelGraph: IPackageReference; out resultGraph: IPackageReference; out resolvedPackages: IList<IPackageInfo>): boolean;
var
  topLevelChild: IPackageReference;
  topLevelIdentity: IPackageIdentity;
  topLevelInfo: IPackageInfo;
  accumulatedRefs: IList<IPackageReference>;
  iterationGraph: IPackageReference;
  iterationResolved: IList<IPackageInfo>;
begin
  result := false;
  resultGraph := nil;
  resolvedPackages := TCollections.CreateList<IPackageInfo>;
  accumulatedRefs := TCollections.CreateList<IPackageReference>;

  for topLevelChild in topLevelGraph.Children do
  begin
    if cancellationToken.IsCancelled then
      exit;

    //Load latest IPackageInfo for this top-level using its dproj-recorded version. The cache lookup
    //gives us deps populated from the manifest. If the user edited the top-level version to something
    //bogus, this returns nil and the restore can't proceed.
    topLevelIdentity := TPackageIdentity.Create('', topLevelChild.Id, topLevelChild.Version, options.CompilerVersion);
    topLevelInfo := GetPackageInfo(cancellationToken, topLevelIdentity);
    if topLevelInfo = nil then
    begin
      FLogger.Error('Restore: top-level package [' + topLevelChild.Id + '-' + topLevelChild.Version.ToStringNoMeta + '] not found in cache or repo');
      exit;
    end;

    if not FDependencyResolver.ResolveForInstall(cancellationToken, options.CompilerVersion, projectFile, options,
                                                 topLevelInfo, accumulatedRefs, iterationGraph, iterationResolved) then
    begin
      FLogger.Error('Restore: re-resolution failed for [' + topLevelChild.Id + ']');
      exit;
    end;

    //Carry the resolver's output forward as projectReferences for the next top-level so shared
    //transients are not re-resolved (and so prior top-levels stay in the final graph).
    resultGraph := iterationGraph;
    resolvedPackages := iterationResolved;
    accumulatedRefs := TCollections.CreateList<IPackageReference>;
    if not CreateProjectRefs(cancellationToken, iterationGraph, accumulatedRefs) then
      exit;
  end;

  result := resultGraph <> nil;
end;

function TPackageInstaller.TryValidateRestoreGraph(const cancellationToken: ICancellationToken; const projectPackageGraph: IPackageReference;
                                                   const compilerVersion: TCompilerVersion; const resolvedPackages: IList<IPackageInfo>): boolean;
var
  packageInfos: IDictionary<string, IPackageInfo>;
  validationOk: boolean;
  topLevelChild: IPackageReference;

  //Recursively validates a node and its declared dependencies.
  //Order matters: we always check the *parent's* declared range against the graph child's recorded
  //version BEFORE attempting to load the child's own info. That way a stale/out-of-range transient
  //is reported as "out of range" rather than "not found in cache or repo".
  procedure ValidateNode(const node: IPackageReference);
  var
    info: IPackageInfo;
    dep: IPackageDependency;
    child: IPackageReference;
  begin
    if not validationOk then
      exit;
    if cancellationToken.IsCancelled then
    begin
      validationOk := false;
      exit;
    end;
    info := GetOrLoadPackageInfo(cancellationToken, node.Id, node.Version, compilerVersion, packageInfos);
    if info = nil then
    begin
      FLogger.Information('Restore: package [' + node.Id + '-' + node.Version.ToStringNoMeta + '] not found in cache or repo - re-resolving');
      validationOk := false;
      exit;
    end;
    //For every dependency this package declares, verify the graph has a child for it whose version
    //satisfies the declared range. Range check happens BEFORE recursing so we don't try to load info
    //for a stale child id/version pair that isn't in the cache or repo.
    for dep in info.Dependencies do
    begin
      child := node.FindFirstChild(dep.Id);
      if child = nil then
      begin
        FLogger.Warning('Restore: package [' + node.Id + '] requires [' + dep.Id + '] which is missing from the project graph - re-resolving');
        validationOk := false;
        exit;
      end;
      if not dep.VersionRange.IsSatisfiedBy(child.Version) then
      begin
        FLogger.Warning('Restore: package [' + node.Id + '] dependency [' + child.Id + '-' + child.Version.ToStringNoMeta + '] is outside required range [' + dep.VersionRange.ToString + '] - re-resolving');
        validationOk := false;
        exit;
      end;
      //Only recurse if the child passed the range check.
      ValidateNode(child);
      if not validationOk then
        exit;
    end;
  end;

begin
  result := false;
  validationOk := true;
  packageInfos := TCollections.CreateDictionary<string, IPackageInfo>;

  for topLevelChild in projectPackageGraph.Children do
  begin
    ValidateNode(topLevelChild);
    if not validationOk then
      exit;
  end;

  //DFS yields children-before-parents - that is the order resolvedPackages needs (deps before dependers).
  projectPackageGraph.VisitDFS(
    procedure(const node: IPackageReference)
    var
      info: IPackageInfo;
      key: string;
      i: integer;
      alreadyAdded: boolean;
    begin
      key := LowerCase(node.Id);
      if not packageInfos.TryGetValue(key, info) then
        exit;
      //skip duplicates - shared transients can appear under multiple parents
      alreadyAdded := false;
      for i := 0 to resolvedPackages.Count - 1 do
      begin
        if SameText(resolvedPackages[i].Id, info.Id) then
        begin
          alreadyAdded := true;
          break;
        end;
      end;
      if not alreadyAdded then
        resolvedPackages.Add(info);
    end);

  result := true;
end;

function TPackageInstaller.InstallPackageFromFile(const cancellationToken : ICancellationToken; const Options: TInstallOptions;const projectFiles: IList<string>;
                                                  const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  packageIdString: string;
  packageIdentity: IPackageIdentity;
  packageManifest: IPackageSpec;
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
  Options.packageId := packageIdentity.Id;
  Options.Version := packageIdentity.Version;
  Options.compilerVersion := packageIdentity.compilerVersion;

  //Restrict to the platforms the package actually supports - read from the now-cached manifest.
  //ValidateAndSetCompilerPlatforms downstream intersects this with the project's enabled platforms.
  packageManifest := FPackageCache.GetPackageManifest(packageIdentity);
  if (packageManifest <> nil) and (packageManifest.TargetPlatform <> nil) then
    Options.platforms := packageManifest.TargetPlatform.Platforms
  else
  begin
    FLogger.Warning('Could not read manifest for [' + packageIdentity.ToString + '] - install will run for all project platforms');
    Options.platforms := [];
  end;

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
    //Per-project platform reconciliation happens later in ValidateAndSetCompilerPlatforms (called from
    //InstallPackage). Mutating Options.platforms here would leak the first project's platforms into the
    //second project's call and narrow installs across the directory.
    projectEditors.Add(projectEditor);
  end;

  if cancellationToken.IsCancelled then
    exit;


  //if we are upgrading, remove package references and clean up the resolutions.
  for i := 0 to projectEditors.Count -1 do
  begin
    projectEditor := projectEditors[i];

   // CONTINUE WORKING HERE!!!!  Need to change to justr single package reference per package no platforms.

    projectPackageGraph := projectEditor.GetPackageReferences; // can return nil

    if projectPackageGraph = nil then
      projectPackageGraph := TPackageReference.CreateRoot(Options.compilerVersion);

    //Install is idempotent - if the package is already in the graph, remove it so the resolver
    //re-resolves below. Repeats for an already-built platform are short-circuited by the
    //per-platform BOM check; new platforms get compiled fresh.
    existingPackageRef := projectPackageGraph.FindTopLevelChild(Options.packageId);
    if existingPackageRef <> nil then
    begin
      projectPackageGraph.RemoveTopLevelChild(existingPackageRef.Id);
      //No call to RemoveResolution here - FindPackageResolution skips the current project, so re-resolving
      //in this project doesn't see its own stale resolution. Other projects' recorded resolutions reflect
      //their dprojs and shouldn't be retroactively mutated by an install in a sibling project.
      existingPackageRef := nil;
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
  projectFile: string;
  config: IConfiguration;
  projectList: IList<string>;
  i: integer;
  stopwatch: TStopwatch;
begin
  result := false;
  stopwatch := TStopwatch.Create;
  stopwatch.Start;
  try
    config := Init(Options);
    if config = nil then
      exit;

    projectList := TCollections.CreateList<string>;
    if not ResolveProjectFiles(Options.ProjectPath, nil, projectList) then
      exit;

    result := true;
    for i := 0 to projectList.Count - 1 do
    begin
      if cancellationToken.IsCancelled then
        exit;
      projectFile := projectList[i];

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
  shouldSkip: boolean;
  platformOptions: TRestoreOptions;
  finalGraph: IPackageReference;
  platformGraph: IPackageReference;
begin
  result := false;
  finalGraph := nil;

  // Load and validate the project file
  if not LoadProjectEditor(projectFile, config, Options.compilerVersion, projectEditor) then
    exit;

  if cancellationToken.IsCancelled then
    exit;

  ValidateAndSetCompilerPlatforms(Options, projectEditor, platforms, shouldSkip);
  if shouldSkip then
    exit;

  FLogger.Information('Restoring for compiler version  [' + CompilerToString(Options.compilerVersion) + '].', true);

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
    platformResult := DoRestoreProjectForPlatform(cancellationToken, platformOptions, projectFile, projectEditor, platform, config, context, platformGraph);
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
    begin
      FLogger.Success('Restore succeeded for ' + DPMPlatformToString(platform), true);
      // Capture the graph from successful platform configuration (graph is same for all platforms)
      if finalGraph = nil then
        finalGraph := platformGraph;
    end;
    result := platformResult and result;
    FLogger.Information('');
  end;

  if not result then
    FLogger.Error('Restore failed for [' + projectFile + '] on platforms [' + DPMPlatformsToString(errorPlatforms) + ']')
  else if finalGraph <> nil then
  begin
    // Update package references once using the new platform-independent format
    projectEditor.UpdatePackageReferences(finalGraph);
    result := projectEditor.SaveProject();
    //Load design-time packages once after all platforms are restored - design BPLs are per-IDE,
    //not per-platform, so this runs at the project level. CLI context is a no-op.
    if result then
      InstallDesignPackages(cancellationToken, projectFile, BuildPackageManifestsFromGraph(finalGraph, Options.compilerVersion));
  end;

end;

function TPackageInstaller.Uninstall(const cancellationToken : ICancellationToken; const Options: TUnInstallOptions; const context: IPackageInstallerContext): boolean;
var
  projectFile: string;
  config: IConfiguration;
  projectList: IList<string>;
  i: integer;
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

  projectList := TCollections.CreateList<string>;
  if not ResolveProjectFiles(Options.ProjectPath, Options.Projects, projectList) then
    exit;

  result := true;
  for i := 0 to projectList.Count - 1 do
  begin
    if cancellationToken.IsCancelled then
      exit;
    projectFile := projectList[i];
    result := UnInstallFromProject(cancellationToken, Options, projectFile, config, context) and result;
  end;

end;

function TPackageInstaller.UnInstallFromProject(const cancellationToken : ICancellationToken; const Options: TUnInstallOptions; const projectFile: string;
                                                const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  projectEditor: IProjectEditor;
  platforms: TDPMPlatforms;
  platform: TDPMPlatform;
  shouldSkip: boolean;
  projectPackageGraph: IPackageReference;
  foundReference: IPackageReference;
  orphanedIds: IList<string>;
  orphanedId: string;
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

  ValidateAndSetCompilerPlatforms(Options, projectEditor, platforms, shouldSkip);
  if shouldSkip then
    exit;

  FLogger.Information('Uninstalling for compiler version  [' + CompilerToString(Options.compilerVersion) + '].', true);

  //The package graph is project-level, not per-platform. Find the top-level reference once and bail
  //if it isn't there - no work needed.
  projectPackageGraph := projectEditor.GetPackageReferences;
  if projectPackageGraph = nil then
  begin
    FLogger.Information('Package [' + Options.packageId + '] is not referenced in project [' + projectFile + '] - nothing to do.');
    exit(true);
  end;

  foundReference := projectPackageGraph.FindTopLevelChild(Options.packageId);
  if foundReference = nil then
  begin
    FLogger.Information('Package [' + Options.packageId + '] is not referenced in project [' + projectFile + '] - nothing to do.');
    exit(true);
  end;

  //Remove the top-level node first so HasAnyChild reflects the post-uninstall graph below.
  projectPackageGraph.RemoveTopLevelChild(foundReference.Id);

  //Walk the removed subtree and collect ids whose subtree is no longer referenced anywhere in the
  //remaining graph - those are the ones we need to drop from per-platform search paths. Transients
  //still pulled in by another top-level stay.
  orphanedIds := TCollections.CreateList<string>;
  foundReference.VisitDFS(
    procedure(const node: IPackageReference)
    begin
      if not projectPackageGraph.HasAnyChild(node.Id) then
        orphanedIds.Add(node.Id);
    end);

  //Search paths ARE per-platform (each <DPMSearch> has a Condition on $(Platform)) so this part
  //runs once per platform.
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit(false);
    FLogger.Information('Removing [' + Options.packageId + '] from search paths for [' + DPMPlatformToString(platform) + ']');
    for orphanedId in orphanedIds do
      projectEditor.RemoveFromSearchPath(platform, orphanedId);
  end;

  //Tell the context to unload design BPLs whose dpm package is no longer referenced by this project.
  if not context.UninstallDesignPackages(cancellationToken, projectFile, orphanedIds) then
    FLogger.Warning('Design-package unload reported failure - continuing.');

  //TODO : Tell context to upgrade graph (FContext.PackageGraphTrimmed once that exists).

  //Project-level: write the updated graph (single <Packages> node, no platform attribute) and save once.
  projectEditor.UpdatePackageReferences(projectPackageGraph);
  result := projectEditor.SaveProject();
  if result then
    FLogger.Success('Uninstall succeeded for [' + Options.packageId + ']', true);
end;

end.
