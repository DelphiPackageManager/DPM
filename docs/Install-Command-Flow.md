# DPM Install Command - Complete Program Flow Documentation

This document provides a thorough, detailed explanation of how the `dpm install` command works, tracing the complete program flow from command-line invocation through to final project file modification.

---

## Table of Contents

1. [Overview](#1-overview)
2. [Architecture Summary](#2-architecture-summary)
3. [Entry Points and Command Registration](#3-entry-points-and-command-registration)
4. [Command Initialization and Option Parsing](#4-command-initialization-and-option-parsing)
5. [The Package Installer Core](#5-the-package-installer-core)
6. [Project File Resolution](#6-project-file-resolution)
7. [Package Resolution and Download](#7-package-resolution-and-download)
8. [Dependency Resolution](#8-dependency-resolution)
9. [Package Cache Management](#9-package-cache-management)
10. [Package Configuration and Compilation](#10-package-configuration-and-compilation)
11. [Project File Modification](#11-project-file-modification)
12. [Multi-Project Coordination](#12-multi-project-coordination)
13. [Complete Flow Diagram](#13-complete-flow-diagram)
14. [Key Files Reference](#14-key-files-reference)
15. [Design Patterns Used](#15-design-patterns-used)

---

## 1. Overview

The DPM install command is responsible for:

1. **Parsing** user-provided options (package ID, version, project path, platforms)
2. **Resolving** project files from paths (single `.dproj`, `.groupproj`, or directories)
3. **Finding** the requested package from configured repositories
4. **Resolving** all transitive dependencies using a depth-first search algorithm
5. **Downloading** packages to the local cache
6. **Compiling** packages that have build entries (source-based packages)
7. **Modifying** the `.dproj` file to add search paths and package references

The command supports:
- Installing from a package ID: `dpm install Spring4D`
- Installing from a `.dpkg` file: `dpm install Spring4D-Delphi11-Win32-4.1.2.dpkg`
- Installing specific versions: `dpm install Spring4D --version=4.1.2`
- Installing to multiple projects in a project group
- Installing for specific platforms: `dpm install Spring4D --platforms=Win32,Win64`

---

## 2. Architecture Summary

### Key Components

| Component | Interface | Implementation | Purpose |
|-----------|-----------|----------------|---------|
| Install Command | `ICommandHandler` | `TInstallCommand` | CLI entry point |
| Package Installer | `IPackageInstaller` | `TPackageInstaller` | Core installation logic |
| Installer Context | `IPackageInstallerContext` | `TCorePackageInstallerContext` | Multi-project coordination |
| Dependency Resolver | `IDependencyResolver` | `TDependencyResolver` | Resolve dependencies |
| Package Cache | `IPackageCache` | `TPackageCache` | Local package storage |
| Repository Manager | `IPackageRepositoryManager` | `TPackageRepositoryManager` | Package source access |
| Project Editor | `IProjectEditor` | `TProjectEditor` | `.dproj` manipulation |
| Compiler Factory | `ICompilerFactory` | `TCompilerFactory` | Create compilers |

### Dependency Injection

All components are wired together using the Spring4D DI container, configured in:
- `Source/Cmdline/DPM.Console.Reg.pas`

---

## 3. Entry Points and Command Registration

### 3.1 Command Registration

**File:** `Source/Cmdline/DPM.Console.Reg.pas`

```pascal
container.RegisterType<ICommandHandler, TInstallCommand>('command.install');
```

The install command is registered with the DI container using the name `'command.install'`.

### 3.2 Command Resolution

**File:** `Source/Cmdline/DPM.Console.Application.pas`

When `dpm install` is executed:

1. `TDPMConsoleApplication.Run()` is called
2. Command-line arguments are parsed using `TOptionsRegistry.Parse()`
3. The command name (`install`) is extracted
4. `ICommandFactory` resolves the command from the DI container
5. The factory looks up `'command.install'` and returns `TInstallCommand`
6. `commandHandler.ExecuteCommand()` is invoked

### 3.3 TInstallCommand Class

**File:** `Source/Cmdline/Commands/DPM.Console.Command.Install.pas`

```pascal
TInstallCommand = class(TBaseCommand)
private
  FPackageInstaller : IPackageInstaller;
  FContext : IPackageInstallerContext;
protected
  function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
public
  constructor Create(
    const logger : ILogger;
    const configurationManager : IConfigurationManager;
    const packageInstaller : IPackageInstaller;
    const context : IPackageInstallerContext
  ); reintroduce;
end;
```

The constructor receives all dependencies via injection.

---

## 4. Command Initialization and Option Parsing

### 4.1 TInstallCommand.Execute

**File:** `Source/Cmdline/Commands/DPM.Console.Command.Install.pas:69-90`

```pascal
function TInstallCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
begin
  // 1. Apply common options (verbosity, config file, etc.)
  TInstallOptions.Default.ApplyCommon(TCommonOptions.Default);

  // 2. Default project path to current directory if not specified
  if TInstallOptions.Default.ProjectPath = '' then
    TInstallOptions.Default.ProjectPath := GetCurrentDir;

  // 3. Validate all options
  if not TInstallOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  // 4. Delegate to the package installer
  if not FPackageInstaller.Install(cancellationToken, TInstallOptions.Default, FContext) then
    result := TExitCode.Error
  else
    result := TExitCode.OK;
end;
```

### 4.2 TInstallOptions Class

**File:** `Source/Core/Options/DPM.Core.Options.Install.pas`

```pascal
TInstallOptions = class(TSearchOptions)
private
  FPackageFile : string;      // Direct .dpkg file path
  FVersionString : string;    // Version string (e.g., "4.1.2")
  FNoCache : boolean;         // Skip cache check
  FProjectPath : string;      // Target project/directory
  FProjects : TArray<string>; // Explicit project list
  FProjectGroup : string;     // Project group file
  FFloat : boolean;           // Allow floating versions
  FIsUpgrade : boolean;       // Upgrade existing package
```

### 4.3 Option Validation

**File:** `Source/Core/Options/DPM.Core.Options.Install.pas:117-176`

The `Validate` method:

1. Checks if a config file is specified
2. Determines if input is a package ID or package file (using regex)
3. Validates version string if provided
4. Ensures project path is not empty
5. Validates the package file exists if specified

```pascal
function TInstallOptions.Validate(const logger : ILogger) : Boolean;
begin
  // Inherits base validation
  result := inherited Validate(logger);

  // Check if packageId matches the package file regex
  if TRegEx.IsMatch(PackageId, cPackageIdRegex) then
    FPackageFile := ''  // It's a package ID
  else if not FileExists(FPackageFile) then
  begin
    Logger.Error('The specified packageFile does not exist.');
    result := false;
  end;

  // Validate version string
  if VersionString <> '' then
  begin
    if not TPackageVersion.TryParseWithError(VersionString, theVersion, error) then
    begin
      Logger.Error('Invalid version');
      result := false;
    end;
    Self.Version := theVersion;
  end;
end;
```

---

## 5. The Package Installer Core

### 5.1 TPackageInstaller Class

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas`

This is the heart of the installation system (~1770 lines). It coordinates all installation activities.

```pascal
TPackageInstaller = class(TInterfacedObject, IPackageInstaller)
private
  FLogger: ILogger;
  FConfigurationManager: IConfigurationManager;
  FRepositoryManager: IPackageRepositoryManager;
  FPackageCache: IPackageCache;
  FDependencyResolver: IDependencyResolver;
  FContext: IPackageInstallerContext;
  FCompilerFactory: ICompilerFactory;
```

### 5.2 Main Entry: Install Method

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:1361-1407`

```pascal
function TPackageInstaller.Install(
  const cancellationToken: ICancellationToken;
  const Options: TInstallOptions;
  const context: IPackageInstallerContext
): boolean;
var
  config: IConfiguration;
  projectList: IList<string>;
begin
  result := false;

  // 1. Initialize configuration and dependency resolver
  config := Init(Options);
  if config = nil then
    exit;

  // 2. Resolve project files from the path
  projectList := TCollections.CreateList<string>;
  if not ResolveProjectFiles(Options.ProjectPath, Options.Projects, projectList) then
    exit;

  // 3. Route to appropriate installer based on input type
  if Options.PackageFile <> '' then
    result := InstallPackageFromFile(cancellationToken, Options, projectList, config, context)
  else
    result := InstallPackageFromId(cancellationToken, Options, projectList, config, context);
end;
```

### 5.3 Initialization

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:1330-1359`

The `Init` method:

1. Validates options
2. Ensures default config exists
3. Loads configuration from file
4. Sets package cache location
5. Initializes dependency resolver (which initializes repository manager)
6. Verifies package sources exist

```pascal
function TPackageInstaller.Init(const options: TSearchOptions): IConfiguration;
begin
  // Validate options
  if (not Options.Validated) and (not Options.Validate(FLogger)) then
    exit(nil);

  // Ensure and load config
  FConfigurationManager.EnsureDefaultConfig;
  result := FConfigurationManager.LoadConfig(Options.ConfigFile);

  // Configure cache location
  FPackageCache.Location := result.PackageCacheLocation;

  // Initialize resolver (also initializes repository manager)
  if not FDependencyResolver.Initialize(result) then
    exit(nil);

  // Verify we have package sources
  if not FRepositoryManager.HasSources then
  begin
    FLogger.Error('No package sources are defined.');
    exit(nil);
  end;
end;
```

---

## 6. Project File Resolution

### 6.1 ResolveProjectFiles Method

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:1115-1182`

This method handles four scenarios:

#### Scenario 1: Explicit Project List
```pascal
if Length(explicitProjects) > 0 then
begin
  for i := 0 to Length(explicitProjects) - 1 do
    if FileExists(explicitProjects[i]) then
      projectList.Add(explicitProjects[i]);
end;
```

#### Scenario 2: Single .dproj File
```pascal
if ExtractFileExt(projectPath) <> '.groupproj' then
  projectList.Add(projectPath);
```

#### Scenario 3: .groupproj File
```pascal
if ExtractFileExt(projectPath) = '.groupproj' then
begin
  groupProjReader := TGroupProjectReader.Create(FLogger);
  groupProjReader.LoadGroupProj(projectPath);
  groupProjReader.ExtractProjects(projectList);

  // Convert relative paths to absolute
  projectRoot := ExtractFilePath(projectPath);
  for i := 0 to projectList.Count - 1 do
    if TPathUtils.IsRelativePath(projectList[i]) then
      projectList[i] := TPathUtils.CompressRelativePath(projectRoot, projectList[i]);
end;
```

#### Scenario 4: Directory
```pascal
if DirectoryExists(projectPath) then
begin
  projectList.AddRange(TDirectory.GetFiles(projectPath, '*.dproj'));
end;
```

---

## 7. Package Resolution and Download

### 7.1 InstallPackageFromFile

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:1415-1440`

When installing from a `.dpkg` file:

```pascal
function TPackageInstaller.InstallPackageFromFile(...): boolean;
begin
  // 1. Install the package file into the cache
  result := FPackageCache.InstallPackageFromFile(Options.PackageFile);

  // 2. Parse the package identity from the filename
  //    e.g., "Spring4D-Delphi11-Win32-4.1.2.dpkg"
  packageIdString := ExtractFileName(Options.PackageFile);
  packageIdString := ChangeFileExt(packageIdString, '');
  TPackageIdentity.TryCreateFromString(FLogger, packageIdString, '', packageIdentity);

  // 3. Update options to use package ID format
  Options.PackageFile := '';
  Options.packageId := packageIdentity.Id + '.' + packageIdentity.Version.ToStringNoMeta;
  Options.compilerVersion := packageIdentity.compilerVersion;

  // 4. Continue with standard ID-based install
  result := InstallPackageFromId(cancellationToken, options, projectFiles, config, context);
end;
```

### 7.2 InstallPackageFromId

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:1442-1546`

This is the main installation path:

```pascal
function TPackageInstaller.InstallPackageFromId(...): boolean;
begin
  projectEditors := TCollections.CreateList<IProjectEditor>;

  // 1. Load all project files into editors
  for projectFile in projectFiles do
  begin
    projectEditor := TProjectEditor.Create(FLogger, config, Options.compilerVersion);
    projectEditor.LoadProject(projectFile);

    // Calculate effective platforms (intersection of options and project)
    if Options.platforms <> [] then
      Options.platforms := Options.platforms * projectEditor.platforms;
    else
      options.Platforms := projectEditor.Platforms;

    projectEditors.Add(projectEditor);
  end;

  // 2. Handle upgrades - remove existing package references
  for projectEditor in projectEditors do
  begin
    projectPackageGraph := projectEditor.GetPackageReferences;
    existingPackageRef := projectPackageGraph.FindTopLevelChild(Options.packageId);

    if existingPackageRef <> nil then
    begin
      if not (Options.force or Options.IsUpgrade) then
      begin
        FLogger.Error('Package already installed. Use -force or -upgrade');
        exit;
      end;
      projectPackageGraph.RemoveTopLevelChild(existingPackageRef.Id);
      FContext.RemoveResolution(Options.PackageId);
    end;

    // Remove any transitive dependencies being promoted
    dependency := projectPackageGraph.FindFirstChild(Options.packageId);
    while dependency <> nil do
    begin
      projectPackageGraph.RemoveChild(dependency);
      dependency := projectPackageGraph.FindFirstChild(Options.packageId);
    end;
  end;

  // 3. Install to each project
  for projectEditor in projectEditors do
    result := InstallPackage(cancellationToken, Options, projectEditor, config, context) and result;
end;
```

### 7.3 InstallPackage (Per-Project)

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:1227-1286`

```pascal
function TPackageInstaller.InstallPackage(...): boolean;
begin
  // 1. Validate and set compiler/platforms
  ValidateAndSetCompilerPlatforms(Options, projectEditor, platforms, shouldSkip);
  if shouldSkip then exit;

  // 2. Install for each platform
  for platform in platforms do
  begin
    platformOptions := Options.Clone;
    platformOptions.platforms := [platform];

    FLogger.Information('Installing [' + platformOptions.SearchTerms + '-' +
                        DPMPlatformToString(platform) + '] into [' + projectEditor.ProjectFile + ']');

    platformResult := DoInstallPackageForPlatform(
      cancellationToken, platformOptions, projectEditor.projectFile,
      projectEditor, platform, config, context, platformGraph);

    if platformResult then
    begin
      if finalGraph = nil then
        finalGraph := platformGraph;
    end;
  end;

  // 3. Update project file once with final graph
  if result and (finalGraph <> nil) then
  begin
    projectEditor.UpdatePackageReferences(finalGraph);
    result := projectEditor.SaveProject();
  end;
end;
```

### 7.4 DoInstallPackageForPlatform (Core Logic)

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:668-813`

This is the main workhorse method:

```pascal
function TPackageInstaller.DoInstallPackageForPlatform(...): boolean;
begin
  // 1. Get existing package graph from project (or create root)
  projectPackageGraph := projectEditor.GetPackageReferences;
  if projectPackageGraph = nil then
    projectPackageGraph := TPackageReference.CreateRoot(Options.compilerVersion);

  // 2. Check if already installed
  existingPackageRef := projectPackageGraph.FindTopLevelChild(Options.packageId);
  if (existingPackageRef <> nil) and (not Options.force) and (not Options.IsUpgrade) then
  begin
    FLogger.Error('Package already installed');
    exit;
  end;

  // 3. Get package info (version specified or latest)
  if not Options.Version.IsEmpty then
  begin
    newPackageIdentity := TPackageIdentity.Create(options.Sources, Options.packageId,
                                                   Options.Version, Options.compilerVersion);
    packageInfo := GetPackageInfo(cancellationToken, newPackageIdentity);
  end
  else
  begin
    packageInfo := FRepositoryManager.FindLatestVersion(
      cancellationToken, options.PackageId, options.CompilerVersion,
      TPackageVersion.Empty, Options.PreRelease, options.Sources);
  end;

  // 4. Ensure package is in cache (download if needed)
  if not FPackageCache.EnsurePackage(packageInfo) then
  begin
    if not FRepositoryManager.DownloadPackage(
      cancellationToken, packageInfo, FPackageCache.PackagesFolder, packageFileName) then
    begin
      FLogger.Error('Failed to download package');
      exit;
    end;
    FPackageCache.InstallPackageFromFile(packageFileName);
  end;

  // 5. Flatten existing graph to project references
  projectReferences := TCollections.CreateList<IPackageReference>;
  CreateProjectRefs(cancellationToken, projectPackageGraph, projectReferences);

  // 6. Resolve dependencies
  if not FDependencyResolver.ResolveForInstall(
    cancellationToken, Options.CompilerVersion, projectFile, Options,
    packageInfo, projectReferences, projectPackageGraph, resolvedPackages) then
  begin
    FLogger.Debug('ResolveForInstall failed');
    exit;
  end;

  // 7. Record graph for multi-project conflict detection
  FContext.RecordGraph(projectFile, projectPackageGraph);

  // 8. Download all resolved packages
  packageManifests := TCollections.CreateDictionary<string, IPackageSpec>;
  if not DownloadPackages(cancellationToken, resolvedPackages, packageManifests) then
    exit;

  // 9. Configure package for this platform
  if not DoConfigurePackageForPlatform(
    cancellationToken, Options, projectFile, projectEditor, platform,
    config, projectPackageGraph, resolvedPackages, packageManifests) then
    exit;

  resultGraph := projectPackageGraph;
  result := true;
end;
```

---

## 8. Dependency Resolution

### 8.1 TDependencyResolver Class

**File:** `Source/Core/Dependency/DPM.Core.Dependency.Resolver.pas`

The resolver uses a **depth-first search with backtracking** algorithm.

### 8.2 Algorithm Overview

From the source comments (lines 102-121):

> This is a simple depth first search with backtracking. It records unresolvable paths (nogoods) to avoid searching those again. When a conflict is found, it tries to resolve that by finding an overlapping dependency version range between the new dependency range and the one already resolved.

Key optimizations:
- Dependencies are sorted by version range width (smaller = less choices = fail faster)
- "No good" paths are recorded to avoid re-exploration
- Version ranges can be intersected to find compatible versions

### 8.3 ResolveForInstall

**File:** `Source/Core/Dependency/DPM.Core.Dependency.Resolver.pas:319-351`

```pascal
function TDependencyResolver.ResolveForInstall(...): boolean;
begin
  // 1. Check for conflicts with already-loaded projects
  for packageRef in projectReferences do
  begin
    resolution := FPackageInstallerContext.FindPackageResolution(projectFile, packageRef.Id);
    if (resolution <> nil) and (not resolution.VersionRange.IsSatisfiedBy(packageRef.Version)) then
    begin
      FLogger.Error('Package project group conflict');
      Inc(errorCount);
    end;
  end;

  // 2. Create resolver context
  context := TResolverContext.Create(FLogger, FPackageInstallerContext,
                                      projectFile, newPackage, projectReferences);

  // 3. Run the resolution algorithm
  result := DoResolve(cancellationToken, compilerVersion, options.Prerelease, context);

  // 4. Extract results
  resolved := context.GetResolvedPackageInfos;
  dependencyGraph := context.BuildDependencyGraph;

  // 5. Record resolutions for other projects in the group
  FPackageInstallerContext.RecordResolutions(projectFile, context.GetResolvedPackages);
end;
```

### 8.4 DoResolve Algorithm

**File:** `Source/Core/Dependency/DPM.Core.Dependency.Resolver.pas:123-310`

```pascal
function TDependencyResolver.DoResolve(...): boolean;
begin
  FLogger.Verbose('Starting dependency resolution...');

  // Process all open requirements
  while context.AnyOpenRequrements do
  begin
    currentPackage := context.PopRequirement;

    // Skip if no dependencies
    if not currentPackage.Dependencies.Any then
      continue;

    // Sort dependencies by version range width (smaller = fail faster)
    currentPackage.Dependencies.Sort(SortDependencies);

    for dependency in currentPackage.Dependencies do
    begin
      FLogger.Information('Resolving: ' + currentPackage.Id + '->' + dependency.Id);

      // Check if already resolved
      if context.TryGetResolvedPackage(dependency.Id, currentPackage.Id, resolution) then
      begin
        // Check if compatible
        if not dependency.VersionRange.IsSatisfiedBy(resolution.PackageInfo.Version) then
        begin
          // Try to find intersecting range
          if resolution.VersionRange.TryGetIntersectingRange(
               dependency.VersionRange, intersectingRange) then
          begin
            dependency.VersionRange := intersectingRange;
          end
          else
          begin
            // Record as no-good and backtrack
            context.RecordNoGood(resolution.PackageInfo);
            context.RemoveResolvedPackage(dependency.Id);
            context.PushRequirement(currentPackage);
          end;
        end;
        continue;
      end;

      // Get available versions
      versions := context.GetPackageVersions(dependency.Id);
      if versions = nil then
      begin
        versions := FRepositoryManager.GetPackageVersionsWithDependencies(
          cancellationToken, compilerVersion, dependency.Id,
          dependency.VersionRange, preRelease);
        context.CachePackageVersions(dependency.Id, versions);
      end;

      // Find a satisfying version
      selected := false;
      for version in versions do
      begin
        if context.IsNoGood(version) then continue;

        if dependency.VersionRange.IsSatisfiedBy(version.Version) then
        begin
          context.RecordResolution(version, dependency.VersionRange, currentPackage.Id);
          if version.Dependencies.Any then
            context.PushRequirement(version);
          selected := true;
          break;
        end
        else
          context.RecordNoGood(version);
      end;

      if not selected then
      begin
        // Backtrack
        context.RecordNoGood(currentPackage);
        // ... backtracking logic
      end;
    end;
  end;

  result := true;
  FLogger.Success('Dependency resolution done in [' + elapsed + 'ms]');
end;
```

---

## 9. Package Cache Management

### 9.1 TPackageCache Class

**File:** `Source/Core/Cache/DPM.Core.Cache.pas`

The cache stores packages in a structured hierarchy:

```
{CacheLocation}/
  {CompilerVersion}/
    {PackageId}/
      {Version}/
        package.dspec         (manifest)
        src/                  (source files)
        lib/{Platform}/       (compiled units)
        bpl/{Platform}/       (compiled BPL files)
```

### 9.2 GetPackagePath

**File:** `Source/Core/Cache/DPM.Core.Cache.pas:161-164`

```pascal
function TPackageCache.GetPackagePath(const packageId : IPackageIdentity) : string;
begin
  result := GetPackagesFolder + PathDelim +
            CompilerToString(packageId.CompilerVersion) + PathDelim +
            packageId.Id + PathDelim +
            packageId.Version.ToStringNoMeta;
end;
```

### 9.3 EnsurePackage

**File:** `Source/Core/Cache/DPM.Core.Cache.pas:209-232`

```pascal
function TPackageCache.EnsurePackage(const packageId : IPackageIdentity) : Boolean;
begin
  // Check if folder and manifest exist
  packageFileName := GetPackagePath(packageId);
  result := DirectoryExists(packageFileName);

  manifestFile := packageFileName + PathDelim + 'package.dspec';
  result := result and (FileExists(manifestFile) or FileExists(oldManifestFile));

  // If not found, try to reinstall from .dpkg file if available
  if not result then
  begin
    packageFileName := GetPackagesFolder + PathDelim + packageId.ToString + '.dpkg';
    if FileExists(packageFileName) then
      result := InstallPackageFromFile(packageFileName);
  end;
end;
```

### 9.4 InstallPackageFromFile

**File:** `Source/Core/Cache/DPM.Core.Cache.pas:235-310`

```pascal
function TPackageCache.InstallPackageFromFile(const packageFileName : string) : boolean;
begin
  // 1. Validate file exists and is a .dpkg
  if not FileExists(packageFileName) then exit(false);
  if ExtractFileExt(packageFileName) <> '.dpkg' then exit(false);

  // 2. Parse package identity from filename
  fileName := ChangeFileExt(ExtractFileName(packageFileName), '');
  TPackageIdentity.TryCreateFromString(FLogger, fileName, '', packageIndentity);

  // 3. Create package folder
  packageFolder := CreatePackagePath(packageIndentity);

  // 4. Copy to cache if not already there
  if not TStringUtils.StartsWith(packageFileName, GetPackagesFolder) then
  begin
    packageFilePath := GetPackagesFolder + PathDelim + ExtractFileName(packageFileName);
    TFile.Copy(packageFileName, packageFilePath, true);
  end;

  // 5. Extract ZIP contents
  TZipFile.ExtractZipFile(packageFilePath, packageFolder);

  // 6. Verify manifest exists
  result := FileExists(packageFolder + PathDelim + 'package.dspec');
end;
```

---

## 10. Package Configuration and Compilation

### 10.1 DoConfigurePackageForPlatform

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:1071-1113`

```pascal
function TPackageInstaller.DoConfigurePackageForPlatform(...): boolean;
begin
  // 1. Check platform compatibility
  if not (platform in GetEffectivePlatforms([platform], packageManifests)) then
  begin
    FLogger.Information('Platform skipped - not supported by packages');
    exit(true);
  end;

  // 2. Setup build lists
  compiledPackages := TCollections.CreateList<IPackageInfo>;
  packagesToCompile := TCollections.CreateList<IPackageInfo>(resolvedPackages);
  packageSearchPaths := TCollections.CreateList<string>;
  packageCompiler := FCompilerFactory.CreateCompiler(options.compilerVersion, platform);

  // 3. Build dependencies (compile packages with build entries)
  if not BuildDependencies(cancellationToken, packageCompiler, projectPackageGraph,
                           packagesToCompile, compiledPackages, packageManifests, options) then
    exit;

  // 4. Collect search paths from all packages
  if not CollectSearchPaths(projectPackageGraph, resolvedPackages, compiledPackages,
                            projectEditor.compilerVersion, platform, packageSearchPaths) then
    exit;

  // 5. Copy runtime BPLs to output (currently commented out)
  if not CopyLocal(cancellationToken, resolvedPackages, packageManifests, projectEditor, platform) then
    exit;

  // 6. Install design-time packages (no-op in CLI, implemented in IDE)
  if not InstallDesignPackages(cancellationToken, projectFile, platform, packageManifests) then
    exit;

  // 7. Add search paths to project file
  if not projectEditor.AddSearchPaths(platform, packageSearchPaths, config.PackageCacheLocation) then
    exit;

  result := true;
end;
```

### 10.2 BuildDependencies

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:921-1003`

Uses depth-first traversal to build packages in correct order:

```pascal
function TPackageInstaller.BuildDependencies(...): boolean;
begin
  // Visit each package in DFS order
  projectPackageGraph.VisitDFS(
    procedure(const packageReference: IPackageReference)
    var
      pkgInfo: IPackageInfo;
      Spec: IPackageSpec;
      template: ISpecTemplate;
    begin
      // Find package in compile list
      pkgInfo := packagesToCompile.FirstOrDefault(...);
      if pkgInfo = nil then exit; // Already processed

      packagesToCompile.Remove(pkgInfo);

      // Get spec and template
      Spec := packageManifests[LowerCase(packageReference.Id)];
      template := Spec.FindTemplate(Spec.TargetPlatform.TemplateName);

      // Check if needs compilation
      if template.BuildEntries.Any or template.DesignEntries.Any then
      begin
        forceCompile := Options.force and SameText(pkgInfo.Id, Options.SearchTerms);

        if not CompilePackage(cancellationToken, packageCompiler, pkgInfo,
                              packageReference, Spec, forceCompile, false) then
          raise Exception.Create('Compilation failed');

        compiledPackages.Add(pkgInfo);
      end;
    end);

  result := true;
end;
```

### 10.3 CompilePackage

**File:** `Source/Core/Package/DPM.Core.Package.Installer.pas:282-423`

```pascal
function TPackageInstaller.CompilePackage(...): boolean;
begin
  packagePath := FPackageCache.GetPackagePath(packageInfo);
  bomFile := TPath.Combine(packagePath, 'package.bom');

  // 1. BOM Optimization: Skip if dependencies unchanged
  if (not force) and FileExists(bomFile) then
  begin
    bomNode := TBOMFile.LoadFromFile(FLogger, bomFile);
    if (bomNode <> nil) and bomNode.AreEqual(packageReference) then
    begin
      FLogger.Information('Dependencies unchanged, skipping compilation');
      exit(true);
    end;
  end;
  DeleteFile(bomFile);

  // 2. Get template for this package
  template := packageSpec.FindTemplate(packageSpec.TargetPlatform.TemplateName);
  if (template = nil) or (not template.BuildEntries.Any) then
    exit(true);

  // 3. Setup compiler output directories
  Compiler.BPLOutputDir := packagePath + '\bpl\' + DPMPlatformToBDString(Compiler.Platform);
  Compiler.LibOutputDir := packagePath + '\lib\' + DPMPlatformToBDString(Compiler.Platform);
  Compiler.Configuration := 'Release';

  // 4. Set search paths from dependencies
  if packageReference.HasChildren then
  begin
    searchPaths := TCollections.CreateList<string>;
    for dependency in packageReference.Children do
    begin
      childSearchPath := FPackageCache.GetPackagePath(dependency.Id,
                           dependency.Version.ToStringNoMeta,
                           Compiler.compilerVersion, Compiler.platform);
      childSearchPath := childSearchPath + '\lib\' + DPMPlatformToBDString(Compiler.Platform);
      searchPaths.Add(childSearchPath);
    end;
    Compiler.SetSearchPaths(searchPaths);
  end;

  // 5. Compile build entries
  for buildEntry in template.BuildEntries do
  begin
    if (buildEntry.Platforms <> []) and not (Compiler.Platform in buildEntry.Platforms) then
      continue;

    projectFile := TPath.Combine(packagePath, buildEntry.Project);
    result := Compiler.BuildProject(cancellationToken, Compiler.Platform,
                                     projectFile, 'Release', packageInfo.Version, false);
    if not result then exit;
  end;

  // 6. Compile design entries (match IDE bitness)
  for designEntry in template.DesignEntries do
  begin
    if (Compiler.compilerVersion >= TCompilerVersion.Delphi13) and
       (Compiler.Platform = TDPMPlatform.Win64) then
      designPlatform := TDPMPlatform.Win64
    else
      designPlatform := TDPMPlatform.Win32;

    projectFile := TPath.Combine(packagePath, designEntry.Project);
    result := Compiler.BuildProject(cancellationToken, designPlatform,
                                     projectFile, 'Release', packageInfo.Version, true);
    if not result then exit;
  end;

  // 7. Save BOM for future optimization
  TBOMFile.SaveToFile(FLogger, bomFile, packageReference);
end;
```

---

## 11. Project File Modification

### 11.1 TProjectEditor Class

**File:** `Source/Core/Project/DPM.Core.Project.Editor.pas`

Handles all `.dproj` file manipulation using MSXML DOM.

### 11.2 LoadProject

**File:** `Source/Core/Project/DPM.Core.Project.Editor.pas:755-789`

```pascal
function TProjectEditor.LoadProject(const filename : string;
                                     const elements : TProjectElements) : Boolean;
begin
  FProjectXML := CoDOMDocument60.Create;
  result := FProjectXML.load(fileName);

  // Configure XPath with namespace
  (FProjectXML as IXMLDOMDocument2).setProperty('SelectionLanguage', 'XPath');
  (FProjectXML as IXMLDOMDocument2).setProperty('SelectionNamespaces',
    'xmlns:x=''http://schemas.microsoft.com/developer/msbuild/2003''');

  // Load requested elements
  result := InternalLoadFromXML(elements);
end;
```

### 11.3 Loading Project Elements

**File:** `Source/Core/Project/DPM.Core.Project.Editor.pas:374-394`

```pascal
function TProjectEditor.InternalLoadFromXML(const elements : TProjectElements) : boolean;
begin
  result := true;
  loadAll := TProjectElement.All in elements;

  if loadAll or (TProjectElement.DPMCompiler in elements) then
    result := result and LoadDPMCompilerVersion;
  if loadAll or (TProjectElement.ProjectVersion in elements) then
    result := result and LoadProjectVersion;
  if loadAll or (TProjectElement.MainSource in elements) then
    result := result and LoadMainSource;
  if loadAll or (TProjectElement.AppType in elements) then
    result := result and LoadAppType;
  if loadAll or (TProjectElement.Platforms in elements) then
    result := result and LoadProjectPlatforms;
  if loadAll or (TProjectElement.Configs in elements) then
    result := result and LoadConfigurations;
  if loadAll or (TProjectElement.PackageRefs in elements) then
    result := result and LoadPackageRefences;
end;
```

### 11.4 LoadPackageReferences

**File:** `Source/Core/Project/DPM.Core.Project.Editor.pas:566-753`

Supports three formats (in priority order):

1. **New format:** `/Project/ProjectExtensions/DPM/Packages`
2. **Previous format:** `/Project/ProjectExtensions/DPM/PackageReferences[@platform]`
3. **Legacy format:** `/Project/ProjectExtensions/DPM/PackageReference`

```pascal
function TProjectEditor.LoadPackageRefences : boolean;
begin
  // Try new format first
  packagesElement := FProjectXML.selectSingleNode(packagesXPath) as IXMLDOMElement;
  if packagesElement <> nil then
  begin
    ReadPackageReferences(nil, packagesElement, TDPMPlatform.Win32);
    exit;
  end;

  // Try previous format (per-platform)
  packageReferencesElements := FProjectXML.selectNodes(packageReferencesXPath);
  if packageReferencesElements.length > 0 then
  begin
    // Read from first platform element (packages same for all platforms now)
    ReadPackageReferences(nil, packageReferencesElements.item[0], platform);
    exit;
  end;

  // Try legacy format
  ReadPackageReferences(nil, FProjectXML.documentElement, TDPMPlatform.UnknownPlatform);
end;
```

### 11.5 AddSearchPaths

**File:** `Source/Core/Project/DPM.Core.Project.Editor.pas:162-203`

```pascal
function TProjectEditor.AddSearchPaths(const platform : TDPMPlatform;
                                        const searchPaths : IList<string>;
                                        const packageCacheLocation : string) : boolean;
begin
  // 1. Get or create DPM property group
  dpmGroup := GetDPMPropertyGroup;

  // 2. Ensure base search path is configured
  EnsureBaseSearchPath;

  // 3. Create platform-specific search path element
  condition := '''$(Platform)''==''' + DPMPlatformToBDString(platform) + '''';
  dpmSearchElement := dpmGroup.selectSingleNode('x:DPMSearch[@Condition = "' + condition + '"]');

  if dpmSearchElement = nil then
  begin
    dpmSearchElement := FProjectXML.createNode(NODE_ELEMENT, 'DPMSearch', msbuildNamespace);
    dpmGroup.appendChild(dpmSearchElement);
  end;
  dpmSearchElement.setAttribute('Condition', condition);

  // 4. Build search path string
  for searchPath in searchPaths do
    searchPathString := searchPathString + '$(DPM)\' + searchPath + ';';

  dpmSearchElement.text := searchPathString;
end;
```

### 11.6 GetDPMPropertyGroup

**File:** `Source/Core/Project/DPM.Core.Project.Editor.pas:279-337`

Creates/updates the DPM property group with:

```xml
<PropertyGroup>
  <DPMCompiler>Delphi11</DPMCompiler>
  <DPMCache Condition="'$(DPMCache)' == ''">$(APPDATA)\.dpm\packages</DPMCache>
  <DPM>$(DPMCache)\$(DPMCompiler)\$(Platform)</DPM>
</PropertyGroup>
```

### 11.7 EnsureBaseSearchPath

**File:** `Source/Core/Project/DPM.Core.Project.Editor.pas:246-277`

Ensures the base config has:

```xml
<DCC_UnitSearchPath>$(DPMSearch);$(DCC_UnitSearchPath)</DCC_UnitSearchPath>
```

### 11.8 UpdatePackageReferences (New Format)

**File:** `Source/Core/Project/DPM.Core.Project.Editor.pas:1050-1125`

```pascal
procedure TProjectEditor.UpdatePackageReferences(const dependencyGraph: IPackageReference);
begin
  // Find or create ProjectExtensions/DPM/Packages element
  dpmElement := projectExtensionsElement.selectSingleNode('x:DPM');
  if dpmElement = nil then
  begin
    dpmElement := FProjectXML.createNode(NODE_ELEMENT, 'DPM', msbuildNamespace);
    projectExtensionsElement.appendChild(dpmElement);
  end
  else
  begin
    // Remove old formats
    oldPackageRefsElements := dpmElement.selectNodes('x:PackageReferences');
    for i := oldPackageRefsElements.length - 1 downto 0 do
      dpmElement.removeChild(oldPackageRefsElements.item[i]);
  end;

  // Find or create Packages element
  packagesElement := dpmElement.selectSingleNode('x:Packages');
  if packagesElement = nil then
  begin
    packagesElement := FProjectXML.createNode(NODE_ELEMENT, 'Packages', msbuildNamespace);
    dpmElement.appendChild(packagesElement);
  end;

  // Write all top-level package references recursively
  for topLevelReference in dependencyGraph.Children do
    WritePackageRef(packagesElement, topLevelReference);
end;
```

The output format:

```xml
<ProjectExtensions>
  <DPM>
    <Packages>
      <PackageReference id="Spring4D" version="4.1.2">
        <PackageReference id="TransitiveDep" version="1.0.0" range="[1.0.0,2.0.0)" />
      </PackageReference>
    </Packages>
  </DPM>
</ProjectExtensions>
```

---

## 12. Multi-Project Coordination

### 12.1 IPackageInstallerContext

**File:** `Source/Core/Package/DPM.Core.Package.InstallerContext.pas`

The context tracks package resolutions across multiple projects in a project group to detect and prevent conflicts.

### 12.2 Key Methods

#### RecordGraph
```pascal
procedure TCorePackageInstallerContext.RecordGraph(
  const projectFile: string;
  const graph: IPackageReference);
begin
  FProjectGraphs[LowerCase(projectFile)] := graph;
end;
```

#### RecordResolutions
```pascal
procedure TCorePackageInstallerContext.RecordResolutions(
  const projectFile: string;
  const resolutions: TArray<IResolvedPackage>);
begin
  FProjectResolutions[LowerCase(projectFile)] :=
    TCollections.CreateList<IResolvedPackage>(resolutions);
end;
```

#### FindPackageResolution
```pascal
function TCorePackageInstallerContext.FindPackageResolution(
  const projectFile : string;
  const packageId: string): IResolvedPackage;
begin
  result := nil;
  for pair in FProjectResolutions do
  begin
    // Only check OTHER projects
    if not SameText(projectFile, pair.Key) then
    begin
      result := pair.Value.FirstOrDefault(
        function (const resolution : IResolvedPackage) : boolean
        begin
          result := SameText(packageId, resolution.PackageInfo.Id);
        end);
      if result <> nil then exit;
    end;
  end;
end;
```

### 12.3 Conflict Detection

When resolving dependencies, the resolver checks if a package was already resolved in another project:

```pascal
resolution := FPackageInstallerContext.FindPackageResolution(projectFile, packageRef.Id);
if (resolution <> nil) and
   (not resolution.VersionRange.IsSatisfiedBy(packageRef.Version)) then
begin
  FLogger.Error('Package project group conflict');
end;
```

---

## 13. Complete Flow Diagram

```
User: dpm install Spring4D --compiler=Delphi11

┌─ TDPMConsoleApplication.Run()
│  ├─ Parse command line arguments
│  ├─ Resolve TInstallCommand from DI container
│  └─ Call commandHandler.Execute()

└─ TInstallCommand.Execute()
   ├─ Apply common options
   ├─ Set default project path if empty
   ├─ Validate TInstallOptions
   └─ Call FPackageInstaller.Install()

   └─ TPackageInstaller.Install()
      ├─ Init(options)
      │  ├─ Validate options
      │  ├─ Load configuration
      │  ├─ Set cache location
      │  ├─ Initialize dependency resolver
      │  └─ Verify package sources exist
      │
      ├─ ResolveProjectFiles(projectPath)
      │  ├─ Handle explicit project list
      │  ├─ Handle single .dproj
      │  ├─ Handle .groupproj (parse and extract)
      │  └─ Handle directory (find all *.dproj)
      │
      ├─ If PackageFile specified → InstallPackageFromFile()
      │  ├─ Install .dpkg to cache
      │  ├─ Parse identity from filename
      │  └─ Route to InstallPackageFromId()
      │
      └─ InstallPackageFromId()
         ├─ Load all project files into editors
         ├─ Validate compiler versions match
         ├─ Calculate effective platforms
         ├─ Handle upgrade: remove existing references
         │
         └─ For each project:
            └─ InstallPackage()
               ├─ ValidateAndSetCompilerPlatforms()
               │
               └─ For each platform:
                  └─ DoInstallPackageForPlatform()
                     ├─ Get existing package graph
                     ├─ Check if already installed
                     ├─ Get package info (version or latest)
                     ├─ EnsurePackage in cache
                     │  └─ DownloadPackage() if needed
                     │
                     ├─ CreateProjectRefs() - flatten graph
                     │
                     ├─ DependencyResolver.ResolveForInstall()
                     │  └─ DoResolve()
                     │     ├─ Pop requirements from stack
                     │     ├─ Sort dependencies by range width
                     │     ├─ For each dependency:
                     │     │  ├─ Check if already resolved
                     │     │  ├─ Try to find intersecting range
                     │     │  ├─ Get available versions
                     │     │  ├─ Select satisfying version
                     │     │  └─ Backtrack if blocked
                     │     └─ Return resolved packages
                     │
                     ├─ RecordGraph() - for conflict detection
                     │
                     ├─ DownloadPackages()
                     │  ├─ EnsurePackage for each
                     │  └─ Load manifests (.dspec files)
                     │
                     └─ DoConfigurePackageForPlatform()
                        ├─ Check platform support
                        │
                        ├─ BuildDependencies()
                        │  └─ VisitDFS: for each package
                        │     └─ CompilePackage()
                        │        ├─ Check BOM for optimization
                        │        ├─ Setup compiler paths
                        │        ├─ Compile build entries
                        │        ├─ Compile design entries
                        │        └─ Save BOM
                        │
                        ├─ CollectSearchPaths()
                        │
                        ├─ CopyLocal() - runtime BPLs
                        │
                        ├─ InstallDesignPackages() - IDE only
                        │
                        └─ projectEditor.AddSearchPaths()
                           ├─ GetDPMPropertyGroup()
                           ├─ EnsureBaseSearchPath()
                           └─ Create platform DPMSearch element

               └─ After all platforms:
                  ├─ projectEditor.UpdatePackageReferences()
                  │  └─ Write to ProjectExtensions/DPM/Packages
                  └─ projectEditor.SaveProject()
```

---

## 14. Key Files Reference

| File | Lines | Purpose |
|------|-------|---------|
| `DPM.Console.Command.Install.pas` | 93 | CLI entry point |
| `DPM.Console.Application.pas` | ~300 | Main app, command routing |
| `DPM.Console.Reg.pas` | ~200 | DI container registration |
| `DPM.Core.Package.Installer.pas` | 1772 | Core installation logic |
| `DPM.Core.Package.Installer.Interfaces.pas` | ~100 | IPackageInstaller interface |
| `DPM.Core.Package.InstallerContext.pas` | 202 | Multi-project coordination |
| `DPM.Core.Options.Install.pas` | 180 | Install options |
| `DPM.Core.Project.Editor.pas` | 1129 | .dproj manipulation |
| `DPM.Core.Cache.pas` | 319 | Package cache management |
| `DPM.Core.Dependency.Resolver.pas` | 427 | Dependency resolution |
| `DPM.Core.Dependency.Context.pas` | ~400 | Resolution context |

---

## 15. Design Patterns Used

### 15.1 Dependency Injection
All major components receive dependencies through constructor injection, wired by Spring4D container.

### 15.2 Factory Pattern
`ICommandFactory` creates command handlers by name from the DI container.

### 15.3 Strategy Pattern
Different repository types (HTTP, directory) implement `IPackageRepository`.

### 15.4 Visitor Pattern
Package graph traversal uses `VisitDFS()` with anonymous method callbacks.

### 15.5 Template Method
`TBaseCommand` defines the command execution template; subclasses implement `Execute()`.

### 15.6 Singleton Pattern
`TInstallOptions.Default` provides a global options instance for command-line parsing.

### 15.7 Context Object Pattern
`IPackageInstallerContext` and `IResolverContext` carry state across operations.

---

## Notes on Current Implementation

1. **Single Package Per Compiler:** The codebase is transitioning to a model where packages are stored once per compiler version, not per compiler+platform (WIP per code comments).

2. **BOM Optimization:** The "Bill of Materials" file (`package.bom`) caches the dependency state to avoid unnecessary recompilation.

3. **Design Package Bitness:** Design packages must match the IDE bitness:
   - Delphi 12 and earlier: Win32 only (32-bit IDE)
   - Delphi 13 and later: Win64 option available

4. **CopyLocal:** The runtime package copying feature is currently commented out in the code.

5. **No VCL in Core:** The `Source/Core/` modules intentionally avoid VCL/GDI dependencies to enable CLI operation in Docker containers.
