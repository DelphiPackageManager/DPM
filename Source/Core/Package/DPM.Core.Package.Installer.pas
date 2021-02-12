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

unit DPM.Core.Package.Installer;

//TODO : Lots of common code between install and restore - refactor!

interface

uses
  VSoft.Awaitable,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Search,
  DPM.Core.Options.Install,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore,
  DPM.Core.Project.Interfaces,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Compiler.Interfaces;

type
  TPackageInstaller = class(TInterfacedObject, IPackageInstaller)
  private
    FLogger : ILogger;
    FConfigurationManager : IConfigurationManager;
    FRepositoryManager : IPackageRepositoryManager;
    FPackageCache : IPackageCache;
    FDependencyResolver : IDependencyResolver;
    FContext : IPackageInstallerContext;
    FCompilerFactory : ICompilerFactory;
  protected
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
    function CreateProjectRefs(const cancellationToken: ICancellationToken; const node: IGraphNode; const seenPackages: IDictionary<string, IPackageInfo>;
                               const projectReferences: IList<TProjectReference>): boolean;

    function CollectSearchPaths(const options : TSearchOptions; const resolvedPackages : IList<IPackageInfo>; const compiledPackages : IList<IPackageInfo>; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const searchPaths : IList<string> ) : boolean;

    procedure GenerateSearchPaths(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; packageSpec : IPackageSpec; const searchPaths : IList<string>);

    function DownloadPackages(const cancellationToken : ICancellationToken; const resolvedPackages : IList<IPackageInfo>; var packageSpecs : IDictionary<string, IPackageSpec>) : boolean;

    function CollectPlatformsFromProjectFiles(const options : TInstallOptions; const projectFiles : TArray <string> ; const config : IConfiguration) : boolean;

    function GetCompilerVersionFromProjectFiles(const options : TInstallOptions; const projectFiles : TArray <string> ; const config : IConfiguration) : boolean;


    function CompilePackage(const cancellationToken : ICancellationToken; const compiler : ICompiler; const packageInfo : IPackageInfo; const graphNode : IGraphNode; const packageSpec : IPackageSpec; const force : boolean) : boolean;

    function BuildDependencies(const cancellationToken : ICancellationToken; const packageCompiler : ICompiler; const projectPackageGraph : IGraphNode;
                               const packagesToCompile : IList<IPackageInfo>; const compiledPackages : IList<IPackageInfo>; packageSpecs : IDictionary<string, IPackageSpec>;
                               const options : TSearchOptions) : boolean;


    function DoRestoreProject(const cancellationToken : ICancellationToken; const options : TRestoreOptions; const projectFile : string; const projectEditor : IProjectEditor; const platform : TDPMPlatform; const config : IConfiguration) : boolean;

    function DoInstallPackage(const cancellationToken : ICancellationToken; const options : TInstallOptions; const projectFile : string; const projectEditor : IProjectEditor; const platform : TDPMPlatform; const config : IConfiguration) : boolean;

    function DoUninstallFromProject(const cancellationToken : ICancellationToken; const options : TUnInstallOptions; const projectFile : string; const projectEditor : IProjectEditor; const platform : TDPMPlatform; const config : IConfiguration) : boolean;


    function DoCachePackage(const cancellationToken : ICancellationToken; const options : TCacheOptions; const platform : TDPMPlatform) : boolean;

    //works out what compiler/platform then calls DoInstallPackage
    function InstallPackage(const cancellationToken : ICancellationToken; const options : TInstallOptions; const projectFile : string; const config : IConfiguration) : boolean;

    //user specified a package file - will install for single compiler/platform - calls InstallPackage
    function InstallPackageFromFile(const cancellationToken : ICancellationToken; const options : TInstallOptions; const projectFiles : TArray <string> ; const config : IConfiguration) : boolean;

    //resolves package from id - calls InstallPackage
    function InstallPackageFromId(const cancellationToken : ICancellationToken; const options : TInstallOptions; const projectFiles : TArray <string> ; const config : IConfiguration) : boolean;

    //calls either InstallPackageFromId or InstallPackageFromFile depending on options.
    function Install(const cancellationToken : ICancellationToken; const options : TInstallOptions) : Boolean;

    function UnInstall(const cancellationToken : ICancellationToken; const options : TUnInstallOptions) : boolean;

    function UnInstallFromProject(const cancellationToken : ICancellationToken; const options : TUnInstallOptions; const projectFile : string; const config : IConfiguration) : Boolean;


    function RestoreProject(const cancellationToken : ICancellationToken; const options : TRestoreOptions; const projectFile : string; const config : IConfiguration) : Boolean;

    //calls restore project
    function Restore(const cancellationToken : ICancellationToken; const options : TRestoreOptions) : Boolean;

    function Remove(const cancellationToken : ICancellationToken; const options : TUninstallOptions) : boolean;


    function Cache(const cancellationToken : ICancellationToken; const options : TCacheOptions) : boolean;

    function Context : IPackageInstallerContext;

  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager;
      const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache;
      const dependencyResolver : IDependencyResolver; const context : IPackageInstallerContext;
      const compilerFactory : ICompilerFactory);
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
  DPM.Core.Utils.System,
  DPM.Core.Project.Editor,
  DPM.Core.Project.GroupProjReader,
  DPM.Core.Options.List,
  DPM.Core.Dependency.Graph,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Metadata,
  DPM.Core.Spec.Reader;


{ TPackageInstaller }


function TPackageInstaller.Cache(const cancellationToken : ICancellationToken; const options : TCacheOptions) : boolean;
var
  config : IConfiguration;
  platform : TDPMPlatform;
  platforms : TDPMPlatforms;
begin
  result := false;
  if (not options.Validated) and (not options.Validate(FLogger)) then
    exit
  else if not options.IsValid then
    exit;

  config := FConfigurationManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;

  FPackageCache.Location := config.PackageCacheLocation;
  if not FRepositoryManager.Initialize(config) then
  begin
    FLogger.Error('Unable to initialize the repository manager.');
    exit;
  end;

  platforms := options.Platforms;

  if platforms = [] then
    platforms := AllPlatforms(options.CompilerVersion);

  result := true;
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit;
    options.Platforms := [platform];
    result := DoCachePackage(cancellationToken, options, platform) and result;
  end;
end;

function TPackageInstaller.CollectPlatformsFromProjectFiles(const options : TInstallOptions; const projectFiles : TArray <string> ; const config : IConfiguration) : boolean;
var
  projectFile : string;
  projectEditor : IProjectEditor;
begin
  result := true;
  for projectFile in projectFiles do
  begin
    projectEditor := TProjectEditor.Create(FLogger, config, options.CompilerVersion);
    result := result and projectEditor.LoadProject(projectFile);
    if result then
      options.Platforms := options.Platforms + projectEditor.Platforms;
  end;

end;

function TPackageInstaller.CollectSearchPaths(const options : TSearchOptions; const resolvedPackages : IList<IPackageInfo>; const compiledPackages : IList<IPackageInfo>; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const searchPaths : IList<string> ) : boolean;
var
  packageInfo : IPackageInfo;
  packageMetadata : IPackageMetadata;
  packageSearchPath : IPackageSearchPath;
  packageBasePath : string;
begin
  result := true;

  //reverse the list so that we add the paths in reverse order, small optimisation for the compiler.
  resolvedPackages.Reverse;
  for packageInfo in resolvedPackages do
  begin

    if not (options.UseSource and SameText(packageInfo.Id, options.SearchTerms)) and compiledPackages.Contains(packageInfo)  then
    begin
      packageBasePath := packageInfo.Id + PathDelim + packageInfo.Version.ToStringNoMeta + PathDelim;
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

      for packageSearchPath in packageMetadata.SearchPaths do
        searchPaths.Add(packageBasePath + packageSearchPath.Path);
    end;
  end;
end;

function TPackageInstaller.CompilePackage(const cancellationToken: ICancellationToken; const compiler: ICompiler; const packageInfo : IPackageInfo; const graphNode : IGraphNode; const packageSpec : IPackageSpec; const force : boolean): boolean;
var
  buildEntry : ISpecBuildEntry;
  packagePath : string;
  projectFile : string;
  searchPaths : IList<string>;
  childNode: IGraphNode;
  bomNode : IGraphNode;
  bomFile : string;
  childSearchPath : string;

  procedure DoCopyFiles(const entry : ISpecBuildEntry);
  var
    copyEntry  : ISpecCopyEntry;
    antPattern : IAntPattern;
    fsPatterns : TArray<IFileSystemPattern>;
    fsPattern : IFileSystemPattern;
    searchBasePath : string;
    files : TStringDynArray;
    f : string;
    destFile : string;
  begin
    for copyEntry in entry.CopyFiles do
    begin
      FLogger.Debug('Post Compile Copy [' + copyEntry.Source + ']..');
      try
        //note : this can throw if the source path steps outside of the base path.
        searchBasePath := TPathUtils.StripWildCard(TPathUtils.CompressRelativePath(packagePath, copyEntry.Source));
        searchBasePath := ExtractFilePath(searchBasePath);

        antPattern := TAntPattern.Create(packagePath);
        fsPatterns := antPattern.Expand(copyEntry.Source);
        for fsPattern in fsPatterns do
        begin
          ForceDirectories(fsPattern.Directory);
          files := TDirectory.GetFiles(fsPattern.Directory, fsPattern.FileMask, TSearchOption.soTopDirectoryOnly);
          for f in files do
          begin
            //copy file to lib directory.
            if copyEntry.flatten then
              destFile := compiler.LibOutputDir + '\' + ExtractFileName(f)
            else
              destFile := compiler.LibOutputDir + '\' + TPathUtils.StripBase(searchBasePath, f);

            ForceDirectories(ExtractFilePath(destFile));

            //FLogger.Debug('Copying "' + f + '" to "' + destFile + '"');

            TFile.Copy(f, destFile, true);

          end;
        end;
      except
        on e : Exception do
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
    //Compare Bill of materials file against node dependencies to determine if we need to compile or not.
    //if the bom file exists that means it was compiled before. We will check that the bom matchs the dependencies
    //in the graph
    bomNode := TBOMFile.LoadFromFile(FLogger, bomFile);

    if bomNode <> nil then
    begin
      if bomNode.AreEqual(graphNode) then
      begin
        exit;
      end;
    end;
  end;

  //if we get here the previous compliation was done with different dependency versions,
  //so we delete the bom and compile again
  DeleteFile(bomFile);

  for buildEntry in packageSpec.TargetPlatform.BuildEntries do
  begin
    FLogger.Information('Building package : ' + buildEntry.Project);

    projectFile := TPath.Combine(packagePath, buildEntry.Project);
    projectFile := TPathUtils.CompressRelativePath('',projectFile);


    // if it's a design time package then we need to do a lot more work.
    // design time packages are win32 (as the IDE is win32) - since we can
    // only have one copy of the design package installed we need to check if
    // it has already been installed via another platform.

    if buildEntry.DesignOnly and ( packageInfo.Platform <> TDPMPlatform.Win32) then
    begin
      compiler.BPLOutputDir := TPath.Combine(packagePath, buildEntry.BplOutputDir);
      compiler.LibOutputDir := TPath.Combine(packagePath, buildEntry.LibOutputDir);
      compiler.Configuration := buildEntry.Config;

      if compiler.Platform <> TDPMPlatform.Win32 then
      begin
        compiler.BPLOutputDir := TPath.Combine(compiler.BPLOutputDir, 'win32');
        compiler.LibOutputDir := TPath.Combine(compiler.LibOutputDir, 'win32');
      end
      else
      begin
        graphNode.LibPath := compiler.LibOutputDir;
        graphNode.BplPath := compiler.BPLOutputDir;
      end;

      if graphNode.HasChildren then
      begin
        searchPaths := TCollections.CreateList<string>;
        for childNode in graphNode.ChildNodes do
        begin
          childSearchPath := FPackageCache.GetPackagePath(childNode.Id, childNode.Version.ToStringNoMeta, compiler.CompilerVersion, compiler.Platform );
          childSearchPath := TPath.Combine(childSearchPath, 'lib\win32');
          searchPaths.Add(childSearchPath);
        end;
        compiler.SetSearchPaths(searchPaths);
      end
      else
        compiler.SetSearchPaths(nil);

      FLogger.Information('Building project [' + projectFile + '] for design time...');
      result := compiler.BuildProject(cancellationToken, projectFile, buildEntry.Config, true);
      if result then
        FLogger.Success('Ok.')
      else
      begin
        FLogger.Error('Building project [' + projectFile + '] failed.');
        exit;
      end;

    end
    else
    begin
      //note we are assuming the build entry paths are all relative.
      compiler.BPLOutputDir := TPath.Combine(packagePath, buildEntry.BplOutputDir);
      compiler.LibOutputDir := TPath.Combine(packagePath, buildEntry.LibOutputDir);
      compiler.Configuration := buildEntry.Config;

      graphNode.LibPath := compiler.LibOutputDir;
      graphNode.BplPath := compiler.BPLOutputDir;

      if graphNode.HasChildren then
      begin
        searchPaths := TCollections.CreateList<string>;
        for childNode in graphNode.ChildNodes do
        begin
          childSearchPath := FPackageCache.GetPackagePath(childNode.Id, childNode.Version.ToStringNoMeta, compiler.CompilerVersion, compiler.Platform);
          childSearchPath := TPath.Combine(childSearchPath, 'lib');
          searchPaths.Add(childSearchPath);
        end;
        compiler.SetSearchPaths(searchPaths);
      end
      else
        compiler.SetSearchPaths(nil);


      result := compiler.BuildProject(cancellationToken, projectFile, buildEntry.Config);
      if result then
        FLogger.Success('Ok.')
      else
      begin
        FLogger.Error('Building project [' + projectFile + '] failed.');
        exit;
      end;


      if buildEntry.BuildForDesign and (compiler.Platform <> TDPMPlatform.Win32) then
      begin
        FLogger.Information('Building project [' + projectFile + '] for design time support...');
        //if buildForDesign is true, then it means the design time bpl's also reference
        //this bpl, so if the platform isn't win32 then we need to build it for win32
        compiler.BPLOutputDir := TPath.Combine(compiler.BPLOutputDir, 'win32');
        compiler.LibOutputDir := TPath.Combine(compiler.LibOutputDir, 'win32');

        if graphNode.HasChildren then
        begin
          searchPaths := TCollections.CreateList<string>;
          for childNode in graphNode.ChildNodes do
          begin
            childSearchPath := FPackageCache.GetPackagePath(childNode.Id, childNode.Version.ToStringNoMeta, compiler.CompilerVersion, compiler.Platform );
            childSearchPath := TPath.Combine(childSearchPath, 'lib\win32');
            searchPaths.Add(childSearchPath);
          end;
          compiler.SetSearchPaths(searchPaths);
        end
        else
          compiler.SetSearchPaths(nil);

        result := compiler.BuildProject(cancellationToken, projectFile, buildEntry.Config, true);
        if result then
          FLogger.Success('Project [' + projectFile + '] Compiled for designtime Ok.')
        else
        begin
          FLogger.Error('Building project [' + projectFile + '] failed.');
          exit;
        end;


      end;

      if buildEntry.CopyFiles.Any then
        DoCopyFiles(buildEntry);

    end;

  end;
  //save the bill of materials file for future reference.
  TBOMFile.SaveToFile(FLogger, bomFile, graphNode);



end;

function TPackageInstaller.Context : IPackageInstallerContext;
begin
  result := FContext;
end;

constructor TPackageInstaller.Create(const logger : ILogger; const configurationManager : IConfigurationManager;
  const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache;
  const dependencyResolver : IDependencyResolver; const context : IPackageInstallerContext;
  const compilerFactory : ICompilerFactory);
begin
  FLogger := logger;
  FConfigurationManager := configurationManager;
  FRepositoryManager := repositoryManager;
  FPackageCache := packageCache;
  FDependencyResolver := dependencyResolver;
  FContext := context;
  FCompilerFactory := compilerFactory;
end;

function TPackageInstaller.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
begin
  result := FPackageCache.GetPackageInfo(cancellationToken, packageId); //faster
  if result = nil then
    result := FRepositoryManager.GetPackageInfo(cancellationToken, packageId); //slower
end;

function TPackageInstaller.DoCachePackage(const cancellationToken : ICancellationToken; const options : TCacheOptions; const platform : TDPMPlatform) : boolean;
var
  packageIdentity : IPackageIdentity;
  searchResult : IList<IPackageIdentity>;
  packageFileName : string;
begin
  result := false;
  if not options.Version.IsEmpty then
    //sourceName will be empty if we are installing the package from a file
    packageIdentity := TPackageIdentity.Create(options.PackageId, '', options.Version, options.CompilerVersion, platform, '')
  else
  begin
    //no version specified, so we need to get the latest version available;
    searchResult := FRepositoryManager.List(cancellationToken, options);
    packageIdentity := searchResult.FirstOrDefault;
    if packageIdentity = nil then
    begin
      FLogger.Error('Package [' + options.PackageId + '] for platform [' + DPMPlatformToString(platform) + '] not found on any sources');
      exit;
    end;
  end;
  FLogger.Information('Caching package ' + packageIdentity.ToString);

  if not FPackageCache.EnsurePackage(packageIdentity) then
  begin
    //not in the cache, so we need to get it from the the repository
    if not FRepositoryManager.DownloadPackage(cancellationToken, packageIdentity, FPackageCache.PackagesFolder, packageFileName) then
    begin
      FLogger.Error('Failed to download package [' + packageIdentity.ToString + ']');
      exit;
    end;
    if not FPackageCache.InstallPackageFromFile(packageFileName, true) then
    begin
      FLogger.Error('Failed to cache package file [' + packageFileName + '] into the cache');
      exit;
    end;
  end;
  result := true;

end;


function TPackageInstaller.CreateProjectRefs(const cancellationToken : ICancellationToken; const node : IGraphNode; const seenPackages : IDictionary<string, IPackageInfo>; const projectReferences : IList<TProjectReference>) : boolean;
var
  child : IGraphNode;
  info : IPackageInfo;
  projectRef : TProjectReference;
begin
  result := true;
  if not node.IsRoot then
  begin
    if seenPackages.TryGetValue(LowerCase(node.Id), info) then
    begin
      //if a node uses source then we need to find the projectRef and update i
      if node.UseSource then
        info.UseSource := true;
      exit;
    end;
    info := GetPackageInfo(cancellationToken, node);
    if info = nil then
    begin
      FLogger.Error('Unable to resolve package : ' + node.ToString);
      exit(false);
    end;
    info.UseSource := node.UseSource;
    projectRef.Package := info;
    projectRef.VersionRange := node.SelectedOn;
    projectRef.ParentId := node.Parent.id;
    seenPackages[LowerCase(node.Id)] := info;
    projectReferences.Add(projectRef);
  end;
  if node.HasChildren then
    for child in node.ChildNodes do
    begin
      result := CreateProjectRefs(cancellationToken,child, seenPackages, projectReferences);
      if not result then
        exit;
    end;
end;


function TPackageInstaller.DoInstallPackage(const cancellationToken : ICancellationToken; const options : TInstallOptions; const projectFile : string; const projectEditor : IProjectEditor; const platform : TDPMPlatform; const config : IConfiguration) : boolean;
var
  newPackageIdentity : IPackageIdentity;
  searchResult : IList<IPackageIdentity>;
  packageFileName : string;
  packageInfo : IPackageInfo; //includes dependencies;
  existingPackageRef : IGraphNode;
  projectPackageGraph : IGraphNode;


  packageSpecs : IDictionary<string, IPackageSpec>;

  projectReferences : IList<TProjectReference>;

  resolvedPackages : IList<IPackageInfo>;
  packagesToCompile : IList<IPackageInfo>;

  compiledPackages : IList<IPackageInfo>;
  packageSearchPaths : IList<string>;
  childNode : IGraphNode;

  packageCompiler : ICompiler;

  seenPackages : IDictionary<string, IPackageInfo>;
begin
  result := false;

  projectPackageGraph := projectEditor.GetPackageReferences(platform); //can return nil
  if projectPackageGraph = nil then
    projectPackageGraph := TGraphNode.CreateRoot(options.CompilerVersion,  platform);

  //see if it's already installed.
  existingPackageRef := projectPackageGraph.FindChild(options.PackageId);
  if (existingPackageRef <> nil) then
  begin
    //if it's installed already and we're not forcing it to install then we're done.
    if not options.Force  then
    begin
      //Note this error won't show from the IDE as we always force install from the IDE.
      FLogger.Error('Package [' + newPackageIdentity.ToString + '] is already installed. Use option -force to force reinstall.');
      exit;
    end;
    //remove it so we can force resolution to happen later.
    projectPackageGraph.RemoveNode(existingPackageRef);
    existingPackageRef := nil; //we no longer need it.
  end;

  //We could have a transitive dependency that is being promoted.
  //Since we want to control what version is installed, we will remove
  //any transitive references to that package so the newly installed version
  //will take precedence when resolving.
  childNode := projectPackageGraph.FindFirstNode(options.PackageId);
  while childNode <> nil do
  begin
    projectPackageGraph.RemoveNode(childNode);
    childNode := projectPackageGraph.FindFirstNode(options.PackageId);
  end;


  //if the user specified a version, either the on the command line or via a file then we will use that
  if not options.Version.IsEmpty then
    //sourceName will be empty if we are installing the package from a file
    newPackageIdentity := TPackageIdentity.Create(options.PackageId, '', options.Version, options.CompilerVersion, platform, '')
  else
  begin
    //no version specified, so we need to get the latest version available;
    searchResult := FRepositoryManager.List(cancellationToken, options);
    newPackageIdentity := searchResult.FirstOrDefault;
    if newPackageIdentity = nil then
    begin
      FLogger.Error('Package [' + options.PackageId + '] for platform [' + DPMPlatformToString(platform) + '] not found on any sources');
      exit;
    end;
  end;
  FLogger.Information('Installing package ' + newPackageIdentity.ToString);



  if not FPackageCache.EnsurePackage(newPackageIdentity) then
  begin
    //not in the cache, so we need to get it from the the repository
    if not FRepositoryManager.DownloadPackage(cancellationToken, newPackageIdentity, FPackageCache.PackagesFolder, packageFileName) then
    begin
      FLogger.Error('Failed to download package [' + newPackageIdentity.ToString + ']');
      exit;
    end;
    if not FPackageCache.InstallPackageFromFile(packageFileName, true) then
    begin
      FLogger.Error('Failed to install package file [' + packageFileName + '] into the cache');
      exit;
    end;
  end;

  //get the package info, which has the dependencies.
  packageInfo := GetPackageInfo(cancellationToken, newPackageIdentity);
  packageInfo.UseSource := options.UseSource; //we need this later when collecting search paths.

  seenPackages := TCollections.CreateDictionary<string, IPackageInfo>;
  projectReferences := TCollections.CreateList<TProjectReference>;

  if not CreateProjectRefs(cancellationtoken, projectPackageGraph, seenPackages, projectReferences) then
    exit;

  result := FDependencyResolver.ResolveForInstall(cancellationToken, options, packageInfo, projectReferences, projectPackageGraph, packageInfo.CompilerVersion, platform, resolvedPackages);
  if not result then
    exit;

  if resolvedPackages = nil then
  begin
    FLogger.Error('Resolver returned no packages!');
    exit(false);
  end;

  //get the package we were installing.
  packageInfo := resolvedPackages.FirstOrDefault(function(const info : IPackageInfo) : boolean
                                                 begin
                                                   result := SameText(info.Id, packageInfo.Id);
                                                 end);

  if packageInfo = nil then
  begin
    FLogger.Error('Something went wrong, resolution did not return installed package!');
    exit(false);
  end;

  //downloads the package files to the cache if they are not already there and
  //returns the deserialized dspec as we need it for search paths and
  result := DownloadPackages(cancellationToken, resolvedPackages, packageSpecs);
  if not result then
    exit;

  compiledPackages := TCollections.CreateList<IPackageInfo>;
  packagesToCompile := TCollections.CreateList<IPackageInfo>(resolvedPackages);
  packageSearchPaths := TCollections.CreateList<string>;
  packageCompiler := FCompilerFactory.CreateCompiler(options.CompilerVersion, platform);

  if not BuildDependencies(cancellationToken,packageCompiler, projectPackageGraph, packagesToCompile, compiledPackages, packageSpecs, options ) then
    exit;

  if not CollectSearchPaths(options, resolvedPackages, compiledPackages, projectEditor.CompilerVersion, platform, packageSearchPaths) then
    exit;

  if not projectEditor.AddSearchPaths(platform, packageSearchPaths, config.PackageCacheLocation) then
    exit;

  projectEditor.UpdatePackageReferences(projectPackageGraph, platform);
  result := projectEditor.SaveProject();

end;


function TPackageInstaller.DoRestoreProject(const cancellationToken : ICancellationToken; const options : TRestoreOptions; const projectFile : string; const projectEditor : IProjectEditor; const platform : TDPMPlatform; const config : IConfiguration) : boolean;
var
  projectPackageGraph : IGraphNode;
  packageSpecs : IDictionary<string, IPackageSpec>;
  projectReferences : IList<TProjectReference>;
  resolvedPackages : IList<IPackageInfo>;
  packagesToCompile : IList<IPackageInfo>;
  compiledPackages : IList<IPackageInfo>;
  packageSearchPaths : IList<string>;
  packageCompiler : ICompiler;
  seenPackages : IDictionary<string, IPackageInfo>;
begin
  result := false;

  projectPackageGraph := projectEditor.GetPackageReferences(platform); //can return nil
  //if there is no project package graph then there is nothing to do.
  if projectPackageGraph = nil then
    exit(true);

  seenPackages := TCollections.CreateDictionary<string, IPackageInfo>;
  projectReferences := TCollections.CreateList<TProjectReference>;

  //TODO : Can packagerefs be replaced by just adding the info to the nodes?
  if not CreateProjectRefs(cancellationtoken, projectPackageGraph, seenPackages, projectReferences) then
    exit;

  result := FDependencyResolver.ResolveForRestore(cancellationToken, options, projectReferences, projectPackageGraph, projectEditor.CompilerVersion, platform, resolvedPackages);
  if not result then
    exit;

  //TODO : The code from here on is the same for install/uninstall/restore - refactor!!!

  if resolvedPackages = nil then
  begin
    FLogger.Error('Resolver returned no packages!');
    exit(false);
  end;

  //downloads the package files to the cache if they are not already there and
  //returns the deserialized dspec as we need it for search paths and
  result := DownloadPackages(cancellationToken, resolvedPackages, packageSpecs);
  if not result then
    exit;

  compiledPackages := TCollections.CreateList<IPackageInfo>;
  packagesToCompile := TCollections.CreateList<IPackageInfo>(resolvedPackages);
  packageSearchPaths := TCollections.CreateList<string>;
  packageCompiler := FCompilerFactory.CreateCompiler(options.CompilerVersion, platform);

  if not BuildDependencies(cancellationToken,packageCompiler, projectPackageGraph, packagesToCompile, compiledPackages, packageSpecs, options ) then
    exit;

  if not CollectSearchPaths(options, resolvedPackages, compiledPackages, projectEditor.CompilerVersion, platform, packageSearchPaths) then
    exit;

  if not projectEditor.AddSearchPaths(platform, packageSearchPaths, config.PackageCacheLocation) then
    exit;

  projectEditor.UpdatePackageReferences(projectPackageGraph, platform);
  result := projectEditor.SaveProject();
end;

function TPackageInstaller.DoUninstallFromProject(const cancellationToken: ICancellationToken; const options: TUnInstallOptions; const projectFile: string;
                                                  const projectEditor: IProjectEditor; const platform: TDPMPlatform; const config: IConfiguration): boolean;
var
  projectPackageGraph : IGraphNode;
  foundReference : IGraphNode;
  packageSpecs : IDictionary<string, IPackageSpec>;
  projectReferences : IList<TProjectReference>;
  resolvedPackages : IList<IPackageInfo>;
  packagesToCompile : IList<IPackageInfo>;
  compiledPackages : IList<IPackageInfo>;
  packageSearchPaths : IList<string>;
  packageCompiler : ICompiler;
  seenPackages : IDictionary<string, IPackageInfo>;
begin
  result := false;
  projectPackageGraph := projectEditor.GetPackageReferences(platform); //can return nil
  //if there is no project package graph then there is nothing to do.
  if projectPackageGraph = nil then
  begin
    FLogger.Information('Package [' + options.PackageId + '] was not referenced in project [' + projectFile + '] for platform [' + DPMPlatformToString(platform) + '] - nothing to do.');
    exit(true);
  end;

  foundReference := projectPackageGraph.FindChild(options.PackageId);

  if foundReference = nil then
  begin
    FLogger.Information('Package [' + options.PackageId + '] was not referenced in project [' + projectFile + '] for platform [' + DPMPlatformToString(platform) + '] - nothing to do.');
    //TODO : Should this fail with an error? It's a noop
    exit(true);
  end;

  projectPackageGraph.RemoveNode(foundReference);


  seenPackages := TCollections.CreateDictionary<string, IPackageInfo>;
  projectReferences := TCollections.CreateList<TProjectReference>;

  //TODO : Can packagerefs be replaced by just adding the info to the nodes?
  if not CreateProjectRefs(cancellationtoken, projectPackageGraph, seenPackages, projectReferences) then
    exit;

  result := FDependencyResolver.ResolveForRestore(cancellationToken, options, projectReferences, projectPackageGraph, projectEditor.CompilerVersion, platform, resolvedPackages);
  if not result then
    exit;


  if resolvedPackages = nil then
  begin
    FLogger.Error('Resolver returned no packages!');
    exit(false);
  end;

  //downloads the package files to the cache if they are not already there and
  //returns the deserialized dspec as we need it for search paths and
  result := DownloadPackages(cancellationToken, resolvedPackages, packageSpecs);
  if not result then
    exit;

  compiledPackages := TCollections.CreateList<IPackageInfo>;
  packagesToCompile := TCollections.CreateList<IPackageInfo>(resolvedPackages);
  packageSearchPaths := TCollections.CreateList<string>;
  packageCompiler := FCompilerFactory.CreateCompiler(options.CompilerVersion, platform);

  //even though we are just uninstalling a package here.. we still need to run the compiliation stage to collect paths
  //it will mostly be a no-op as everyhing is likely already compiled.
  if not BuildDependencies(cancellationToken,packageCompiler, projectPackageGraph, packagesToCompile, compiledPackages, packageSpecs, options) then
    exit;

  if not CollectSearchPaths(options, resolvedPackages, compiledPackages, projectEditor.CompilerVersion, platform, packageSearchPaths) then
    exit;

  if not projectEditor.AddSearchPaths(platform, packageSearchPaths, config.PackageCacheLocation) then
    exit;

  projectEditor.UpdatePackageReferences(projectPackageGraph, platform);
  result := projectEditor.SaveProject();

end;


function TPackageInstaller.BuildDependencies(const cancellationToken : ICancellationToken; const packageCompiler : ICompiler; const projectPackageGraph : IGraphNode; const packagesToCompile : IList<IPackageInfo>;
                                             const compiledPackages : IList<IPackageInfo>; packageSpecs : IDictionary<string, IPackageSpec>; const options : TSearchOptions) : boolean;
begin
  result := false;
  try
    //build the dependency graph in the correct order.
    projectPackageGraph.VisitDFS(
      procedure(const node : IGraphNode)
      var
        pkgInfo : IPackageInfo;
        spec : IPackageSpec;
        otherNodes : IList<IGraphNode>;
        forceCompile : boolean;
      begin
        Assert(node.IsRoot = false, 'graph should not visit root node');

        pkgInfo := packagesToCompile.FirstOrDefault(
          function(const value : IPackageInfo) : boolean
          begin
            result := SameText(value.Id, node.Id);
          end);
        //if it's not found that means we have already processed the package elsewhere in the graph
        if pkgInfo = nil then
          exit;

        //do we need an option to force compilation when restoring?
        forceCompile := options.force and SameText(pkgInfo.Id, options.SearchTerms); //searchterms backs packageid

        //removing it so we don't process it again
        packagesToCompile.Remove(pkgInfo);

        spec := packageSpecs[LowerCase(node.Id)];
        Assert(spec <> nil);

        if spec.TargetPlatform.BuildEntries.Any then
        begin
          //we need to build the package.
          if not CompilePackage(cancellationToken, packageCompiler, pkgInfo, node, spec, forceCompile) then
            raise Exception.Create('Comping package [' + pkgInfo.ToIdVersionString + '] failed.' );
          compiledPackages.Add(pkgInfo);
          //compiling updates the node searchpaths and libpath, so just copy to any same package nodes
          otherNodes := projectPackageGraph.FindNodes(node.Id);
          if otherNodes.Count > 1 then
           otherNodes.ForEach(procedure(const otherNode : IGraphNode)
                             begin
                                otherNode.SearchPaths.Clear;
                                otherNode.SearchPaths.AddRange(node.SearchPaths);
                                otherNode.LibPath := node.LibPath;
                                otherNode.BplPath := node.BplPath;
                             end);
        end;

        if spec.TargetPlatform.DesignFiles.Any then
        begin
          //we have design time packages to install.
        end;
      end);
      result := true;

  except
    on e : Exception do
    begin
      FLogger.Error(e.Message);
      exit;
    end;
  end;

end;

(*
function TPackageInstaller.DoUninstallFromProject(const cancellationToken: ICancellationToken; const options: TUnInstallOptions; const projectFile: string;
                                                  const projectEditor: IProjectEditor; const platform: TDPMPlatform; const config: IConfiguration): boolean;
var
  packageReference : IGraphNode;
  packageReferences : IList<IGraphNode>;
  projectPackageInfos : IList<IPackageInfo>;
  projectPackageInfo : IPackageInfo;
  resolvedPackages : IList<IPackageInfo>;
  compiledPackages : IList<IPackageInfo>;
  packageSpecs : IDictionary<string, IPackageSpec>;
  packageSearchPaths : IList<string>;
  conflictDetect : IDictionary<string, TPackageVersion>;
  dependencyGraph : IGraphNode;
  projectReferences : IList<TProjectReference>;
  foundReference : IGraphNode;
begin
  result := false;

  //get the packages already referenced by the project for the platform
  packageReferences := TCollections.CreateList<IGraphNode>;

  conflictDetect := TCollections.CreateDictionary < string, TPackageVersion > ;

  dependencyGraph := TGraphNode.CreateRoot(options.CompilerVersion, platform);

  foundReference := nil;
  for packageReference in projectEditor.PackageReferences.Where(
    function(const packageReference : IGraphNode) : boolean
    begin
      result := platform = packageReference.Platform;
    end) do
  begin
    if not SameText(packageReference.Id, options.PackageId) then
    begin
      BuildGraph(packageReference, dependencyGraph);
      AddPackageReference(packageReference, packageReferences, conflictDetect);
    end
    else
      foundReference := packageReference;
  end;

  if foundReference = nil then
  begin
    FLogger.Information('Package [' + options.PackageId + '] was not referenced in project [' + projectFile + '] for platform [' + DPMPlatformToString(platform) + '] - nothing to do.');
    //TODO : Should this fail with an error? It's a noop
    exit(true);
  end;

  projectEditor.PackageReferences.Remove(foundReference);

  //NOTE : Even though there might be no package references after this, we still need to do the restore process to update search paths etc.

  projectPackageInfos := TCollections.CreateList<IPackageInfo>;
  for packageReference in packageReferences do
  begin
    projectPackageInfo := GetPackageInfo(cancellationToken, packageReference);
    if projectPackageInfo = nil then
    begin
      FLogger.Error('Unable to resolve package : ' + packageReference.ToString);
      exit;
    end;
    projectPackageInfos.Add(projectPackageInfo);
  end;


  projectReferences := TCollections.CreateList<TProjectReference>;

  if packageReferences.Any then
    projectReferences.AddRange(TEnumerable.Select<IPackageInfo, TProjectReference>(projectPackageInfos,
      function(const info : IPackageInfo) : TProjectReference
      var
        node : IGraphNode;
        parentNode : IGraphNode;
      begin
        result.Package := info;
        node := dependencyGraph.FindFirstNode(info.Id);
        if node <> nil then
        begin
          result.VersionRange := node.SelectedOn;
          parentNode := node.Parent;
          result.ParentId := parentNode.Id;
        end
        else
        begin
          result.VersionRange := TVersionRange.Empty;
          result.ParentId := 'root';
        end;
      end));


  //NOTE : The resolver may modify the graph.
  result := FDependencyResolver.ResolveForRestore(cancellationToken, options, projectReferences, dependencyGraph, options.CompilerVersion, platform, resolvedPackages);
  if not result then
    exit;

  if resolvedPackages = nil then
  begin
    FLogger.Error('Resolver returned no packages!');
    exit(false);
  end;

  if resolvedPackages.Any then
  begin
    for projectPackageInfo in resolvedPackages do
    begin
      FLogger.Information('Resolved : ' + projectPackageInfo.ToIdVersionString);
    end;

    result := DownloadPackages(cancellationToken, resolvedPackages, packageSpecs );
    if not result then
      exit;
  end;

  //TODO : Detect if anything has actually changed and only do this if we need to
  packageSearchPaths := TCollections.CreateList <string> ;

  if not CollectSearchPaths(resolvedPackages, compiledPackages, projectEditor.CompilerVersion, platform, packageSearchPaths) then
    exit;

  if not projectEditor.AddSearchPaths(platform, packageSearchPaths, config.PackageCacheLocation) then
    exit;

  packageReferences.Clear;
  GeneratePackageReferences(dependencyGraph, nil, packageReferences, options.CompilerVersion, platform);

  projectEditor.UpdatePackageReferences(packageReferences, platform);
  result := projectEditor.SaveProject();
end;

*)

function TPackageInstaller.DownloadPackages(const cancellationToken : ICancellationToken; const resolvedPackages : IList<IPackageInfo>; var packageSpecs : IDictionary<string, IPackageSpec>) : boolean;
var
  packageInfo : IPackageInfo;
  packageFileName : string;
  spec : IPackageSpec;
begin
  result := false;
  packageSpecs := TCollections.CreateDictionary<string, IPackageSpec>;

  for packageInfo in resolvedPackages do
  begin
    if cancellationToken.IsCancelled then
      exit;

    if not FPackageCache.EnsurePackage(packageInfo) then
    begin
      //not in the cache, so we need to get it from the the repository
      if not FRepositoryManager.DownloadPackage(cancellationToken, packageInfo, FPackageCache.PackagesFolder, packageFileName) then
      begin
        FLogger.Error('Failed to download package [' + packageInfo.ToString + ']');
        exit;
      end;
      if not FPackageCache.InstallPackageFromFile(packageFileName, true) then
      begin
        FLogger.Error('Failed to install package file [' + packageFileName + '] into the cache');
        exit;
      end;
      if cancellationToken.IsCancelled then
        exit;
    end;
    if not packageSpecs.ContainsKey(LowerCase(packageInfo.Id)) then
    begin
      spec := FPackageCache.GetPackageSpec(packageInfo);
      packageSpecs[LowerCase(packageInfo.Id)] := spec;
    end;

  end;
  result := true;

end;

function TPackageInstaller.InstallPackage(const cancellationToken : ICancellationToken; const options : TInstallOptions; const projectFile : string; const config : IConfiguration) : boolean;
var
  projectEditor : IProjectEditor;
  platforms : TDPMPlatforms;
  platform : TDPMPlatform;
  platformResult : boolean;
  ambiguousProjectVersion : boolean;
  ambiguousVersions : string;
begin
  result := false;

  //make sure we can parse the dproj
  projectEditor := TProjectEditor.Create(FLogger, config, options.CompilerVersion);
  if not projectEditor.LoadProject(projectFile) then
  begin
    FLogger.Error('Unable to load project file, cannot continue');
    exit;
  end;

  ambiguousProjectVersion := IsAmbigousProjectVersion(projectEditor.ProjectVersion, ambiguousVersions);

  if ambiguousProjectVersion and (options.CompilerVersion = TCompilerVersion.UnknownVersion) then
    FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] is ambiguous (' + ambiguousVersions  +'), recommend specifying compiler version on command line.');

  //if the compiler version was specified (either on the command like or through a package file)
  //then make sure our dproj is actually for that version.
  if options.CompilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    if projectEditor.CompilerVersion <> options.CompilerVersion then
    begin
      if not ambiguousProjectVersion then
        FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] does not match the compiler version.');
      projectEditor.CompilerVersion := options.CompilerVersion;
    end;
  end
  else
    options.CompilerVersion := projectEditor.CompilerVersion;


  //if the platform was specified (either on the command like or through a package file)
  //then make sure our dproj is actually for that platform.
  if options.Platforms <> [] then
  begin
    platforms := options.Platforms * projectEditor.Platforms; //gets the intersection of the two sets.
    if platforms = [] then //no intersection
    begin
      FLogger.Warning('Skipping project file [' + projectFile + '] as it does not match target specified platforms.');
      exit;
    end;
    //TODO : what if only some of the platforms are supported, what should we do?
  end
  else
    platforms := projectEditor.Platforms;

  result := true;
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit;

    options.Platforms := [platform];
    FLogger.Information('Installing [' + options.SearchTerms + '-' + DPMPlatformToString(platform) + '] into [' + projectFile + ']', true);
    platformResult := DoInstallPackage(cancellationToken, options, projectFile, projectEditor, platform, config);
    if not platformResult then
      FLogger.Error('Install failed for [' + options.SearchTerms + '-' + DPMPlatformToString(platform) + ']')
    else
      FLogger.Success('Install succeeded for [' + options.SearchTerms + '-' + DPMPlatformToString(platform) + ']', true);

    result := platformResult and result;
    FLogger.Information('');
  end;

end;

procedure TPackageInstaller.GenerateSearchPaths(const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; packageSpec: IPackageSpec;
                                               const searchPaths: IList<string>);
var
  packageBasePath : string;
  packageSearchPath : ISpecSearchPath;
begin
  packageBasePath := packageSpec.MetaData.Id + PathDelim + packageSpec.MetaData.Version.ToStringNoMeta + PathDelim;

  for packageSearchPath  in packageSpec.TargetPlatform.SearchPaths do
      searchPaths.Add(packageBasePath + packageSearchPath.Path);
end;

function TPackageInstaller.GetCompilerVersionFromProjectFiles(const options : TInstallOptions; const projectFiles : TArray <string> ; const config : IConfiguration) : boolean;
var
  projectFile : string;
  projectEditor : IProjectEditor;
  compilerVersion : TCompilerVersion;
  bFirst : boolean;
begin
  result := true;
  compilerVersion := TCompilerVersion.UnknownVersion;
  bFirst := true;
  for projectFile in projectFiles do
  begin
    projectEditor := TProjectEditor.Create(FLogger, config, options.CompilerVersion);
    result := result and projectEditor.LoadProject(projectFile);
    if result then
    begin
      if not bFirst then
      begin
        if projectEditor.CompilerVersion <> compilerVersion then
        begin
          FLogger.Error('Projects are not all the for same compiler version.');
          result := false;
        end;
      end;
      compilerVersion := options.CompilerVersion;
      options.CompilerVersion := projectEditor.CompilerVersion;
      bFirst := false;
    end;
  end;
end;

function TPackageInstaller.Install(const cancellationToken : ICancellationToken; const options : TInstallOptions) : Boolean;
var
  projectFiles : TArray<string>;
  config : IConfiguration;
begin
  result := false;
  try
    if (not options.Validated) and (not options.Validate(FLogger)) then
      exit
    else if not options.IsValid then
      exit;

    config := FConfigurationManager.LoadConfig(options.ConfigFile);
    if config = nil then
      exit;

    FPackageCache.Location := config.PackageCacheLocation;
    if not FRepositoryManager.Initialize(config) then
    begin
      FLogger.Error('Unable to initialize the repository manager.');
      exit;
    end;

    if not FRepositoryManager.HasSources then
    begin
      FLogger.Error('No package sources are defined. Use `dpm sources add` command to add a package source.');
      exit;
    end;


    if FileExists(options.ProjectPath) then
    begin
      if ExtractFileExt(options.ProjectPath) <> '.dproj' then
      begin
        FLogger.Error('Unsupported project file type [' + options.ProjectPath + ']');
      end;
      SetLength(projectFiles, 1);
      projectFiles[0] := options.ProjectPath;

    end
    else if DirectoryExists(options.ProjectPath) then
    begin
      projectFiles := TArray <string> (TDirectory.GetFiles(options.ProjectPath, '*.dproj'));
      if Length(projectFiles) = 0 then
      begin
        FLogger.Error('No dproj files found in projectPath : ' + options.ProjectPath);
        exit;
      end;
      FLogger.Information('Found ' + IntToStr(Length(projectFiles)) + ' dproj file(s) to install into.');
    end
    else
    begin
      //should never happen when called from the commmand line, but might from the IDE plugin.
      FLogger.Error('The projectPath provided does no exist, no project to install to');
      exit;
    end;

    if options.PackageFile <> '' then
    begin
      if not FileExists(options.PackageFile) then
      begin
        //should never happen if validation is called on the options.
        FLogger.Error('The specified packageFile [' + options.PackageFile + '] does not exist.');
        exit;
      end;
      result := InstallPackageFromFile(cancellationToken, options, TArray <string> (projectFiles), config);
    end
    else
      result := InstallPackageFromId(cancellationToken, options, TArray <string> (projectFiles), config);
  except
    on e : Exception do
    begin
      FLogger.Error(e.Message);
      result := false;
    end;
  end;

end;

function TPackageInstaller.InstallPackageFromFile(const cancellationToken : ICancellationToken; const options : TInstallOptions; const projectFiles : TArray <string> ; const config : IConfiguration) : boolean;
var
  packageIdString : string;
  packageIdentity : IPackageIdentity;
  projectFile : string;
begin
  //get the package into the cache first then just install as normal
  result := FPackageCache.InstallPackageFromFile(options.PackageFile, true);
  if not result then
    exit;

  //get the identity so we can get the compiler version
  packageIdString := ExtractFileName(options.PackageFile);
  packageIdString := ChangeFileExt(packageIdString, '');
  if not TPackageIdentity.TryCreateFromString(FLogger, packageIdString, '', packageIdentity) then
    exit;

  //update options so we can install from the packageid.
  options.PackageFile := '';
  options.PackageId := packageIdentity.Id + '.' + packageIdentity.Version.ToStringNoMeta;
  options.CompilerVersion := packageIdentity.CompilerVersion; //package file is for single compiler version
  options.Platforms := [packageIdentity.Platform]; //package file is for single platform.

  FContext.Reset;
  try
    for projectFile in projectFiles do
    begin
      if cancellationToken.IsCancelled then
        exit;
      result := InstallPackage(cancellationToken, options, projectFile, config) and result;
    end;

  finally
    FContext.Reset; //free up memory as this might be used in the IDE
  end;

end;

function TPackageInstaller.InstallPackageFromId(const cancellationToken : ICancellationToken; const options : TInstallOptions; const projectFiles : TArray <string> ; const config : IConfiguration) : boolean;
var
  projectFile : string;
begin
  result := true;
  FContext.Reset;
  try
    for projectFile in projectFiles do
    begin
      if cancellationToken.IsCancelled then
        exit;
      result := InstallPackage(cancellationToken, options, projectFile, config) and result;
    end;
  finally
    FContext.Reset;
  end;
end;

function TPackageInstaller.Remove(const cancellationToken : ICancellationToken; const options : TUninstallOptions) : boolean;
begin
  result := false;
end;

function TPackageInstaller.Restore(const cancellationToken : ICancellationToken; const options : TRestoreOptions) : Boolean;
var
  projectFiles : TArray<string>;
  projectFile : string;
  config : IConfiguration;
  groupProjReader : IGroupProjectReader;
  projectList : IList<string>;
  i : integer;
  projectRoot : string;
begin
  result := false;
  try
    //commandline would have validated already, but IDE probably not.
    if (not options.Validated) and (not options.Validate(FLogger)) then
      exit
    else if not options.IsValid then
      exit;

    config := FConfigurationManager.LoadConfig(options.ConfigFile);
    if config = nil then //no need to log, config manager will
      exit;

    FPackageCache.Location := config.PackageCacheLocation;
    if not FRepositoryManager.Initialize(config) then
    begin
      FLogger.Error('Unable to initialize the repository manager.');
      exit;
    end;

    if not FRepositoryManager.HasSources then
    begin
      FLogger.Error('No package sources are defined. Use `dpm sources add` command to add a package source.');
      exit;
    end;


    if FileExists(options.ProjectPath) then
    begin
      //TODO : If we are using a groupProj then we shouldn't allow different versions of a package in different projects
      //need to work out how to detect this.

      if ExtractFileExt(options.ProjectPath) = '.groupproj' then
      begin
        groupProjReader := TGroupProjectReader.Create(FLogger);
        if not groupProjReader.LoadGroupProj(options.ProjectPath) then
          exit;

        projectList := TCollections.CreateList <string> ;
        if not groupProjReader.ExtractProjects(projectList) then
          exit;

        //projects in a project group are likely to be relative, so make them full paths
        projectRoot := ExtractFilePath(options.ProjectPath);
        for i := 0 to projectList.Count - 1 do
        begin
          //sysutils.IsRelativePath returns false with paths starting with .\
          if TPathUtils.IsRelativePath(projectList[i]) then
            //TPath.Combine really should do this but it doesn't
            projectList[i] := TPathUtils.CompressRelativePath(projectRoot, projectList[i])
        end;
        projectFiles := projectList.ToArray;
      end
      else
      begin
        SetLength(projectFiles, 1);
        projectFiles[0] := options.ProjectPath;
      end;
    end
    else if DirectoryExists(options.ProjectPath) then
    begin
      //todo : add groupproj support!
      projectFiles := TArray <string> (TDirectory.GetFiles(options.ProjectPath, '*.dproj'));
      if Length(projectFiles) = 0 then
      begin
        FLogger.Error('No project files found in projectPath : ' + options.ProjectPath);
        exit;
      end;
      FLogger.Information('Found ' + IntToStr(Length(projectFiles)) + ' project file(s) to restore.');
    end
    else
    begin
      //should never happen when called from the commmand line, but might from the IDE plugin.
      FLogger.Error('The projectPath provided does no exist, no project to install to');
      exit;
    end;

    result := true;
    //TODO : create some sort of context object here to pass in so we can collect runtime/design time packages
    for projectFile in projectFiles do
    begin
      if cancellationToken.IsCancelled then
        exit;
      result := RestoreProject(cancellationToken, options, projectFile, config) and result;
    end;
  except
    on e : Exception do
    begin
      FLogger.Error(e.Message);
      result := false;
    end;
  end;
end;

function TPackageInstaller.RestoreProject(const cancellationToken : ICancellationToken; const options : TRestoreOptions; const projectFile : string; const config : IConfiguration) : Boolean;
var
  projectEditor : IProjectEditor;
  platforms : TDPMPlatforms;
  platform : TDPMPlatform;
  platformResult : boolean;
  ambiguousProjectVersion : boolean;
  ambiguousVersions : string;
begin
  result := false;

  //make sure we can parse the dproj
  projectEditor := TProjectEditor.Create(FLogger, config, options.CompilerVersion);

  if not projectEditor.LoadProject(projectFile) then
  begin
    FLogger.Error('Unable to load project file, cannot continue');
    exit;
  end;


  if cancellationToken.IsCancelled then
    exit;

  ambiguousProjectVersion := IsAmbigousProjectVersion(projectEditor.ProjectVersion, ambiguousVersions);

  if ambiguousProjectVersion and (options.CompilerVersion = TCompilerVersion.UnknownVersion) then
    FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] is ambiguous (' + ambiguousVersions  +'), recommend specifying compiler version on command line.');

  //if the compiler version was specified (either on the command like or through a package file)
  //then make sure our dproj is actually for that version.
  if options.CompilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    if projectEditor.CompilerVersion <> options.CompilerVersion then
    begin
      if not ambiguousProjectVersion then
        FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] does not match the compiler version.');
      projectEditor.CompilerVersion := options.CompilerVersion;
    end;
  end
  else
    options.CompilerVersion := projectEditor.CompilerVersion;

  FLogger.Warning('Restoring for compiler version  [' + CompilerToString(options.CompilerVersion) + '].');


  //if the platform was specified (either on the command like or through a package file)
  //then make sure our dproj is actually for that platform.
  if options.Platforms <> [] then
  begin
    platforms := options.Platforms * projectEditor.Platforms; //gets the intersection of the two sets.
    if platforms = [] then //no intersection
    begin
      FLogger.Warning('Skipping project file [' + projectFile + '] as it does not match specified platforms.');
      exit;
    end;
    //TODO : what if only some of the platforms are supported, what should we do?
  end
  else
    platforms := projectEditor.Platforms;

  result := true;
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit(false);

    options.Platforms := [platform];
    FLogger.Information('Restoring project [' + projectFile + '] for [' + DPMPlatformToString(platform) + ']', true);
    platformResult := DoRestoreProject(cancellationToken, options, projectFile, projectEditor, platform, config);
    if not platformResult then
      FLogger.Error('Restore failed for ' + DPMPlatformToString(platform))
    else
      FLogger.Success('Restore succeeded for ' + DPMPlatformToString(platform), true);
    result := platformResult and result;
    FLogger.Information('');
  end;
end;

function TPackageInstaller.UnInstall(const cancellationToken: ICancellationToken; const options: TUnInstallOptions): boolean;
var
  projectFiles : TArray<string>;
  projectFile : string;
  config : IConfiguration;
  groupProjReader : IGroupProjectReader;
  projectList : IList<string>;
  i : integer;
  projectRoot : string;
begin
  result := false;
  //commandline would have validated already, but IDE probably not.
  if (not options.Validated) and (not options.Validate(FLogger)) then
    exit
  else if not options.IsValid then
    exit;

  config := FConfigurationManager.LoadConfig(options.ConfigFile);
  if config = nil then //no need to log, config manager will
    exit;

  FPackageCache.Location := config.PackageCacheLocation;
  if not FRepositoryManager.Initialize(config) then
  begin
    FLogger.Error('Unable to initialize the repository manager.');
    exit;
  end;

  if FileExists(options.ProjectPath) then
  begin
    //TODO : If we are using a groupProj then we shouldn't allow different versions of a package in different projects
    //need to work out how to detect this.

    if ExtractFileExt(options.ProjectPath) = '.groupproj' then
    begin
      groupProjReader := TGroupProjectReader.Create(FLogger);
      if not groupProjReader.LoadGroupProj(options.ProjectPath) then
        exit;

      projectList := TCollections.CreateList <string> ;
      if not groupProjReader.ExtractProjects(projectList) then
        exit;

      //projects in a project group are likely to be relative, so make them full paths
      projectRoot := ExtractFilePath(options.ProjectPath);
      for i := 0 to projectList.Count - 1 do
      begin
        //sysutils.IsRelativePath returns false with paths starting with .\
        if TPathUtils.IsRelativePath(projectList[i]) then
          //TPath.Combine really should do this but it doesn't
          projectList[i] := TPathUtils.CompressRelativePath(projectRoot, projectList[i])
      end;
      projectFiles := projectList.ToArray;
    end
    else
    begin
      SetLength(projectFiles, 1);
      projectFiles[0] := options.ProjectPath;
    end;
  end
  else if DirectoryExists(options.ProjectPath) then
  begin
    //todo : add groupproj support!
    projectFiles := TArray <string> (TDirectory.GetFiles(options.ProjectPath, '*.dproj'));
    if Length(projectFiles) = 0 then
    begin
      FLogger.Error('No project files found in projectPath : ' + options.ProjectPath);
      exit;
    end;
    FLogger.Information('Found ' + IntToStr(Length(projectFiles)) + ' project file(s) to uninstall from.');
  end
  else
  begin
    //should never happen when called from the commmand line, but might from the IDE plugin.
    FLogger.Error('The projectPath provided does no exist, no project to install to');
    exit;
  end;


  result := true;
  //TODO : create some sort of context object here to pass in so we can collect runtime/design time packages
  for projectFile in projectFiles do
  begin
    if cancellationToken.IsCancelled then
      exit;
    result := UnInstallFromProject(cancellationToken, options, projectFile, config) and result;
  end;


end;

function TPackageInstaller.UnInstallFromProject(const cancellationToken: ICancellationToken; const options: TUnInstallOptions; const projectFile: string; const config: IConfiguration): Boolean;
var
  projectEditor : IProjectEditor;
  platforms : TDPMPlatforms;
  platform : TDPMPlatform;
  platformResult : boolean;
  ambiguousProjectVersion : boolean;
  ambiguousVersions : string;
begin
  result := false;

  //make sure we can parse the dproj
  projectEditor := TProjectEditor.Create(FLogger, config, options.CompilerVersion);

  if not projectEditor.LoadProject(projectFile) then
  begin
    FLogger.Error('Unable to load project file, cannot continue');
    exit;
  end;


  if cancellationToken.IsCancelled then
    exit;

  ambiguousProjectVersion := IsAmbigousProjectVersion(projectEditor.ProjectVersion, ambiguousVersions);

  if ambiguousProjectVersion and (options.CompilerVersion = TCompilerVersion.UnknownVersion) then
    FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] is ambiguous (' + ambiguousVersions  +'), recommend specifying compiler version on command line.');

  //if the compiler version was specified (either on the command like or through a package file)
  //then make sure our dproj is actually for that version.
  if options.CompilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    if projectEditor.CompilerVersion <> options.CompilerVersion then
    begin
      if not ambiguousProjectVersion then
        FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] does not match the compiler version.');
      projectEditor.CompilerVersion := options.CompilerVersion;
    end;
  end
  else
    options.CompilerVersion := projectEditor.CompilerVersion;

  FLogger.Warning('Uninstalling for compiler version  [' + CompilerToString(options.CompilerVersion) + '].');


  //if the platform was specified (either on the command like or through a package file)
  //then make sure our dproj is actually for that platform.
  if options.Platforms <> [] then
  begin
    platforms := options.Platforms * projectEditor.Platforms; //gets the intersection of the two sets.
    if platforms = [] then //no intersection
    begin
      FLogger.Warning('Skipping project file [' + projectFile + '] as it does not match specified platforms.');
      exit;
    end;
    //TODO : what if only some of the platforms are supported, what should we do?
  end
  else
    platforms := projectEditor.Platforms;

  result := true;
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit(false);

    options.Platforms := [platform];
    FLogger.Information('Uninstalling from project [' + projectFile + '] for [' + DPMPlatformToString(platform) + ']', true);
    platformResult := DoUninstallFromProject(cancellationToken, options, projectFile, projectEditor, platform, config);
    if not platformResult then
      FLogger.Error('Uninstall failed for ' + DPMPlatformToString(platform))
    else
      FLogger.Success('Uninstall succeeded for ' + DPMPlatformToString(platform), true);
    result := platformResult and result;
    FLogger.Information('');
  end;
end;

end.

