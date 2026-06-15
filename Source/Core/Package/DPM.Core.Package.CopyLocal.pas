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

unit DPM.Core.Package.CopyLocal;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Options.CopyLocal;

type
  //Copies package binaries from the cache into a project's build output folder. Invoked by the
  //'dpm copylocal' command (from DPM.CopyLocal.targets at build time) and reusable from the IDE.
  ICopyLocalService = interface
    ['{6C7E2A1B-9D4F-4E58-AB31-2F0C8A6E1D77}']
    function CopyLocal(const cancellationToken : ICancellationToken; const options : TCopyLocalOptions) : boolean;
  end;

  TCopyLocalService = class(TInterfacedObject, ICopyLocalService)
  private
    FLogger : ILogger;
    FConfigManager : IConfigurationManager;
    FPackageCache : IPackageCache;
  protected
    function CopyLocal(const cancellationToken : ICancellationToken; const options : TCopyLocalOptions) : boolean;
  public
    constructor Create(const logger : ILogger; const configManager : IConfigurationManager; const packageCache : IPackageCache);
  end;

//Exposed for testing. Removes a trailing run of digits from a package base name so a versioned bpl
//('FooPkg290') compares equal to its dcp/runtime-package token ('FooPkg').
function StripLibSuffix(const baseName : string) : string;

//Exposed for testing. Given a package's own .dcp base names (from lib\{platform}) and .bpl file
//names (from bpl\{platform}), plus the build's semicolon-separated $(DCC_UsePackage) token list,
//returns the bpl file names that should be auto-copied: those bpls paired with a .dcp whose base
//name is one of the referenced runtime-package tokens. Pure (no filesystem access). Using the dcp
//set as the authority excludes design bpls (their library name never appears in $(DCC_UsePackage)).
function ResolveRuntimeBplNames(const dcpBaseNames : array of string; const bplFileNames : array of string;
                                const runtimePackages : string) : TArray<string>;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Types,
  VSoft.AntPatterns,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.Editor,
  DPM.Core.Utils.Files,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.Path;

{ TCopyLocalService }

constructor TCopyLocalService.Create(const logger : ILogger; const configManager : IConfigurationManager; const packageCache : IPackageCache);
begin
  inherited Create;
  FLogger := logger;
  FConfigManager := configManager;
  FPackageCache := packageCache;
end;

//Removes a trailing run of digits from a package base name so a versioned bpl ('FooPkg290') compares
//equal to its dcp/runtime-package token ('FooPkg'). Case is normalised by the caller.
function StripLibSuffix(const baseName : string) : string;
var
  len : integer;
begin
  result := baseName;
  len := Length(result);
  while (len > 0) and CharInSet(result[len], ['0'..'9']) do
    Dec(len);
  result := Copy(result, 1, len);
end;

//True when a $(DCC_UsePackage) token refers to the given dcp base name. Both already lowercased.
//  1. exact match ('sempare.templater' = 'sempare.templater').
//  2. fallback - strip a trailing numeric lib suffix from both sides so a token that carries a
//     suffix ('sempare.templater370') still resolves to the dcp base ('sempare.templater').
function TokenMatchesDcp(const tokenLower : string; const dcpLower : string) : boolean;
var
  tokenStem : string;
  dcpStem : string;
begin
  result := false;
  if (tokenLower = '') or (dcpLower = '') then
    exit;
  if tokenLower = dcpLower then
  begin
    result := true;
    exit;
  end;
  tokenStem := StripLibSuffix(tokenLower);
  dcpStem := StripLibSuffix(dcpLower);
  result := (tokenStem <> '') and (tokenStem = dcpStem);
end;

//Maps each referenced runtime-package token to the package's paired bpl, using the package's own
//.dcp base names as the authority. bpl-centric: for each bpl we find its OWNING dcp (exact base /
//numeric-stem match wins; otherwise the longest dcp that is a prefix of the bpl base - covers a
//custom non-numeric suffix like '_D12' while keeping 'Foo' from claiming 'Foobar290'), then include
//the bpl iff that owning dcp is referenced. Case-insensitive; deduped by bpl file name.
function ResolveRuntimeBplNames(const dcpBaseNames : array of string; const bplFileNames : array of string;
                                const runtimePackages : string) : TArray<string>;
var
  tokens : TArray<string>;
  token : string;
  tokenLower : string;
  i : integer;
  j : integer;
  bplBaseLower : string;
  bplStem : string;
  dcpLower : string;
  owningDcp : string;
  referenced : boolean;
  matched : ISet<string>;
  resultList : IList<string>;
begin
  resultList := TCollections.CreateList<string>;
  if Trim(runtimePackages) = '' then
  begin
    result := resultList.ToArray;
    exit;
  end;
  tokens := TStringUtils.SplitStr(runtimePackages, ';', TSplitStringOptions.ExcludeEmpty);
  matched := TCollections.CreateSet<string>;

  for j := Low(bplFileNames) to High(bplFileNames) do
  begin
    bplBaseLower := LowerCase(ChangeFileExt(bplFileNames[j], ''));
    if bplBaseLower = '' then
      continue;
    bplStem := StripLibSuffix(bplBaseLower);

    //Find the dcp this bpl belongs to.
    owningDcp := '';
    for i := Low(dcpBaseNames) to High(dcpBaseNames) do
    begin
      dcpLower := LowerCase(dcpBaseNames[i]);
      if dcpLower = '' then
        continue;
      if (bplBaseLower = dcpLower) or (bplStem = dcpLower) then
      begin
        owningDcp := dcpLower; //exact / numeric-stem match is definitive.
        break;
      end;
      //custom-suffix fallback: keep the longest dcp that prefixes the bpl base.
      if (Copy(bplBaseLower, 1, Length(dcpLower)) = dcpLower) and (Length(dcpLower) > Length(owningDcp)) then
        owningDcp := dcpLower;
    end;
    if owningDcp = '' then
      continue;

    //Is the owning dcp referenced by the build?
    referenced := false;
    for token in tokens do
    begin
      tokenLower := LowerCase(Trim(token));
      if TokenMatchesDcp(tokenLower, owningDcp) then
      begin
        referenced := true;
        break;
      end;
    end;

    if referenced and matched.Add(LowerCase(bplFileNames[j])) then
      resultList.Add(bplFileNames[j]);
  end;

  result := resultList.ToArray;
end;

function TCopyLocalService.CopyLocal(const cancellationToken : ICancellationToken; const options : TCopyLocalOptions) : boolean;
var
  config : IConfiguration;
  editor : IProjectEditor;
  compilerVersion : TCompilerVersion;
  graphRoot : IPackageReference;
  outputDir : string;
  projectDir : string;
  visited : ISet<string>;
  copiedTargets : ISet<string>;
  copiedCount : integer;

  //Resolves outputDir against the project folder when it was passed as a relative path.
  function ResolveOutputDir : string;
  begin
    result := ExcludeTrailingPathDelimiter(StringReplace(Trim(options.OutputDir), '/', PathDelim, [rfReplaceAll]));
    if TPathUtils.IsRelativePath(result) then
    begin
      result := TPath.Combine(projectDir, result);
      result := TPathUtils.CompressRelativePath('', result);
    end;
  end;

  //Copies one source file into the flat output folder, deduped by target filename across the whole
  //invocation so the explicit and automatic passes never copy/log the same file twice.
  procedure CopyOneFile(const srcFile : string; const packageId : string);
  var
    targetFile : string;
    targetKey : string;
  begin
    targetKey := LowerCase(ExtractFileName(srcFile));
    if copiedTargets.Contains(targetKey) then
      exit;
    targetFile := TPath.Combine(outputDir, ExtractFileName(srcFile));
    if FileExists(targetFile) and TFileUtils.AreSameFiles(srcFile, targetFile) then
    begin
      copiedTargets.Add(targetKey);
      exit;
    end;
    try
      ForceDirectories(outputDir);
      TFile.Copy(srcFile, targetFile, true);
      copiedTargets.Add(targetKey);
      Inc(copiedCount);
      FLogger.Information('Copied [' + ExtractFileName(srcFile) + '] to output for package [' + packageId + '].');
    except
      on e : Exception do
        FLogger.Warning('Unable to copy [' + srcFile + '] to output during copylocal: ' + e.Message);
    end;
  end;

  procedure CopyEntriesForPackage(const node : IPackageReference);
  var
    identity : IPackageIdentity;
    spec : IPackageSpec;
    template : ISpecTemplate;
    packagePath : string;
    platformFolder : string;
    copyEntry : ISpecCopyLocalEntry;
    antPattern : IAntPattern;
    patterns : TArray<IFileSystemPattern>;
    pattern : IFileSystemPattern;
    sourceGlob : string;
    files : TStringDynArray;
    srcFile : string;
    libDir : string;
    bplDir : string;
    dcpFiles : TStringDynArray;
    bplFiles : TStringDynArray;
    dcpBaseNames : TArray<string>;
    bplFileNames : TArray<string>;
    runtimeBplNames : TArray<string>;
    bplName : string;
    i : integer;
  begin
    identity := TPackageIdentity.Create('', node.Id, node.Version, compilerVersion);
    spec := nil;
    try
      spec := FPackageCache.GetPackageSpec(identity);
    except
      on e : Exception do
        FLogger.Warning('[copylocal] could not load spec for ' + node.Id + ' ' + node.Version.ToStringNoMeta + ' : ' + e.Message);
    end;
    if spec = nil then
      exit;

    template := spec.FindTemplate(spec.TargetPlatform.TemplateName);
    packagePath := FPackageCache.GetPackagePath(identity);
    platformFolder := DPMPlatformToBDString(options.Platform);

    //1. Explicit copyLocal entries - always copied (platform filter permitting), any file type.
    if (template <> nil) and template.CopyLocalEntries.Any then
    begin
      antPattern := TAntPattern.Create(packagePath);
      for copyEntry in template.CopyLocalEntries do
      begin
        //When the entry lists platforms, restrict to a matching one (Win64/Win64x interchangeable);
        //an empty list means the entry applies to any platform.
        if (copyEntry.Platforms <> []) and
           (not PlatformSatisfiedBy(options.Platform, copyEntry.Platforms)) then
          continue;

        //$platform$ resolves to the build platform's folder name - the same name the installer wrote
        //bpl\{platform} / lib\{platform} output to.
        sourceGlob := StringReplace(copyEntry.Source, '$platform$', platformFolder, [rfReplaceAll, rfIgnoreCase]);
        sourceGlob := StringReplace(sourceGlob, '/', PathDelim, [rfReplaceAll]);
        if (sourceGlob <> '') and (sourceGlob[1] = PathDelim) then
          Delete(sourceGlob, 1, 1);
        patterns := antPattern.Expand(sourceGlob);
        for pattern in patterns do
        begin
          if not TDirectory.Exists(pattern.Directory) then
            continue;
          files := TDirectory.GetFiles(pattern.Directory, pattern.FileMask, TSearchOption.soTopDirectoryOnly);
          for srcFile in files do
            CopyOneFile(srcFile, node.Id);
        end;
      end;
    end;

    //2. Automatic runtime-bpl copy - only when the build links runtime packages. Map each
    //$(DCC_UsePackage) token to the package's paired bpl via its own .dcp files (the authority),
    //which excludes design bpls.
    if (not options.UsePackages) or (Trim(options.RuntimePackages) = '') then
      exit;

    libDir := TPath.Combine(packagePath, 'lib' + PathDelim + platformFolder);
    bplDir := TPath.Combine(packagePath, 'bpl' + PathDelim + platformFolder);
    if (not TDirectory.Exists(libDir)) or (not TDirectory.Exists(bplDir)) then
      exit;

    dcpFiles := TDirectory.GetFiles(libDir, '*.dcp', TSearchOption.soTopDirectoryOnly);
    bplFiles := TDirectory.GetFiles(bplDir, '*.bpl', TSearchOption.soTopDirectoryOnly);
    if (Length(dcpFiles) = 0) or (Length(bplFiles) = 0) then
      exit;

    SetLength(dcpBaseNames, Length(dcpFiles));
    for i := Low(dcpFiles) to High(dcpFiles) do
      dcpBaseNames[i] := ChangeFileExt(ExtractFileName(dcpFiles[i]), '');
    SetLength(bplFileNames, Length(bplFiles));
    for i := Low(bplFiles) to High(bplFiles) do
      bplFileNames[i] := ExtractFileName(bplFiles[i]);

    runtimeBplNames := ResolveRuntimeBplNames(dcpBaseNames, bplFileNames, options.RuntimePackages);
    for bplName in runtimeBplNames do
      CopyOneFile(TPath.Combine(bplDir, bplName), node.Id);
  end;

  procedure Visit(const node : IPackageReference);
  var
    key : string;
    child : IPackageReference;
  begin
    if node = nil then
      exit;
    if node.IsRoot then
    begin
      for child in node.Children do
        Visit(child);
      exit;
    end;
    key := LowerCase(node.Id + '@' + node.Version.ToStringNoMeta);
    if not visited.Contains(key) then
    begin
      visited.Add(key);
      CopyEntriesForPackage(node);
    end;
    for child in node.Children do
      Visit(child);
  end;

begin
  result := false;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
  begin
    FLogger.Error('Unable to load configuration.');
    exit;
  end;
  if FPackageCache.Location = '' then
    FPackageCache.Location := config.PackageCacheLocation;

  if not FileExists(options.ProjectPath) then
  begin
    FLogger.Error('Project file not found: ' + options.ProjectPath);
    exit;
  end;

  compilerVersion := options.CompilerVersion;

  editor := TProjectEditor.Create(FLogger, config, compilerVersion);
  if not editor.LoadProject(options.ProjectPath, [TProjectElement.All]) then
  begin
    FLogger.Error('Unable to load project: ' + options.ProjectPath);
    exit;
  end;

  if compilerVersion = TCompilerVersion.UnknownVersion then
  begin
    compilerVersion := editor.CompilerVersion;
    if compilerVersion = TCompilerVersion.UnknownVersion then
      compilerVersion := ProjectVersionToCompilerVersion(editor.ProjectVersion);
  end;

  graphRoot := editor.GetPackageReferences;
  if graphRoot = nil then
  begin
    //No DPM packages - nothing to copy. Not an error.
    result := true;
    exit;
  end;

  projectDir := ExtractFilePath(options.ProjectPath);
  outputDir := ResolveOutputDir;

  visited := TCollections.CreateSet<string>;
  copiedTargets := TCollections.CreateSet<string>;
  copiedCount := 0;
  Visit(graphRoot);

  if copiedCount = 0 then
    FLogger.Debug('copylocal: nothing to copy for [' + ExtractFileName(options.ProjectPath) + '] (' +
                  DPMPlatformToString(options.Platform) + '/' + options.Config + ').')
  else
    FLogger.Information('copylocal: copied ' + IntToStr(copiedCount) + ' file(s) into [' + outputDir + '].');
  result := true;
end;

end.
