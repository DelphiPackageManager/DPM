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

//Exposed for testing. True when one of the given bpl files (the .bpl files copylocal matched in
//the package cache - whether shipped precompiled or built during install) appears in the build's
//runtime-package link set (semicolon-separated $(DCC_UsePackage) tokens). Non-.bpl entries are
//ignored.
function PackageRuntimeLinked(const bplFiles : array of string; const runtimePackages : string) : boolean;

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

//True when one of the given bpl files appears in the build's runtime-package link set
//($(DCC_UsePackage)). Two-stage match, both case-insensitive:
//  1. Strip the trailing numeric lib suffix from both sides so 'FooPkg290.bpl' matches token 'FooPkg'.
//  2. Fallback for non-numeric/custom lib suffixes: treat it as a match when the dcp reference is a
//     prefix of the bpl filename (e.g. token 'VSoft.Foo' starts bpl 'VSoft.Foo_D12'). A minimum token
//     length guards against short names ('rtl','vcl') matching unrelated bpls.
function PackageRuntimeLinked(const bplFiles : array of string; const runtimePackages : string) : boolean;
const
  cMinPrefixMatch = 4;
var
  tokens : TArray<string>;
  token : string;
  tokenTrimmed : string;
  tokenStem : string;
  i : integer;
  binary : string;
  bplName : string;
  bplStem : string;
begin
  result := false;
  if Trim(runtimePackages) = '' then
    exit;
  tokens := TStringUtils.SplitStr(runtimePackages, ';', TSplitStringOptions.ExcludeEmpty);
  for i := Low(bplFiles) to High(bplFiles) do
  begin
    binary := bplFiles[i];
    if not SameText(ExtractFileExt(binary), '.bpl') then
      continue;
    //bplName is the bpl filename (no extension) - may carry a lib suffix, numeric or not.
    bplName := LowerCase(ChangeFileExt(ExtractFileName(StringReplace(binary, '/', PathDelim, [rfReplaceAll])), ''));
    if bplName = '' then
      continue;
    //bplStem additionally drops a trailing numeric lib suffix ('foopkg290' -> 'foopkg').
    bplStem := StripLibSuffix(bplName);
    for token in tokens do
    begin
      tokenTrimmed := LowerCase(Trim(token));
      if tokenTrimmed = '' then
        continue;
      //1. exact match after stripping the numeric lib suffix from both sides.
      tokenStem := StripLibSuffix(tokenTrimmed);
      if (tokenStem <> '') and (tokenStem = bplStem) then
      begin
        result := true;
        exit;
      end;
      //2. prefix fallback for non-numeric lib suffixes - the dcp reference starts the bpl name.
      if (Length(tokenTrimmed) >= cMinPrefixMatch) and
         (Copy(bplName, 1, Length(tokenTrimmed)) = tokenTrimmed) then
      begin
        result := true;
        exit;
      end;
    end;
  end;
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

  procedure CopyEntriesForPackage(const node : IPackageReference);
  var
    identity : IPackageIdentity;
    spec : IPackageSpec;
    template : ISpecTemplate;
    packagePath : string;
    isRuntimeOnly : boolean;
    copyEntry : ISpecCopyLocalEntry;
    antPattern : IAntPattern;
    patterns : TArray<IFileSystemPattern>;
    pattern : IFileSystemPattern;
    sourceGlob : string;
    files : TStringDynArray;
    srcFile : string;
    targetFile : string;
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
    if (template = nil) or (not template.CopyLocalEntries.Any) then
      exit;

    packagePath := FPackageCache.GetPackagePath(identity);
    antPattern := TAntPattern.Create(packagePath);

    for copyEntry in template.CopyLocalEntries do
    begin
      //When the entry lists platforms, restrict to a matching one (Win64/Win64x interchangeable);
      //an empty list means the entry applies to any platform.
      if (copyEntry.Platforms <> []) and
         (not PlatformSatisfiedBy(options.Platform, copyEntry.Platforms)) then
        continue;

      //runtimeOnly governs only bpls: skip the whole entry when the build doesn't link runtime
      //packages at all. Per-bpl linkage is checked below as each file is copied.
      isRuntimeOnly := copyEntry.Mode = TCopyLocalMode.runtimeOnly;
      if isRuntimeOnly and (not options.UsePackages) then
        continue;

      //$platform$ resolves to the build platform's folder name - the same name the installer wrote
      //bpl\{platform} / lib\{platform} output to - so 'bpl\$platform$\*.bpl' matches the right output.
      sourceGlob := StringReplace(copyEntry.Source, '$platform$', DPMPlatformToBDString(options.Platform), [rfReplaceAll, rfIgnoreCase]);
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
        begin
          //runtimeOnly: copy a bpl only when it's actually in the build's runtime-package link set.
          //A non-bpl here returns false and is skipped (it belongs in an 'always' entry).
          if isRuntimeOnly and (not PackageRuntimeLinked([srcFile], options.RuntimePackages)) then
            continue;
          //Flatten - everything lands directly in the output folder next to the exe.
          targetFile := TPath.Combine(outputDir, ExtractFileName(srcFile));
          if FileExists(targetFile) and TFileUtils.AreSameFiles(srcFile, targetFile) then
            continue;
          try
            ForceDirectories(outputDir);
            TFile.Copy(srcFile, targetFile, true);
            Inc(copiedCount);
            FLogger.Information('Copied [' + ExtractFileName(srcFile) + '] to output for package [' + node.Id + '].');
          except
            on e : Exception do
              FLogger.Warning('Unable to copy [' + srcFile + '] to output during copylocal: ' + e.Message);
          end;
        end;
      end;
    end;
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
