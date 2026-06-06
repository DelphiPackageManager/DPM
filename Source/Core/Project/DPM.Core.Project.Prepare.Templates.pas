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

unit DPM.Core.Project.Prepare.Templates;

interface

uses
  Spring.Collections,
  DPM.Core.Types;

type
  //pkRuntime  : runtime-only package (RuntimeOnlyPackage=true; requires rtl)
  //pkDesign   : design-time package (DesignOnlyPackage=true; requires rtl, designide)
  TPrepareProjectKind = (pkRuntime, pkDesign);

  //A single Pascal source file to include in the package. FormName is set when a
  //matching .dfm file exists alongside the .pas (e.g. MyForm.pas + MyForm.dfm),
  //and is the form's object identifier parsed from the .dfm's first line.
  TPrepareSourceFile = record
    Path : string;       //relative path from the package folder (e.g. ..\..\src\MyForm.pas)
    FormName : string;   //form identifier, or '' when the unit has no associated .dfm
  end;

  TPrepareTemplates = class
  public
    //Renders a minimal .dpk file. sourceFiles are relative paths from the package folder
    //(e.g. "..\..\Source\Unit1.pas") - one `contains` entry is emitted per file.
    //Pass nil/empty to emit a TODO-placeholder. Units with a non-empty FormName get
    //a `{FormName}` annotation appended to the contains entry.
    //requiredPackages are sibling package names (no extension) that this package
    //requires - typically the runtime package(s) a design package depends on.
    //Each is added to the `requires` clause and (for the dproj) emitted as a
    //`<DCCReference Include="<name>.dcp"/>` entry. Pass nil/empty for none.
    class function RenderDpk(const packageId : string; const compiler : TCompilerVersion;
                             const sourceFiles : IList<TPrepareSourceFile>;
                             const requiredPackages : IList<string>;
                             const kind : TPrepareProjectKind) : string;

    //Renders a minimal .dproj file flavored for the supplied compiler. sourceFiles are
    //relative paths from the package folder - one `<DCCReference>` per file. Pass nil/empty
    //to emit just the rtl.dcp reference. platforms drives the per-platform Cfg_N_<Platform>
    //chain stubs and the BorlandProject <Platforms> list; pass [] to fall back to Win32.
    //Units with a non-empty FormName get a child `<Form>FormName</Form>` element.
    class function RenderDproj(const packageId : string; const compiler : TCompilerVersion;
                               const sourceFiles : IList<TPrepareSourceFile>;
                               const requiredPackages : IList<string>;
                               const kind : TPrepareProjectKind;
                               const platforms : TDPMPlatforms) : string;
  end;

implementation

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Utils.Hash;

//Pull in the dpk/dproj scaffold templates. Delphi compiles the .rc on demand via
//brcc32, so rebuilding any binary that uses this unit embeds the latest template
//content. The .rc lives in the Source/ directory (alongside dpm.rc) so both the CLI
//and every IDE plugin share one source - $R resolves first in this unit's directory,
//then in the project (.dpr) directory which is Source/ for every consumer here.
{$R '..\..\DPM.PrepareTemplates.res'}

const
  cDpkResource     = 'DPM_DPK_TEMPLATE';
  cDprojResource   = 'DPM_DPROJ_TEMPLATE';
  cTemplateResType = 'TEMPLATE';

//Load an embedded template resource (RCDATA-style, UTF-8) as a string and normalise
//to CRLF line endings so the generated dpk/dproj is consistent regardless of how the
//template file was saved. Raises on a missing/empty resource - that's a build defect,
//not a recoverable runtime condition.
function LoadTemplateResource(const resName : string) : string;
var
  hRes : HRSRC;
  hMem : HGLOBAL;
  ptr : Pointer;
  size : DWORD;
  bytes : TBytes;
begin
  result := '';
  hRes := FindResource(HInstance, PChar(resName), PChar(cTemplateResType));
  if hRes <> 0 then
  begin
    hMem := LoadResource(HInstance, hRes);
    if hMem <> 0 then
    begin
      ptr := LockResource(hMem);
      size := SizeofResource(HInstance, hRes);
      if (ptr <> nil) and (size > 0) then
      begin
        SetLength(bytes, size);
        Move(ptr^, bytes[0], size);
        result := TEncoding.UTF8.GetString(bytes);
      end;
    end;
  end;
  if result = '' then
    raise Exception.Create('DPM template resource [' + resName + '] is missing or empty - rebuild required.');
  //normalise to CRLF (collapse any CRLF/CR/LF mix to LF first, then expand).
  result := StringReplace(result, #13#10, #10, [rfReplaceAll]);
  result := StringReplace(result, #13, #10, [rfReplaceAll]);
  result := StringReplace(result, #10, #13#10, [rfReplaceAll]);
end;

//Derive a stable project GUID from the project name so regenerating a package project
//(e.g. on every install) produces a byte-identical dproj rather than churning the GUID.
//Uses the first 16 bytes of a SHA256 over the lower-cased name - XE2-safe (no System.Hash).
function DeterministicProjectGuid(const seed : string) : string;
var
  stream : TStringStream;
  digest : TBytes;
  guid : TGUID;
begin
  stream := TStringStream.Create(LowerCase(seed), TEncoding.UTF8);
  try
    digest := THashSHA256.GetHashBytes(stream);
  finally
    stream.Free;
  end;
  Move(digest[0], guid, SizeOf(TGUID));
  result := GUIDToString(guid);
end;

function ExtractUnitName(const relativePath : string) : string;
begin
  //unit identifier = file name without extension. The relative path uses the source
  //file's actual name (e.g. "..\..\Source\VSoft.Base64.pas" -> "VSoft.Base64").
  result := ChangeFileExt(ExtractFileName(relativePath), '');
end;

class function TPrepareTemplates.RenderDpk(const packageId : string; const compiler : TCompilerVersion;
                                            const sourceFiles : IList<TPrepareSourceFile>;
                                            const requiredPackages : IList<string>;
                                            const kind : TPrepareProjectKind) : string;
var
  containsBlock : string;
  requiresBlock : string;
  i : integer;
  entry : TPrepareSourceFile;
  unitName : string;
  separator : string;
  formSuffix : string;
  pasEntries : IList<TPrepareSourceFile>;
  requiresLines : TStringList;
  requireIdx : integer;
begin
  //The dpk `contains` clause only takes Pascal units (.pas). Filter to just those
  //first so we know how many there are - that drives the separator (',' vs ';') for
  //the last entry. Non-.pas files (.inc, .rc, .res) belong only in the dproj as
  //DCCReferences, not in the dpk.
  pasEntries := TCollections.CreateList<TPrepareSourceFile>;
  if sourceFiles <> nil then
    for i := 0 to sourceFiles.Count - 1 do
      if SameText(ExtractFileExt(sourceFiles[i].Path), '.pas') then
        pasEntries.Add(sourceFiles[i]);

  containsBlock := '';
  if pasEntries.Count = 0 then
    containsBlock := '  // TODO: add your units here, e.g. MyUnit in ''..\..\src\MyUnit.pas'';'#13#10 +
                     '  ;'
  else
  begin
    for i := 0 to pasEntries.Count - 1 do
    begin
      entry := pasEntries[i];
      unitName := ExtractUnitName(entry.Path);
      if i < pasEntries.Count - 1 then
        separator := ','
      else
        separator := ';';
      //Form units get a {FormName} annotation between the path and the separator,
      //matching the dpk syntax Delphi emits when a form is added in the IDE.
      if entry.FormName <> '' then
        formSuffix := ' {' + entry.FormName + '}'
      else
        formSuffix := '';
      containsBlock := containsBlock + '  ' + unitName + ' in ''' + entry.Path + '''' + formSuffix + separator + #13#10;
    end;
    //trim trailing CRLF for cleaner output (template already adds the closing CRLF after).
    if (Length(containsBlock) >= 2) and (Copy(containsBlock, Length(containsBlock) - 1, 2) = #13#10) then
      SetLength(containsBlock, Length(containsBlock) - 2);
  end;

  //Build the `requires` clause: always rtl; designide for design packages; plus
  //any sibling packages (e.g. a design package that wraps a runtime package will
  //list the runtime package's base name here). Last entry gets a semicolon, all
  //others a comma.
  requiresLines := TStringList.Create;
  try
    requiresLines.Add('rtl');
    if kind = pkDesign then
      requiresLines.Add('designide');
    if requiredPackages <> nil then
      for requireIdx := 0 to requiredPackages.Count - 1 do
        if Trim(requiredPackages[requireIdx]) <> '' then
          requiresLines.Add(requiredPackages[requireIdx]);

    requiresBlock := '';
    for requireIdx := 0 to requiresLines.Count - 1 do
    begin
      if requireIdx < requiresLines.Count - 1 then
        requiresBlock := requiresBlock + '  ' + requiresLines[requireIdx] + ','#13#10
      else
        requiresBlock := requiresBlock + '  ' + requiresLines[requireIdx] + ';';
    end;
  finally
    requiresLines.Free;
  end;

  result := LoadTemplateResource(cDpkResource);
  result := StringReplace(result, '{{PACKAGE_ID}}', packageId, [rfReplaceAll]);

  //LIBSUFFIX directive syntax differs by version:
  //  - 10.4+ : {$LIBSUFFIX AUTO}  (bare keyword - IDE expands to BDS version)
  //  - older : {$LIBSUFFIX '160'} (quoted string literal)
  //This is NOT the same as the dproj's <DllSuffix>$(Auto)</DllSuffix> MSBuild macro.
  if compiler >= TCompilerVersion.Delphi10_4 then
    result := StringReplace(result, '{{LIBSUFFIX_DIRECTIVE}}', '{$LIBSUFFIX AUTO}', [rfReplaceAll])
  else
    result := StringReplace(result, '{{LIBSUFFIX_DIRECTIVE}}',
      '{$LIBSUFFIX ''' + CompilerToLibSuffix(compiler) + '''}', [rfReplaceAll]);

  //{$RUNONLY} / {$DESIGNONLY} bakes the package kind into the compiled BPL so the
  //IDE doesn't try to load a runtime package as design-time (and vice versa).
  case kind of
    pkDesign  : result := StringReplace(result, '{{KIND_DIRECTIVE}}', '{$DESIGNONLY}', [rfReplaceAll]);
  else //pkRuntime
    result := StringReplace(result, '{{KIND_DIRECTIVE}}', '{$RUNONLY}', [rfReplaceAll]);
  end;
  result := StringReplace(result, '{{REQUIRES_ENTRIES}}', requiresBlock, [rfReplaceAll]);
  result := StringReplace(result, '{{CONTAINS_ENTRIES}}', containsBlock, [rfReplaceAll]);
end;

function ChainStubFor(const platformName, cfg : string) : string;
begin
  //Establishes the Cfg_<n>_<Platform> child config so Delphi can hang per-platform
  //settings off it. Mirrors what the IDE emits when a platform is enabled.
  result :=
    '    <PropertyGroup Condition="(''$(Platform)''==''' + platformName +
      ''' and ''$(' + cfg + ')''==''true'') or ''$(' + cfg + '_' + platformName + ')''!=''''">'#13#10 +
    '        <' + cfg + '_' + platformName + '>true</' + cfg + '_' + platformName + '>'#13#10 +
    '        <CfgParent>' + cfg + '</CfgParent>'#13#10 +
    '        <' + cfg + '>true</' + cfg + '>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10;
end;

function BuildChainStubs(const platforms : TDPMPlatforms; const cfg : string) : string;
const
  cOrder : array[0..14] of TDPMPlatform = (
    TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.Win64x, TDPMPlatform.WinARM64EC,
    TDPMPlatform.MacOS32, TDPMPlatform.MacOS64, TDPMPlatform.MacOSARM64,
    TDPMPlatform.Android, TDPMPlatform.Android64,
    TDPMPlatform.iOS32, TDPMPlatform.iOS64, TDPMPlatform.iOSSimulator, TDPMPlatform.iOSSimARM64,
    TDPMPlatform.Linux64,
    TDPMPlatform.UnknownPlatform); //sentinel - never present in a real platform set
var
  i : integer;
begin
  //iterate in a stable, IDE-friendly order rather than enum order so Win32/Win64
  //come first - matches what the Delphi IDE emits and keeps diffs readable.
  result := '';
  for i := 0 to High(cOrder) do
    if cOrder[i] in platforms then
      result := result + ChainStubFor(DPMPlatformToBDString(cOrder[i]), cfg);
end;

function BuildPlatformsList(const platforms : TDPMPlatforms) : string;
const
  cOrder : array[0..14] of TDPMPlatform = (
    TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.Win64x, TDPMPlatform.WinARM64EC,
    TDPMPlatform.MacOS32, TDPMPlatform.MacOS64, TDPMPlatform.MacOSARM64,
    TDPMPlatform.Android, TDPMPlatform.Android64,
    TDPMPlatform.iOS32, TDPMPlatform.iOS64, TDPMPlatform.iOSSimulator, TDPMPlatform.iOSSimARM64,
    TDPMPlatform.Linux64,
    TDPMPlatform.UnknownPlatform);
var
  i : integer;
begin
  result := '';
  for i := 0 to High(cOrder) do
    if cOrder[i] in platforms then
      result := result + '                <Platform value="' + DPMPlatformToBDString(cOrder[i]) + '">true</Platform>'#13#10;
end;

function PickDefaultPlatform(const platforms : TDPMPlatforms) : string;
const
  cOrder : array[0..14] of TDPMPlatform = (
    TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.Win64x, TDPMPlatform.WinARM64EC,
    TDPMPlatform.MacOS32, TDPMPlatform.MacOS64, TDPMPlatform.MacOSARM64,
    TDPMPlatform.Android, TDPMPlatform.Android64,
    TDPMPlatform.iOS32, TDPMPlatform.iOS64, TDPMPlatform.iOSSimulator, TDPMPlatform.iOSSimARM64,
    TDPMPlatform.Linux64,
    TDPMPlatform.UnknownPlatform);
var
  i : integer;
begin
  //prefer Win32 since the IDE is Win32 and design-time loading expects it; fall
  //back to the first platform in canonical order if Win32 isn't supported.
  if TDPMPlatform.Win32 in platforms then
  begin
    result := 'Win32';
    exit;
  end;
  for i := 0 to High(cOrder) do
    if cOrder[i] in platforms then
    begin
      result := DPMPlatformToBDString(cOrder[i]);
      exit;
    end;
  result := 'Win32';
end;

class function TPrepareTemplates.RenderDproj(const packageId : string; const compiler : TCompilerVersion;
                               const sourceFiles : IList<TPrepareSourceFile>;
                               const requiredPackages : IList<string>;
                               const kind : TPrepareProjectKind;
                               const platforms : TDPMPlatforms) : string;
var
  debugInfoRelease : string;
  projectGuid : string;
  references : string;
  packageKindFlag : string;
  effectivePlatforms : TDPMPlatforms;
  cfgDebugStubs : string;
  cfgReleaseStubs : string;
  platformOverrides : string;
  platformsList : string;
  i : integer;
begin
  //Release config gets debug info disabled (false / 0). Debug config relies on
  //DCC_DebugInfoInExe rather than DCC_DebugInformation, so we only need to set
  //the release value here. The transformer will normalize it for the target
  //compiler when this dproj is propagated.
  if CompilerUsesIntegerDebugInformation(compiler) then
    debugInfoRelease := '0'
  else
    debugInfoRelease := 'false';

  //Derive a stable GUID from the project name so regeneration is idempotent.
  projectGuid := DeterministicProjectGuid(packageId);

  references := '';
  //Required sibling packages come first as <name>.dcp DCCReferences (matches the
  //order Delphi itself emits when a package is added via Project -> Information).
  if requiredPackages <> nil then
    for i := 0 to requiredPackages.Count - 1 do
      if Trim(requiredPackages[i]) <> '' then
        references := references + '        <DCCReference Include="' + requiredPackages[i] + '.dcp"/>'#13#10;

  if sourceFiles <> nil then
    for i := 0 to sourceFiles.Count - 1 do
    begin
      //Form units expand to a non-self-closing DCCReference with a child <Form>
      //element so the IDE knows to bundle the matching .dfm at build time.
      if sourceFiles[i].FormName <> '' then
        references := references +
          '        <DCCReference Include="' + sourceFiles[i].Path + '">'#13#10 +
          '            <Form>' + sourceFiles[i].FormName + '</Form>'#13#10 +
          '        </DCCReference>'#13#10
      else
        references := references + '        <DCCReference Include="' + sourceFiles[i].Path + '"/>'#13#10;
    end;

  //defensive: never emit a dproj with zero <Platform> entries - that would prevent
  //the IDE from loading it. Caller logs the fallback when platforms = [].
  effectivePlatforms := platforms;
  if effectivePlatforms = [] then
    effectivePlatforms := [TDPMPlatform.Win32];

  cfgDebugStubs := BuildChainStubs(effectivePlatforms, 'Cfg_1');
  cfgReleaseStubs := BuildChainStubs(effectivePlatforms, 'Cfg_2');
  platformsList := BuildPlatformsList(effectivePlatforms);

  //Only Win32 gets the DCC_RemoteDebug override (matches what the IDE emits).
  if TDPMPlatform.Win32 in effectivePlatforms then
    platformOverrides :=
      '    <PropertyGroup Condition="''$(Cfg_1_Win32)''!=''''">'#13#10 +
      '        <DCC_RemoteDebug>false</DCC_RemoteDebug>'#13#10 +
      '    </PropertyGroup>'#13#10
  else
    platformOverrides := '';

  //<DllSuffix> tells the IDE the BPL output filename has the libsuffix appended
  //(otherwise the IDE looks for a base-name BPL the compiler never produces).
  //The .dpk's {$LIBSUFFIX} directive still controls the actual compile-time suffix.

  case kind of
    pkDesign  : packageKindFlag := '        <DesignOnlyPackage>true</DesignOnlyPackage>'#13#10;
  else //pkRuntime
    packageKindFlag := '        <RuntimeOnlyPackage>true</RuntimeOnlyPackage>'#13#10;
  end;

  result := LoadTemplateResource(cDprojResource);
  result := StringReplace(result, '{{PACKAGE_ID}}', packageId, [rfReplaceAll]);
  result := StringReplace(result, '{{PROJECT_VERSION}}', CompilerVersionToProjectVersion(compiler), [rfReplaceAll]);
  result := StringReplace(result, '{{LIB_SUFFIX}}', CompilerToDefaultLibSuffix(compiler), [rfReplaceAll]);
  result := StringReplace(result, '{{DPM_COMPILER}}', CompilerToString(compiler), [rfReplaceAll]);
  result := StringReplace(result, '{{DEFAULT_PLATFORM}}', PickDefaultPlatform(effectivePlatforms), [rfReplaceAll]);
  result := StringReplace(result, '{{CFG_DEBUG_PLATFORM_GROUPS}}', cfgDebugStubs, [rfReplaceAll]);
  result := StringReplace(result, '{{CFG_RELEASE_PLATFORM_GROUPS}}', cfgReleaseStubs, [rfReplaceAll]);
  result := StringReplace(result, '{{PLATFORM_OVERRIDES}}', platformOverrides, [rfReplaceAll]);
  result := StringReplace(result, '{{PLATFORMS_LIST}}', platformsList, [rfReplaceAll]);
  result := StringReplace(result, '{{DEBUG_INFO_RELEASE}}', debugInfoRelease, [rfReplaceAll]);
  result := StringReplace(result, '{{GUID_PROJECT}}', projectGuid, [rfReplaceAll]);
  result := StringReplace(result, '{{DCC_REFERENCES}}', references, [rfReplaceAll]);
  result := StringReplace(result, '{{PACKAGE_KIND_FLAG}}', packageKindFlag, [rfReplaceAll]);
end;

end.
