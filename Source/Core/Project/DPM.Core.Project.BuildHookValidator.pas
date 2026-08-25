{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
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

unit DPM.Core.Project.BuildHookValidator;

// A package's .dproj is whatever its author shipped, and DPM hands it to msbuild during install.
// TMSBuildCompiler already passes the documented IDE hooks (PreBuildEvent, PostBuildEvent,
// CustomToolCommand ...) empty as global properties so they cannot fire - see GetMSBuildParameters.
// That closes the hooks the IDE exposes, but a .dproj is still an msbuild file : a <Target> hung
// off Build with AfterTargets, a <UsingTask> loading an assembly, or an <Import> of a .targets
// file that ships inside the package will all execute, and no command line switch prevents it.
//
// This scans a project file for those constructs. It is an allow list rather than a deny list,
// because the shape of an IDE authored dproj is narrow and known : across a fully populated
// package cache (749 package dprojs, XE2 through 13) not one declared a <Target> or a <UsingTask>,
// and only four distinct <Import> values occur - the three the IDE writes plus DPM's own
// copylocal import.
//
// Findings are split by whether the construct can still execute :
//   blocking - <Target>, <UsingTask>, an unrecognised <Import>. Nothing neuters these, so a
//              package carrying one must not be built.
//   advisory - the properties TMSBuildCompiler blanks. They cannot run, but shipping one means
//              the author expected a step to happen that never will, which is worth saying.
//
// Used at pack time (TPackageWriter - the author's own diagnostic) and at install time
// (TPackageInstaller - the control that actually matters, since nothing forces an attacker to
// produce their package with `dpm pack`).

interface

uses
  Spring.Collections;

type
  TBuildHookValidator = class
  private
    class function ScanProjectFile(const filename : string; const blocking : IList<string>;
                                   const advisory : IList<string>) : boolean;
  public
    /// <summary>
    /// True for the <Import Project=".."> values the Delphi IDE and DPM itself write into a
    /// .dproj. Anything else is the author's own addition and can pull in arbitrary targets.
    /// </summary>
    class function IsAllowedImport(const importProject : string) : boolean;

    /// <summary>
    /// Scans projectFile - and its sibling .deployproj when one exists - adding a short
    /// description of each construct found to blocking or advisory.
    /// Returns false when a file that had to be inspected could not be read or parsed, leaving
    /// the lists as they were. Callers decide what an uninspectable project means : the pack
    /// path skips it (the dproj may not have been generated yet), the install path refuses it.
    /// </summary>
    class function TryScanFile(const projectFile : string; const blocking : IList<string>;
                               const advisory : IList<string>) : boolean;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  DPM.Core.MSXML,
  DPM.Core.Project.CopyLocalTargets,
  DPM.Core.Utils.XML;

const
  //Namespace agnostic on purpose. A dproj normally declares the msbuild namespace, but nothing
  //makes msbuild require it - matching on a namespace prefix would let a dproj that simply omits
  //the xmlns walk straight past the scan.
  cTargetXPath = '//*[local-name()=''Target'']';
  cUsingTaskXPath = '//*[local-name()=''UsingTask'']';
  cImportXPath = '//*[local-name()=''Import'']';
  cPropertyXPathFmt = '//*[local-name()=''PropertyGroup'']/*[local-name()=''%s'']';

  //The properties TMSBuildCompiler.GetMSBuildParameters passes empty on the msbuild command line.
  //Keep the two lists in step - a property dropped there has to move from advisory to blocking.
  cNeuteredProperties : array[0..5] of string = (
    'PreBuildEvent',
    'PostBuildEvent',
    'PreLinkEvent',
    'CustomToolCommand',
    '_PreCompileTargets',
    '_PostCompileTargets');

  //Import allow list.
  cCodeGearTargetsPrefix = '$(BDS)\Bin\CodeGear.';   //+ <product>.Targets, no further path segment
  cTargetsSuffix = '.targets';
  cUserToolsPrefix = '$(APPDATA)\';                  //+ ..\UserTools.proj, the IDE's own tool config
  cUserToolsSuffix = '\UserTools.proj';
  cDeployProj = '$(MSBuildProjectName).deployproj';  //IDE deployment manifest, scanned separately
  //DPM's own import - taken from the same constant TProjectEditor.EnsureCopyLocalImport writes,
  //so this can never drift out of step with it.
  cDpmCopyLocal = cCopyLocalImportProject;

function AttributeText(const node : IXMLDOMNode; const name : string) : string;
var
  element : IXMLDOMElement;
  value : OleVariant;
begin
  result := '';
  if not Supports(node, IXMLDOMElement, element) then
    exit;
  value := element.getAttribute(name);
  //getAttribute returns Null when the attribute is absent; VarToStr maps that to ''.
  if not VarIsNull(value) then
    result := Trim(VarToStr(value));
end;

function NameOrPlaceholder(const value : string) : string;
begin
  if value <> '' then
    result := value
  else
    result := '<unnamed>';
end;

{ TBuildHookValidator }

class function TBuildHookValidator.IsAllowedImport(const importProject : string) : boolean;
var
  value : string;
  tail : string;
begin
  result := false;
  value := Trim(importProject);
  if value = '' then
    exit;

  //$(BDS)\Bin\CodeGear.<product>.Targets - the IDE's own targets. The tail check keeps
  //$(BDS)\Bin\sub\CodeGear.Delphi.Targets out : only a file sitting directly in Bin qualifies.
  if StartsText(cCodeGearTargetsPrefix, value) and EndsText(cTargetsSuffix, value) then
  begin
    tail := Copy(value, Length(cCodeGearTargetsPrefix) + 1, MaxInt);
    if Pos('\', tail) = 0 then
      exit(true);
  end;

  //The IDE's per user custom build tool config. It lives on the consumer's machine rather than
  //in the package, and the tool commands it defines are neutered by CustomToolCommand= anyway.
  if StartsText(cUserToolsPrefix, value) and EndsText(cUserToolsSuffix, value) then
    exit(true);

  //Written into every dproj by the IDE. Relative, so the file itself can ship inside a package -
  //TryScanFile scans it under these same rules rather than trusting it.
  if SameText(cDeployProj, value) then
    exit(true);

  //DPM's own copylocal import. Authors who use DPM sometimes ship a dproj still carrying it. It
  //defines a target that execs dpm.exe, so it is only safe because TMSBuildCompiler passes
  //DPMCopyLocalDisable=true, which is the off switch the generated targets file already has.
  if SameText(cDpmCopyLocal, value) then
    exit(true);
end;

class function TBuildHookValidator.ScanProjectFile(const filename : string; const blocking : IList<string>;
                                                   const advisory : IList<string>) : boolean;
var
  doc : IXMLDOMDocument;
  nodes : IXMLDOMNodeList;
  i : integer;
  displayName : string;
  importProject : string;
  taskName : string;
begin
  result := false;
  if not FileExists(filename) then
    exit;

  displayName := ExtractFileName(filename);
  doc := CoDOMDocument60.Create;
  try
    doc.async := false;
    doc.validateOnParse := false;
    //Nothing in a dproj needs an external entity, and this file is untrusted input.
    doc.resolveExternals := false;
    if not TXMLUtils.LoadXMLFromFile(doc, filename) then
      exit;
    //Load can report success for a document the parser gave up on, so ask the parser directly.
    if doc.parseError.errorCode <> 0 then
      exit;
    (doc as IXMLDOMDocument2).setProperty('SelectionLanguage', 'XPath');
    if (doc.documentElement = nil) or (not SameText(doc.documentElement.baseName, 'Project')) then
      exit;
  except
    //An unreadable or locked file is 'could not inspect', not 'clean'.
    on e : Exception do
      exit;
  end;

  result := true;

  //--- targets. The IDE never writes one. Anything here can be hung off the build with
  //AfterTargets/BeforeTargets, or spliced into a DependsOn list, and will then run tasks.
  nodes := doc.selectNodes(cTargetXPath);
  for i := 0 to nodes.length - 1 do
    blocking.Add(displayName + ' declares msbuild target [' +
                 NameOrPlaceholder(AttributeText(nodes.item[i], 'Name')) + ']');

  //--- custom tasks. Loads and runs an arbitrary assembly inside the msbuild process.
  nodes := doc.selectNodes(cUsingTaskXPath);
  for i := 0 to nodes.length - 1 do
  begin
    taskName := AttributeText(nodes.item[i], 'TaskName');
    if taskName = '' then
      taskName := AttributeText(nodes.item[i], 'AssemblyFile');
    blocking.Add(displayName + ' declares msbuild task [' + NameOrPlaceholder(taskName) + ']');
  end;

  //--- imports. An import is a target injection point, so only the known ones pass.
  nodes := doc.selectNodes(cImportXPath);
  for i := 0 to nodes.length - 1 do
  begin
    importProject := AttributeText(nodes.item[i], 'Project');
    if not IsAllowedImport(importProject) then
      blocking.Add(displayName + ' imports [' + NameOrPlaceholder(importProject) + ']');
  end;

  //--- the properties the compiler already blanks.
  for i := Low(cNeuteredProperties) to High(cNeuteredProperties) do
  begin
    nodes := doc.selectNodes(Format(cPropertyXPathFmt, [cNeuteredProperties[i]]));
    if nodes.length = 0 then
      continue;
    //The IDE writes empty elements into projects that never had the event set - those say nothing
    //about intent, so only a non blank value is worth reporting.
    if Trim(nodes.item[0].text) = '' then
      continue;
    advisory.Add(displayName + ' sets [' + cNeuteredProperties[i] + ']');
  end;
end;

class function TBuildHookValidator.TryScanFile(const projectFile : string; const blocking : IList<string>;
                                               const advisory : IList<string>) : boolean;
var
  deployProj : string;
begin
  result := ScanProjectFile(projectFile, blocking, advisory);
  if not result then
    exit;

  //$(MSBuildProjectName).deployproj is allow listed because the IDE writes the import into every
  //dproj, but the path is relative - the file it resolves to can ship inside the package. Scan it
  //under the same rules. Not having one is the normal case and says nothing.
  deployProj := ChangeFileExt(projectFile, '.deployproj');
  if FileExists(deployProj) then
    result := ScanProjectFile(deployProj, blocking, advisory);
end;

end.
