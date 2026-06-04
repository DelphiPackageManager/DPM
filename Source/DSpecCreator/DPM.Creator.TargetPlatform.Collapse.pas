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


unit DPM.Creator.TargetPlatform.Collapse;

// The DSpecCreator UI edits compilers one row at a time. A .dspec targetPlatform entry can
// however cover several compilers at once (compiler from/to range, or compilers list), in which
// case multiple UI rows would share one ISpecTargetPlatform object and editing one would change
// the others. To keep the UI simple we expand every entry into one single-compiler entry on load,
// and collapse equivalent single-compiler entries back into ranges/lists on save.

interface

uses
  System.Generics.Collections,
  Spring.Collections,
  DPM.Core.Spec.Interfaces;

/// <summary> Rewrites spec.TargetPlatforms so there is exactly one single-compiler entry per
///  covered compiler. Each expanded entry is a clone of the source (platforms, template and
///  variables copied). The source entry's comments are carried onto the first expanded compiler
///  only, so a comment that sat above a range survives the round-trip without being duplicated. </summary>
procedure ExpandTargetPlatforms(const spec : IPackageSpec);

/// <summary> Produces a new list where single-compiler entries that share identical platforms,
///  template and variables are merged: maximal contiguous runs become compiler from/to ranges,
///  remaining non-contiguous singletons (2 or more) become a compilers list, and a lone compiler
///  stays a single compiler entry. The supplied spec is not modified. </summary>
function CollapseTargetPlatforms(const spec : IPackageSpec) : IList<ISpecTargetPlatform>;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Defaults,
  DPM.Core.Types,
  DPM.Core.Spec.TargetPlatform;

function CompilerComparer : IComparer<TCompilerVersion>;
begin
  result := TComparer<TCompilerVersion>.Construct(
    function(const left, right : TCompilerVersion) : integer
    begin
      result := Ord(left) - Ord(right);
    end);
end;

function VariablesEqual(const left, right : IVariables) : boolean;
var
  pair : TPair<string,string>;
  otherValue : string;
begin
  if left.Count <> right.Count then
    exit(false);
  for pair in left do
  begin
    if not right.TryGetValue(pair.Key, otherValue) then
      exit(false);
    if otherValue <> pair.Value then
      exit(false);
  end;
  result := true;
end;

//Two single-compiler entries can be merged when everything except the compiler matches.
function ConfigEqual(const left, right : ISpecTargetPlatform) : boolean;
begin
  result := (left.Platforms = right.Platforms) and
            SameText(left.TemplateName, right.TemplateName) and
            VariablesEqual(left.Variables, right.Variables);
end;

//Resets a cloned entry to a single-compiler entry (clone copies the source's compiler fields,
//which may be a range or list - clear them so only Compiler is set).
procedure SetSingleCompiler(const entry : ISpecTargetPlatform; const compiler : TCompilerVersion);
begin
  entry.Compiler := compiler;
  entry.MinCompiler := TCompilerVersion.UnknownVersion;
  entry.MaxCompiler := TCompilerVersion.UnknownVersion;
  entry.Compilers := nil;
end;

procedure ExpandTargetPlatforms(const spec : IPackageSpec);
var
  i : integer;
  source : ISpecTargetPlatform;
  compilers : TCompilerVersions;
  c : TCompilerVersion;
  isFirst : boolean;
  expanded : ISpecTargetPlatform;
  newList : IList<ISpecTargetPlatform>;
begin
  if (spec = nil) or (spec.TargetPlatforms = nil) then
    exit;

  newList := TCollections.CreateList<ISpecTargetPlatform>;
  for i := 0 to spec.TargetPlatforms.Count - 1 do
  begin
    source := spec.TargetPlatforms[i];
    if source = nil then
      continue;
    compilers := ExpandedCompilersOf(source);
    isFirst := true;
    for c := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if c = TCompilerVersion.UnknownVersion then
        continue;
      if not (c in compilers) then
        continue;
      expanded := source.Clone;
      SetSingleCompiler(expanded, c);
      //Clone does not copy comments - put the source comment on the first expanded compiler only.
      if isFirst and source.HasComments then
        expanded.Comments := source.Comments;
      isFirst := false;
      newList.Add(expanded);
    end;
  end;

  newList.Sort(TComparer<ISpecTargetPlatform>.Construct(
    function(const left, right : ISpecTargetPlatform) : integer
    begin
      result := Ord(left.Compiler) - Ord(right.Compiler);
    end));

  spec.TargetPlatforms.Clear;
  spec.TargetPlatforms.AddRange(newList);
end;

//Returns the comments to use for an emitted entry: the comments of the first member compiler
//whose expanded entry carried any (nil when none did).
function CommentsForRun(const map : IDictionary<TCompilerVersion, ISpecTargetPlatform>; const members : IList<TCompilerVersion>) : TStrings;
var
  c : TCompilerVersion;
  entry : ISpecTargetPlatform;
begin
  result := nil;
  for c in members do
  begin
    if map.TryGetValue(c, entry) and entry.HasComments then
      exit(entry.Comments);
  end;
end;

function CollapseTargetPlatforms(const spec : IPackageSpec) : IList<ISpecTargetPlatform>;
var
  i : integer;
  source : ISpecTargetPlatform;
  c : TCompilerVersion;
  map : IDictionary<TCompilerVersion, ISpecTargetPlatform>;
  allCompilers : IList<TCompilerVersion>;
  reps : IList<ISpecTargetPlatform>;
  buckets : IList<IList<TCompilerVersion>>;
  found : boolean;
  g : integer;
  rep : ISpecTargetPlatform;
  comps : IList<TCompilerVersion>;
  runStart : integer;
  runEnd : integer;
  k : integer;
  run : IList<TCompilerVersion>;
  singletons : IList<TCompilerVersion>;

  procedure EmitEntry(const members : IList<TCompilerVersion>; const isRange : boolean);
  var
    entry : ISpecTargetPlatform;
    comments : TStrings;
    arr : TArray<TCompilerVersion>;
    k : integer;
  begin
    entry := rep.Clone;
    if isRange then
    begin
      entry.Compiler := TCompilerVersion.UnknownVersion;
      entry.MinCompiler := members[0];
      entry.MaxCompiler := members[members.Count - 1];
      entry.Compilers := nil;
    end
    else if members.Count = 1 then
      SetSingleCompiler(entry, members[0])
    else
    begin
      entry.Compiler := TCompilerVersion.UnknownVersion;
      entry.MinCompiler := TCompilerVersion.UnknownVersion;
      entry.MaxCompiler := TCompilerVersion.UnknownVersion;
      SetLength(arr, members.Count);
      for k := 0 to members.Count - 1 do
        arr[k] := members[k];
      entry.Compilers := arr;
    end;
    comments := CommentsForRun(map, members);
    if comments <> nil then
      entry.Comments := comments;
    result.Add(entry);
  end;

begin
  result := TCollections.CreateList<ISpecTargetPlatform>;
  if (spec = nil) or (spec.TargetPlatforms = nil) then
    exit;

  //Map every covered compiler to its entry, and collect the full sorted compiler list.
  map := TCollections.CreateDictionary<TCompilerVersion, ISpecTargetPlatform>;
  allCompilers := TCollections.CreateList<TCompilerVersion>;
  for i := 0 to spec.TargetPlatforms.Count - 1 do
  begin
    source := spec.TargetPlatforms[i];
    if source = nil then
      continue;
    for c := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if c = TCompilerVersion.UnknownVersion then
        continue;
      if not source.IsForCompiler(c) then
        continue;
      if map.ContainsKey(c) then
        continue;
      map[c] := source;
      allCompilers.Add(c);
    end;
  end;
  allCompilers.Sort(CompilerComparer);

  //Group compilers that share platforms/template/variables, preserving ascending order in each group.
  reps := TCollections.CreateList<ISpecTargetPlatform>;
  buckets := TCollections.CreateList<IList<TCompilerVersion>>;
  for c in allCompilers do
  begin
    source := map[c];
    found := false;
    for g := 0 to reps.Count - 1 do
    begin
      if ConfigEqual(reps[g], source) then
      begin
        buckets[g].Add(c);
        found := true;
        break;
      end;
    end;
    if not found then
    begin
      reps.Add(source);
      comps := TCollections.CreateList<TCompilerVersion>;
      comps.Add(c);
      buckets.Add(comps);
    end;
  end;

  //Emit each group: contiguous runs of 2+ become ranges, leftover singletons become a list (2+) or
  //a single compiler entry. Emission order does not matter - the writer sorts by min compiler.
  for g := 0 to reps.Count - 1 do
  begin
    rep := reps[g];
    comps := buckets[g];
    singletons := TCollections.CreateList<TCompilerVersion>;

    runStart := 0;
    while runStart < comps.Count do
    begin
      runEnd := runStart;
      while (runEnd + 1 < comps.Count) and (Ord(comps[runEnd + 1]) = Ord(comps[runEnd]) + 1) do
        Inc(runEnd);

      if runEnd > runStart then
      begin
        run := TCollections.CreateList<TCompilerVersion>;
        for k := runStart to runEnd do
          run.Add(comps[k]);
        EmitEntry(run, true);
      end
      else
        singletons.Add(comps[runStart]);

      runStart := runEnd + 1;
    end;

    //EmitEntry with isRange=false produces a single compiler entry for one member, or a
    //compilers list for several.
    if singletons.Count >= 1 then
      EmitEntry(singletons, false);
  end;

  //Emit order above follows grouping, not compiler order - sort so the output (and the
  //modified-check comparison) is canonical, matching what the YAML writer produces.
  result.Sort(TComparer<ISpecTargetPlatform>.Construct(
    function(const left, right : ISpecTargetPlatform) : integer
    begin
      result := Ord(MinCompilerOf(left)) - Ord(MinCompilerOf(right));
    end));
end;

end.
