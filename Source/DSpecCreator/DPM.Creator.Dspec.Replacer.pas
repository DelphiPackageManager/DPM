unit DPM.Creator.Dspec.Replacer;

interface

uses
  System.SysUtils,
  System.RegularExpressions,
  DPM.Core.Types,
//  DPM.dspec.format,
  DPM.Core.Spec.Interfaces
  ;

type
  TClassReplacer = class
  private
    Fspec : IPackageSpec;
    FCompiler : TCompilerVersion;
    function matcher(const Match: TMatch): String;
    function Replace(inputStr: string): string;
    constructor Create(compiler: TCompilerVersion; structure : IPackageSpec);
  public
    class function ReplaceVars(inputStr: String; compiler: TCompilerVersion; structure: IPackageSpec): string;
  end;


implementation

{ TClassReplacer }

constructor TClassReplacer.Create(compiler: TCompilerVersion; structure : IPackageSpec);
begin
  FCompiler := compiler;
  Fspec := structure;
end;

function TClassReplacer.matcher(const Match: TMatch): String;
begin
  if SameText(Match.Groups[1].Value, 'compiler') then
    Exit(CompilerToString(FCompiler))
  else if SameText(Match.Groups[1].Value, 'compilerNoPoint') then
    Exit(CompilerToStringNoPoint(FCompiler))
  else if SameText(Match.Groups[1].Value, 'compilerCodeName') then
    Exit(CompilerCodeName(FCompiler))
  else if SameText(Match.Groups[1].Value, 'compilerWithCodeName') then
    Exit(CompilerWithCodeName(FCompiler))
  else if SameText(Match.Groups[1].Value, 'compilerVersion') then
    Exit(CompilerToCompilerVersionIntStr(FCompiler))
  else if SameText(Match.Groups[1].Value, 'libSuffix') then
    Exit(CompilerToLibSuffix(FCompiler))
  else if SameText(Match.Groups[1].Value, 'bdsVersion') then
    Exit(CompilerToBDSVersion(FCompiler))
  else if SameText(Match.Groups[1].Value, 'id') then
    Exit(FSpec.metadata.id)
  else if SameText(Match.Groups[1].Value, 'version') then
    Exit(FSpec.metadata.version.ToString)
  else if SameText(Match.Groups[1].Value, 'author') then
    Exit(FSpec.metadata.authors)
  else if SameText(Match.Groups[1].Value, 'title') then
    Exit(FSpec.metadata.id)
  else if SameText(Match.Groups[1].Value, 'description') then
    Exit(FSpec.metadata.description)
  else if SameText(Match.Groups[1].Value, 'copyright') then
    Exit(FSpec.metadata.copyright)
  else
    Exit(Match.Value);  // In case of no match, return the original placeholder
end;

function TClassReplacer.Replace(inputStr: string): string;
begin
  Result := TRegEx.Replace(inputStr, '\$(.*?)\$', matcher);
end;

class function TClassReplacer.ReplaceVars(inputStr: String; compiler: TCompilerVersion; structure: IPackageSpec): string;
var
  replacer : TClassReplacer;
begin
  replacer := TClassReplacer.Create(compiler, structure);
  try
    Result := replacer.Replace(inputStr);
  finally
    FreeAndNil(replacer);
  end;
end;


end.
