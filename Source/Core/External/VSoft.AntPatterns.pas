unit VSoft.AntPatterns;

interface

uses
  System.SysUtils,
  Generics.Collections;

type
  IFileSystemPattern = interface
  ['{A7FC46D9-2FCE-4AF3-9D3B-82666C5C53B2}']
    function GetDirectory : string;
    function GetFileMask  : string;
    property Directory  : string read GetDirectory;
    property FileMask   : string read GetFileMask;
  end;


  TFileSystemPattern = class(TInterfacedObject, IFileSystemPattern)
  private
    FDirectory  : string;
    FFileMask   : string;
  protected
    function GetDirectory : string;
    function GetFileMask  : string;
  public
    constructor Create(const directory : string; const fileMask : string);
    property Directory  : string read GetDirectory;
    property FileMask   : string read GetFileMask;
  end;

  TWalkerFunc = reference to function (const path: string; const isDirectory: boolean): boolean;

  IAntPattern = interface
  ['{8271F607-C4CF-4E8E-8C73-1E44827C3512}']

 		/// <summary>
		/// Expands an 'Ant' style pattern into a series of FileSystem patterns
    /// that can easily be used for filecopy etc
		/// </summary>
		/// <param name="antPattern"></param>
		/// <returns>TArray of IFileSystemPattern</returns>
    function Expand(antPattern : string) : TArray<IFileSystemPattern>;
    function ConvertAntToRegexString(const antPattern : string) : string;
  end;

  TAntPattern = class(TInterfacedObject, IAntPattern)
  private
    FRootDirectory : string;

  protected
    function IsRooted(const path : string) : boolean;
    function Combine(const root : string; pattern : string) : string;

    function NormalizeDirectorySeparators(const path : string) : string;


    /// <summary>
		/// Walk
		/// </summary>
		/// <param name="path"></param>
		/// <param name="walker">A func that is called for each file, the result is used to determine whether to continue checking files in the current dir.</param>
    procedure Walk(const path : string; const walker : TWalkerFunc);virtual;

    //IAntPattern
    function ConvertAntToRegexString(const antPattern : string) : string;
    function Expand(antPattern : string) : TArray<IFileSystemPattern>;
  public
    constructor Create(const rootDirectory : string);

  end;

  //TODO : Not sure if this belongs here
  /// <summary>
  /// Takes a path like 'c:\temp\foo\..\bar\test.txt' and
  ///  converts it to   'c:\temp\bar\test.txt'
  /// </summary>
  function CompressRelativePath(const basePath : string; path : string) : string;



implementation

uses
  System.Types,
  System.IOUtils,
  System.SyncObjs,
  System.RegularExpressions;
var
  antPatternRegexCache : TDictionary<string,string>;

  //lazy create thread safe
  function InitCache: TDictionary<string,string>;
  var
   newObject: TDictionary<string,string>;
  begin
    if (antPatternRegexCache = nil) then
    begin
      //The object doesn't exist yet. Create one.
      newObject := TDictionary<string,string>.Create;

      //It's possible another thread also created one.
      //Only one of us will be able to set the AObject singleton variable
      if TInterlocked.CompareExchange(Pointer(antPatternRegexCache), Pointer(newObject), nil) <> nil then
      begin
         //The other beat us. Destroy our newly created object and use theirs.
         newObject.Free;
      end;
    end;

    Result := antPatternRegexCache;
  end;

  procedure AddToCache(const antPattern : string; const regex : string);
  begin
    InitCache;
    MonitorEnter(antPatternRegexCache);
    try
      antPatternRegexCache.AddOrSetValue(antPattern, regex);
    finally
      MonitorExit(antPatternRegexCache);
    end;
  end;

  function GetRegexFromCache(const antPattern : string) : string;
  begin
    result := '';
    if antPatternRegexCache = nil then
      exit;
    antPatternRegexCache.TryGetValue(antPattern, result);
  end;



{ TFileSystemPattern }

constructor TFileSystemPattern.Create(const directory, fileMask: string);
begin
  FDirectory := directory;
  FFileMask  := fileMask;
end;

function TFileSystemPattern.GetDirectory: string;
begin
  result := FDirectory;
end;

function TFileSystemPattern.GetFileMask: string;
begin
  result := FFileMask;
end;

{ TAntPattern }

function TAntPattern.Combine(const root: string; pattern: string): string;
begin
  if pattern.StartsWith(PathDelim) then
    Delete(pattern,1,1);
  result := IncludeTrailingPathDelimiter(FRootDirectory) + pattern;
end;


function TAntPattern.ConvertAntToRegexString(const antPattern: string): string;
begin
    result := GetRegexFromCache(antPattern);
    if result <> '' then
      exit;

    result := TRegEx.Escape(antPattern.Trim);

    // Make all path delimiters the same (to simplify following expressions)
    result := TRegEx.Replace(result, '(\\\\|/)', '/');

    // start ** matches. e.g. any folder (recursive match)

    // ** at start or as complete pattern e.g. '**'
    result := TRegEx.Replace(result, '^\\\*\\\*($|/)', '.*');

    // ** end of pattern e.g. 'blah*/**' or 'blah*/**' matches blah1/a.txt and blah2/folder/b.txt
    result := TRegEx.Replace(result, '(?<c>[^/])\\\*/\\\*\\\*/?$', '${c}.*');

    // ** end of pattern e.g. 'blah/**' or 'blah/**/' matches blah/a.txt, blah/folder/b.txt and blah
    result := TRegEx.Replace(result, '/\\\*\\\*/?$', '(?:/.+)*');

    // ** end of delimited pattern e.g. 'blah/**;' or 'blah/**/;' matches blah/a.txt;, blah/folder/b.txt; and blah;
    result := TRegEx.Replace(result, '/\\\*\\\*/?$', '(?:/[^;]+)*;');

    // ** middle of pattern e.g. 'blah*/**/*b.txt' matches blah1/ab.txt and blah2/folder/b.txt
    //                          'blah*/**/a.txt' matches blah1/a.txt and blah2/folder/a.txt
    result := TRegEx.Replace(result, '(?<!(\\\*))\\\*/\\\*\\\*/(\\\*)?(?!(\\\*))', '[^/]*/.*');

    //** middle of pattern e.g. 'blah/**/*b.txt' matches blah/ab.txt and blah/folder/b.txt
    result := TRegEx.Replace(result, '(?<!(\\\*))/\\\*\\\*/(\\\*)(?!(\\\*))', '/.*');

    //** middle of pattern e.g. 'blah/**/b.txt' matches blah/b.txt and blah/folder/b.txt
    result := TRegEx.Replace(result, '(?<!(\\\*))/\\\*\\\*/(?!(\\\*))', '/(?:.*/)*');

    //** middle of pattern e.g. 'blah/**/a.txt' matches blah/a.txt and blah/folder/a.txt
    result := TRegEx.Replace(result, '(?<c>/?)\\\*\\\*', '${c}.*');

    // end of matches for **

    // Any path delimiter at start is optional
    result := TRegEx.Replace(result, '^/', '/?');

    // Make all path delimiters ambiguous (/ or \)
    result := TRegEx.Replace(result, '/', '(?:\\\\|/)');

    // Make all path delimiters ambiguous (/ or \) again in character class matching
    result := TRegEx.Replace(result, '\[\^\\\\\]', '[^\\/]');

    // * matches zero or more characters which are not a path delimiter
    result := result.Replace('\*', '[^\\/]*');

    // ? matches anything but a path delimiter
    result := result.Replace('\?', '[^\\/]');

    // Semicolons become |-delimited OR groups
    if result.Contains(';') then
    begin
      result := result.Replace(';', ')|(?:');
      result := '(?:' + result + ')';
    end;

    // Any match must take the entire string and may optionally start with /
    result := '^(?:' + result + ')$';

    AddToCache(antPattern, result);

end;

constructor TAntPattern.Create(const rootDirectory: string);
begin
  FRootDirectory := NormalizeDirectorySeparators(rootDirectory);
end;

function TAntPattern.Expand(antPattern: string): TArray<IFileSystemPattern>;
var
  firstWildcard : integer;
  directory     : string;
  mask          : string;
  pattern       : string;
  root         : string;
  newPattern    : IFileSystemPattern;
  regExPattern  : string;
  lastSepBeforeWildcard : integer;
  regEx : TRegEx;
  list : TList<IFileSystemPattern>;
begin
  list := TList<IFileSystemPattern>.Create;
  try
    antPattern := NormalizeDirectorySeparators(antPattern);

    if not IsRooted(antPattern) then //cannot use TPath.IsPathRooted as it matched \xxx
      antPattern := Combine(ExcludeTrailingPathDelimiter(FRootDirectory),antPattern); //TPath.Combine fails
    firstWildcard := antPattern.IndexOfAny(['?', '*' ]);

    if firstWildcard = -1 then
    begin
      directory := ExtractFilePath(antPattern);
      mask := ExtractFileName(antPattern);
      // This is for when 'S:\' is passed in. Default it to '*' wildcard
      if mask = '' then
        mask := '*';
      newPattern := TFileSystemPattern.Create(CompressRelativePath(FRootDirectory, IncludeTrailingPathDelimiter(directory)), mask);
      list.Add(newPattern);
      exit;
    end;

    lastSepBeforeWildcard := antPattern.LastIndexOf(PathDelim , firstWildcard);
    // C:\Foo\Bar\Go?\**\*.txt
    root := antPattern.Substring(0, lastSepBeforeWildcard + 1 );  // C:\Foo\Bar\
    pattern := antPattern.Substring(lastSepBeforeWildcard + 1);   // Go?\**\*.txt

    if pattern = '' then // C:\Foo\bar\ == all files recursively in C:\Foo\bar\
      pattern := '**';

    regExPattern := ConvertAntToRegexString(pattern);
    if regExPattern = '' then
      exit;

    regEx := TRegEx.Create(regExPattern,[TRegExOption.roIgnoreCase]);

    Walk(root,
      function(const path : string; const isDirectory : boolean) : boolean
      var
        subPath : string;
      begin
        result := false;
        if not path.StartsWith(root) then
          exit;

        subPath := path.Substring(root.Length);

        //-----------------------------------------------------------
        //this is a work around for an issue with TRegEx where it does not
        //match empty strings in earlier versions of Delphi. Not sure when
        //it was fixed but regex matches empty strings ok in 10.3.2
        //once we find out we can ifdef this out for newer versions
        if ((subPath = '') and (path = root) and isDirectory) then
          subPath := '*';
        //-----------------------------------------------------------

        if regEx.IsMatch(subPath) then
        begin
          if isDirectory then
          begin
            newPattern := TFileSystemPattern.Create(CompressRelativePath(FRootDirectory, IncludeTrailingPathDelimiter(path)), '*');
            list.Add(newPattern);
            exit(true);
          end;
          newPattern := TFileSystemPattern.Create(CompressRelativePath(FRootDirectory, IncludeTrailingPathDelimiter(ExtractFilePath(path))), ExtractFileName(path));
          list.Add(newPattern);
        end;

      end);
  finally
    result := list.ToArray();
    list.Free;
  end;

end;


function TAntPattern.IsRooted(const path: string): boolean;
begin
  result := TRegEx.IsMatch(path, '^[a-zA-z]\:\\|\\\\');
end;

function TAntPattern.NormalizeDirectorySeparators(const path: string): string;
begin
  //TODO : Check PathDelim for all platforms;
  {$IFDEF MSWINDOWS}
    result := StringReplace(path, '/', PathDelim, [rfReplaceAll]);
  {$ELSE}
    result := StringReplace(path, '\', PathDelim, [rfReplaceAll]);
  {$ENDIF}
end;

procedure TAntPattern.Walk(const path: string; const walker: TWalkerFunc);
var
  files : TStringDynArray;
  subs  : TStringDynArray;
  fileName : string;
  dir : string;
begin
  if not walker(path, true) then
  begin
    files := [];
    try
      files := TDirectory.GetFiles(path,'*', TSearchOption.soTopDirectoryOnly);
    except
      //TODO : Catch Security, unauth, directory not found, let everything else go
    end;

    for fileName in files do
    begin
      if walker(fileName, false) then
          break;
    end;
    subs := [];

    try
      subs := TDirectory.GetDirectories(path, '*', TSearchOption.soTopDirectoryOnly);
    except
      //TODO : same as for above
    end;

    for dir in subs do
      Walk(dir,walker);

  end;
end;


function CompressRelativePath(const basePath : string; path : string) : string;
var
  stack : TStack<string>;
  segments : TArray<string>;
  segment : string;

begin
  if not TPath.IsPathRooted(path) then
    path := IncludeTrailingPathDelimiter(basePath) + path
  else if not path.StartsWith(basePath) then
    exit(path); //should probably except ?

  segments := path.Split([PathDelim]);
  stack := TStack<string>.Create;
  try
    for segment in segments do
    begin
      if segment = '..' then
      begin
        if stack.Count > 0 then
          stack.Pop //up one
        else
          raise Exception.Create('Relative path goes below base path');
      end
      else if segment <> '.' then
        stack.Push(segment);
    end;
    result := '';
    while stack.Count > 0 do
    begin
      if result <> '' then
        result := stack.Pop + PathDelim + result
      else
        result := stack.Pop;
    end;
    if path.EndsWith(PathDelim) then
      result := IncludeTrailingPathDelimiter(result);
  finally
    stack.Free;
  end;
end;



end.
