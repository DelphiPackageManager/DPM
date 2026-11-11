unit DPM.Core.Repository.Directory.Index;

// A folder source keeps a .sha256 and a .dspec.yaml beside each package, and on a network share reading those two
// small files for every package is most of what a search costs (35-85ms a package once the share has gone idle).
// This is a per-user copy of what they held, for the reads that are only shown - the search, the installed list, a
// package's details - so a search only reads the share for what has changed. Installing never uses it.
//
// What makes it safe : an entry stands for three files - the package and its two sidecars - and records each as the
// folder listing saw it (there or not, its size, its last write time). It is used only when the listing done for the
// query sees all three exactly as recorded, so anything written to any of them since makes it be read again. The
// listing returns those values anyway - nothing more is asked of the share. They are only ever compared for equality,
// never as newer or older, so the clocks of this machine and the server don't matter. What is kept is the files' text,
// parsed afresh each time it is used : nothing about a package is serialised, so a later DPM that reads a spec
// differently reads a recorded one the same way.
//
// The index file is a cache. Any problem with it - missing, damaged, for another folder or compiler, unwritable - just
// means reading the share, as without it. Processes don't coordinate their writes : each entry carries the stamps it
// is valid for, so whichever writes last, what it left out only gets read again.

interface

uses
  System.SyncObjs,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging;

const
  //Bumped whenever what an entry records, or how it is worked out, changes. It is part of the file name, so two DPM
  //builds with different formats (an IDE plugin and a dpm.exe, say) each keep their own file.
  cFolderIndexFormat = 1;
  //an index file bigger than this isn't one of ours.
  cFolderIndexMaxFileSize = 32 * 1024 * 1024;
  //An entry not used for this many days is dropped when the index is saved - a version that is no longer the latest,
  //installed or looked at stays on the share, but not in the index.
  cFolderIndexExpiryDays = 60;
  //Beyond this many entries, the least recently used go when the index is saved. A search uses an entry for the
  //latest version of every package in the folder, so this has to be more than a folder holds - it is a backstop for
  //one with very many versions (a CI feed), not the usual trim.
  cFolderIndexMaxEntries = 5000;

type
  //A file as a folder listing saw it.
  TFileStamp = record
    Present : boolean;
    Size : Int64;
    //the raw FILETIME (UTC, 100ns units) - compared, never converted.
    LastWrite : Int64;
    function SameAs(const other : TFileStamp) : boolean;
  end;

  //A package file and its two sidecars - what an index entry stands for.
  TPackageFileStamps = record
    Package : TFileStamp;
    //<package>.dpkg.sha256
    Hash : TFileStamp;
    //<package>.dspec.yaml
    Spec : TFileStamp;
    function SameAs(const other : TPackageFileStamps) : boolean;
  end;

  //What one pass over a folder saw - kept, so each file need not be asked about again.
  IFolderListing = interface
    ['{AA3B3116-D99C-4BD1-97BA-6F2E9F6C8882}']
    function GetPackageFiles : IList<string>;
    function GetComplete : boolean;
    //The stamps of packageFile and its sidecars, as listed. Package.Present is false when the listing didn't include
    //the package - a name worked out rather than listed, say.
    function StampsFor(const packageFile : string) : TPackageFileStamps;
    //The full paths of the package files listed, in the order listed, each once.
    property PackageFiles : IList<string> read GetPackageFiles;
    //False when a listing failed part way (the share went away) - what it holds may be short.
    property Complete : boolean read GetComplete;
  end;

  IFolderSourceIndex = interface
    ['{3EEBE2B2-E852-44D4-AD7D-5F6BCE0130AC}']
    //What was recorded for packageFile, when its stamps are exactly these.
    function TryGet(const packageFile : string; const stamps : TPackageFileStamps; out hash : string; out specText : string) : boolean;
    procedure Put(const packageFile : string; const stamps : TPackageFileStamps; const hash : string; const specText : string);
    procedure Remove(const packageFile : string);
    //Drops the entries of packages the listing didn't see. Only for a listing of every package of the compiler, and
    //does nothing for an incomplete one.
    procedure Prune(const listing : IFolderListing);
    //Writes the index if it has changed. Never raises - a failure is logged, and the index carries on in memory.
    procedure Save;
    function GetCount : integer;
    function GetFileName : string;
    property Count : integer read GetCount;
    property FileName : string read GetFileName;
  end;

  TFolderIndexEntry = record
    Stamps : TPackageFileStamps;
    Hash : string;
    Spec : string;
    //The day it was last used (Trunc(Date)) - only for trimming. It is only ever compared with this machine's own
    //clock, in days, so a clock change can only drop an entry early or keep it longer : one more read, never a wrong one.
    LastUsed : integer;
  end;

  TFolderSourceIndex = class(TInterfacedObject, IFolderSourceIndex)
  private
    FLogger : ILogger;
    FFileName : string;
    FSource : string;
    FCompiler : string;
    FLock : TCriticalSection;
    //saves write one at a time, outside FLock so lookups don't wait on the disk.
    FSaveLock : TCriticalSection;
    FEntries : IDictionary<string, TFolderIndexEntry>;
    FLoaded : boolean;
    //counts changes - a save writes only when there are ones it hasn't written.
    FGeneration : integer;
    FSavedGeneration : integer;
    procedure EnsureLoaded;
    procedure Load;
    procedure Touch(const key : string; var entry : TFolderIndexEntry);
    procedure TrimEntries;
    function BuildJson : string;
    procedure DeleteStaleTempFiles;
  protected
    //today, as LastUsed records it - a test moves it on.
    function CurrentDay : integer; virtual;
    function TryGet(const packageFile : string; const stamps : TPackageFileStamps; out hash : string; out specText : string) : boolean;
    procedure Put(const packageFile : string; const stamps : TPackageFileStamps; const hash : string; const specText : string);
    procedure Remove(const packageFile : string);
    procedure Prune(const listing : IFolderListing);
    procedure Save;
    function GetCount : integer;
    function GetFileName : string;
  public
    //sourceFolder is the folder the source lists - it is normalised here, and checked against the file's.
    constructor Create(const logger : ILogger; const fileName : string; const sourceFolder : string; const compilerVersion : TCompilerVersion);
    destructor Destroy; override;
  end;

//One pass over folder for each of patterns, keeping what the listing says about every file it returns. The package
//files are those whose names end with packageSuffix. Never raises.
function ListFolder(const folder : string; const patterns : array of string; const packageSuffix : string) : IFolderListing;

//folder as an index knows it : the full path, without a trailing delimiter, in lower case.
function NormaliseFolderIndexSource(const folder : string) : string;

//The file, in indexFolder, holding the index of sourceFolder for compilerVersion.
function FolderIndexFileName(const indexFolder : string; const sourceFolder : string; const compilerVersion : TCompilerVersion) : string;

//False when the DPMFOLDERINDEX environment variable turns the index off (off, 0 or false).
function FolderIndexEnabled : boolean;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  JsonDataObjects,
  DPM.Core.Constants,
  DPM.Core.Utils.Files,
  DPM.Core.Utils.Hash,
  DPM.Core.Utils.Json;

const
  //what a killed process leaves behind of a save is cleared once it is this old.
  cStaleTempFileAge = 1 / 24;

type
  TFolderListing = class(TInterfacedObject, IFolderListing)
  private
    FPackageFiles : IList<string>;
    //lower case file name -> stamp
    FFiles : IDictionary<string, TFileStamp>;
    FComplete : boolean;
    function StampOf(const fileName : string) : TFileStamp;
  protected
    function GetPackageFiles : IList<string>;
    function GetComplete : boolean;
    function StampsFor(const packageFile : string) : TPackageFileStamps;
  public
    constructor Create;
  end;

function FileTimeToInt64(const fileTime : TFileTime) : Int64;
begin
  result := (Int64(fileTime.dwHighDateTime) shl 32) or Int64(fileTime.dwLowDateTime);
end;

function IndexKey(const packageFile : string) : string;
begin
  result := LowerCase(ExtractFileName(packageFile));
end;

{ TFileStamp }

function TFileStamp.SameAs(const other : TFileStamp) : boolean;
begin
  result := (Present = other.Present) and (Size = other.Size) and (LastWrite = other.LastWrite);
end;

{ TPackageFileStamps }

function TPackageFileStamps.SameAs(const other : TPackageFileStamps) : boolean;
begin
  result := Package.SameAs(other.Package) and Hash.SameAs(other.Hash) and Spec.SameAs(other.Spec);
end;

{ TFolderListing }

constructor TFolderListing.Create;
begin
  FPackageFiles := TCollections.CreateList<string>;
  FFiles := TCollections.CreateDictionary<string, TFileStamp>;
  FComplete := true;
end;

function TFolderListing.GetComplete : boolean;
begin
  result := FComplete;
end;

function TFolderListing.GetPackageFiles : IList<string>;
begin
  result := FPackageFiles;
end;

function TFolderListing.StampOf(const fileName : string) : TFileStamp;
begin
  if not FFiles.TryGetValue(LowerCase(fileName), result) then
  begin
    result.Present := false;
    result.Size := 0;
    result.LastWrite := 0;
  end;
end;

function TFolderListing.StampsFor(const packageFile : string) : TPackageFileStamps;
var
  name : string;
begin
  name := ExtractFileName(packageFile);
  result.Package := StampOf(name);
  result.Hash := StampOf(name + cPackageHashAlgorithmExt);
  result.Spec := StampOf(ChangeFileExt(name, cLegacyPackageSpecExt));
end;

function ListFolder(const folder : string; const patterns : array of string; const packageSuffix : string) : IFolderListing;
var
  listing : TFolderListing;
  theDir : string;
  i : integer;
  searchRec : TSearchRec;
  findResult : integer;
  key : string;
  stamp : TFileStamp;
begin
  listing := TFolderListing.Create;
  result := listing;
  theDir := IncludeTrailingPathDelimiter(folder);
  for i := 0 to High(patterns) do
  begin
    findResult := FindFirst(theDir + patterns[i], faAnyFile, searchRec);
    if findResult = 0 then
    begin
      try
        repeat
          if (searchRec.Attr and faDirectory) = 0 then
          begin
            key := LowerCase(searchRec.Name);
            //two patterns can both match a file - it is listed once, where it was first seen.
            if not listing.FFiles.ContainsKey(key) then
            begin
              stamp.Present := true;
              stamp.Size := searchRec.Size;
              //the raw time, not TSearchRec.TimeStamp - a conversion to local time could make two writes look the same.
              {$WARN SYMBOL_PLATFORM OFF}
              stamp.LastWrite := FileTimeToInt64(searchRec.FindData.ftLastWriteTime);
              {$WARN SYMBOL_PLATFORM ON}
              listing.FFiles[key] := stamp;
              if EndsText(packageSuffix, searchRec.Name) then
                listing.FPackageFiles.Add(theDir + searchRec.Name);
            end;
          end;
          findResult := FindNext(searchRec);
        until findResult <> 0;
      finally
        System.SysUtils.FindClose(searchRec);
      end;
    end;
    //nothing matching is a whole answer; anything else (the folder or share unreachable, a failure part way) is not.
    if (findResult <> ERROR_NO_MORE_FILES) and (findResult <> ERROR_FILE_NOT_FOUND) then
      listing.FComplete := false;
  end;
end;

function NormaliseFolderIndexSource(const folder : string) : string;
begin
  result := LowerCase(ExcludeTrailingPathDelimiter(ExpandFileName(folder)));
end;

function FolderIndexFileName(const indexFolder : string; const sourceFolder : string; const compilerVersion : TCompilerVersion) : string;
begin
  result := TPath.Combine(indexFolder, Copy(THashSHA256.GetHashStringFromText(NormaliseFolderIndexSource(sourceFolder)), 1, 16) + '-' +
                                       CompilerToString(compilerVersion) + '.v' + IntToStr(cFolderIndexFormat) + '.json');
end;

function FolderIndexEnabled : boolean;
var
  value : string;
begin
  value := Trim(System.SysUtils.GetEnvironmentVariable(cDPMFolderIndexEnvironmentVar));
  result := not (SameText(value, 'off') or SameText(value, '0') or SameText(value, 'false'));
end;

{ TFolderSourceIndex }

constructor TFolderSourceIndex.Create(const logger : ILogger; const fileName : string; const sourceFolder : string; const compilerVersion : TCompilerVersion);
begin
  FLogger := logger;
  FFileName := fileName;
  FSource := NormaliseFolderIndexSource(sourceFolder);
  FCompiler := CompilerToString(compilerVersion);
  FLock := TCriticalSection.Create;
  FSaveLock := TCriticalSection.Create;
  FEntries := TCollections.CreateDictionary<string, TFolderIndexEntry>;
  FLoaded := false;
  FGeneration := 0;
  FSavedGeneration := 0;
end;

destructor TFolderSourceIndex.Destroy;
begin
  FSaveLock.Free;
  FLock.Free;
  inherited;
end;

function TFolderSourceIndex.GetFileName : string;
begin
  result := FFileName;
end;

function TFolderSourceIndex.CurrentDay : integer;
begin
  result := System.Trunc(Date);
end;

//FLock is held. Records that entry was used today - a change, so it is saved, but only once a day.
procedure TFolderSourceIndex.Touch(const key : string; var entry : TFolderIndexEntry);
var
  today : integer;
begin
  today := CurrentDay;
  if entry.LastUsed = today then
    exit;
  entry.LastUsed := today;
  FEntries[key] := entry;
  Inc(FGeneration);
end;

//FLock is held. Drops what hasn't been used for cFolderIndexExpiryDays, then the least recently used beyond
//cFolderIndexMaxEntries. Either only means a read of the share, should one be wanted again.
procedure TFolderSourceIndex.TrimEntries;
var
  today : integer;
  key : string;
  gone : IList<string>;
  keys : IList<string>;
  entries : IDictionary<string, TFolderIndexEntry>;
  i : integer;
begin
  today := CurrentDay;
  gone := TCollections.CreateList<string>;
  for key in FEntries.Keys do
    if today - FEntries[key].LastUsed > cFolderIndexExpiryDays then
      gone.Add(key);
  for key in gone do
    FEntries.Remove(key);

  if FEntries.Count > cFolderIndexMaxEntries then
  begin
    entries := FEntries;
    keys := TCollections.CreateList<string>(FEntries.Keys);
    keys.Sort(
      function(const left, right : string) : integer
      begin
        result := entries[left].LastUsed - entries[right].LastUsed;
      end);
    for i := 0 to keys.Count - cFolderIndexMaxEntries - 1 do
      FEntries.Remove(keys[i]);
  end;
end;

function TFolderSourceIndex.GetCount : integer;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    result := FEntries.Count;
  finally
    FLock.Leave;
  end;
end;

//FLock is held.
procedure TFolderSourceIndex.EnsureLoaded;
begin
  if FLoaded then
    exit;
  FLoaded := true;
  Load;
end;

function TryReadStamp(const item : TJsonObject; const name : string; out stamp : TFileStamp) : boolean;
var
  values : TJsonArray;
begin
  result := false;
  if item.Types[name] <> jdtArray then
    exit;
  values := item.A[name];
  if values.Count <> 3 then
    exit;
  if values.Types[0] <> jdtBool then
    exit;
  if not (values.Types[1] in [jdtInt, jdtLong]) then
    exit;
  if not (values.Types[2] in [jdtInt, jdtLong]) then
    exit;
  stamp.Present := values.B[0];
  stamp.Size := values.L[1];
  stamp.LastWrite := values.L[2];
  result := stamp.Size >= 0;
end;

function TryReadEntry(const item : TJsonObject; const today : integer; out entry : TFolderIndexEntry) : boolean;
begin
  result := false;
  //trimming is all it is for - without one, it counts as used today.
  if item.Types['used'] in [jdtInt, jdtLong] then
    entry.LastUsed := item.L['used']
  else
    entry.LastUsed := today;
  if not TryReadStamp(item, 'dpkg', entry.Stamps.Package) then
    exit;
  if not TryReadStamp(item, 'sha256', entry.Stamps.Hash) then
    exit;
  if not TryReadStamp(item, 'dspec', entry.Stamps.Spec) then
    exit;
  //an entry is only ever made for a package the listing saw.
  if not entry.Stamps.Package.Present then
    exit;
  if item.Types['hash'] <> jdtString then
    exit;
  if item.Types['spec'] <> jdtString then
    exit;
  entry.Hash := item.S['hash'];
  entry.Spec := item.S['spec'];
  result := entry.Spec <> '';
end;

//FLock is held. Anything wrong with the file leaves the index empty - it is rebuilt from the share, and the file
//replaced on the next save.
procedure TFolderSourceIndex.Load;
var
  text : string;
  error : string;
  root : TJsonObject;
  packages : TJsonObject;
  value : PJsonDataValue;
  entry : TFolderIndexEntry;
  i : integer;
  dropped : integer;
  today : integer;
begin
  FEntries.Clear;
  if not FileExists(FFileName) then
    exit;
  try
    if TFileUtils.GetSize(FFileName) > cFolderIndexMaxFileSize then
    begin
      FLogger.Verbose('Ignoring the folder index [' + FFileName + '] : it is too big to be one.');
      exit;
    end;
    text := TFile.ReadAllText(FFileName, TEncoding.UTF8);
  except
    on e : Exception do
    begin
      FLogger.Verbose('Unable to read the folder index [' + FFileName + '] : ' + e.Message);
      exit;
    end;
  end;

  if not TJsonText.TryParseObject(text, root, error) then
  begin
    FLogger.Verbose('Ignoring the folder index [' + FFileName + '] : ' + error);
    exit;
  end;
  try
    if (not (root.Types['format'] in [jdtInt, jdtLong])) or (root.L['format'] <> cFolderIndexFormat) then
    begin
      FLogger.Verbose('Ignoring the folder index [' + FFileName + '] : it is not format ' + IntToStr(cFolderIndexFormat) + '.');
      exit;
    end;
    if (root.Types['source'] <> jdtString) or (root.S['source'] <> FSource) then
    begin
      FLogger.Verbose('Ignoring the folder index [' + FFileName + '] : it is not the index of [' + FSource + '].');
      exit;
    end;
    if (root.Types['compiler'] <> jdtString) or (root.S['compiler'] <> FCompiler) then
    begin
      FLogger.Verbose('Ignoring the folder index [' + FFileName + '] : it is not the index for ' + FCompiler + '.');
      exit;
    end;
    if root.Types['packages'] <> jdtObject then
    begin
      FLogger.Verbose('Ignoring the folder index [' + FFileName + '] : it has no packages.');
      exit;
    end;
    packages := root.O['packages'];
    dropped := 0;
    today := CurrentDay;
    for i := 0 to packages.Count - 1 do
    begin
      value := packages.Items[i];
      if (value.Typ = jdtObject) and TryReadEntry(value.ObjectValue, today, entry) then
        FEntries[LowerCase(packages.Names[i])] := entry
      else
        Inc(dropped);
    end;
    if dropped > 0 then
      FLogger.Verbose('The folder index [' + FFileName + '] had ' + IntToStr(dropped) + ' unreadable entries - they will be read again.');
  finally
    root.Free;
  end;
end;

function TFolderSourceIndex.TryGet(const packageFile : string; const stamps : TPackageFileStamps; out hash : string; out specText : string) : boolean;
var
  key : string;
  entry : TFolderIndexEntry;
begin
  result := false;
  hash := '';
  specText := '';
  key := IndexKey(packageFile);
  FLock.Enter;
  try
    EnsureLoaded;
    if not FEntries.TryGetValue(key, entry) then
      exit;
    if not entry.Stamps.SameAs(stamps) then
      exit;
    hash := entry.Hash;
    specText := entry.Spec;
    Touch(key, entry);
    result := true;
  finally
    FLock.Leave;
  end;
end;

procedure TFolderSourceIndex.Put(const packageFile : string; const stamps : TPackageFileStamps; const hash : string; const specText : string);
var
  key : string;
  entry : TFolderIndexEntry;
begin
  //an entry without a spec isn't one (see TryReadEntry), nor is one for a package the listing didn't see.
  if (specText = '') or (not stamps.Package.Present) then
    exit;
  key := IndexKey(packageFile);
  FLock.Enter;
  try
    EnsureLoaded;
    if FEntries.TryGetValue(key, entry) and entry.Stamps.SameAs(stamps) and (entry.Hash = hash) and (entry.Spec = specText) then
    begin
      Touch(key, entry);
      exit;
    end;
    entry.Stamps := stamps;
    entry.Hash := hash;
    entry.Spec := specText;
    entry.LastUsed := CurrentDay;
    FEntries[key] := entry;
    Inc(FGeneration);
  finally
    FLock.Leave;
  end;
end;

procedure TFolderSourceIndex.Remove(const packageFile : string);
var
  key : string;
begin
  key := IndexKey(packageFile);
  FLock.Enter;
  try
    EnsureLoaded;
    if FEntries.ContainsKey(key) then
    begin
      FEntries.Remove(key);
      Inc(FGeneration);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TFolderSourceIndex.Prune(const listing : IFolderListing);
var
  gone : IList<string>;
  key : string;
begin
  if (listing = nil) or (not listing.Complete) then
    exit;
  gone := TCollections.CreateList<string>;
  FLock.Enter;
  try
    EnsureLoaded;
    for key in FEntries.Keys do
      if not listing.StampsFor(key).Package.Present then
        gone.Add(key);
    for key in gone do
      FEntries.Remove(key);
    if gone.Any then
      Inc(FGeneration);
  finally
    FLock.Leave;
  end;
end;

//FLock is held.
function TFolderSourceIndex.BuildJson : string;
var
  root : TJsonObject;
  packages : TJsonObject;
  item : TJsonObject;
  key : string;
  entry : TFolderIndexEntry;

  procedure AddStamp(const name : string; const stamp : TFileStamp);
  var
    values : TJsonArray;
  begin
    values := item.A[name];
    values.Add(stamp.Present);
    values.Add(stamp.Size);
    values.Add(stamp.LastWrite);
  end;

begin
  root := TJsonObject.Create;
  try
    root.L['format'] := cFolderIndexFormat;
    root.S['source'] := FSource;
    root.S['compiler'] := FCompiler;
    packages := root.O['packages'];
    for key in FEntries.Keys do
    begin
      entry := FEntries[key];
      item := packages.O[key];
      AddStamp('dpkg', entry.Stamps.Package);
      AddStamp('sha256', entry.Stamps.Hash);
      AddStamp('dspec', entry.Stamps.Spec);
      item.S['hash'] := entry.Hash;
      item.S['spec'] := entry.Spec;
      item.L['used'] := entry.LastUsed;
    end;
    result := root.ToJSON(true);
  finally
    root.Free;
  end;
end;

procedure TFolderSourceIndex.Save;
var
  json : string;
  generation : integer;
begin
  FLock.Enter;
  try
    if (not FLoaded) or (FGeneration = FSavedGeneration) then
      exit;
    //trimmed as it is written - what is in memory is only ever as much as the last save let through, and what
    //this query read.
    TrimEntries;
    json := BuildJson;
    generation := FGeneration;
  finally
    FLock.Leave;
  end;

  FSaveLock.Enter;
  try
    //a save that started later may already have written more than this.
    if generation <= FSavedGeneration then
      exit;
    try
      ForceDirectories(ExtractFilePath(FFileName));
      TFileUtils.AtomicWriteAllText(FFileName, json, TEncoding.UTF8);
      DeleteStaleTempFiles;
    except
      on e : Exception do
        FLogger.Verbose('Unable to save the folder index [' + FFileName + '] : ' + e.Message);
    end;
    //not tried again until something changes - a folder that can't be written to won't be by trying each query.
    FLock.Enter;
    try
      if generation > FSavedGeneration then
        FSavedGeneration := generation;
    finally
      FLock.Leave;
    end;
  finally
    FSaveLock.Leave;
  end;
end;

//FSaveLock is held. A save writes a temporary file and renames it over the index - one killed in between leaves it.
procedure TFolderSourceIndex.DeleteStaleTempFiles;
var
  searchRec : TSearchRec;
  folder : string;
  tempFile : string;
begin
  folder := ExtractFilePath(FFileName);
  if FindFirst(FFileName + '.tmp-*', faAnyFile, searchRec) = 0 then
  begin
    try
      repeat
        if (searchRec.Attr and faDirectory) = 0 then
        begin
          tempFile := folder + searchRec.Name;
          try
            if Now - TFile.GetLastWriteTime(tempFile) > cStaleTempFileAge then
              System.SysUtils.DeleteFile(tempFile);
          except
            //another process's save, just finished with it.
          end;
        end;
      until FindNext(searchRec) <> 0;
    finally
      System.SysUtils.FindClose(searchRec);
    end;
  end;
end;

end.
