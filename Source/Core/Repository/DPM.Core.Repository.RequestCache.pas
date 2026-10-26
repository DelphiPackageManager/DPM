unit DPM.Core.Repository.RequestCache;

interface

uses
  System.SyncObjs,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces;

type
  ///<summary> Remembers repository query results for the length of one operation, so identical queries
  ///  made by several resolves (planning trials, then one resolve per project) reach the server once.
  ///  Before the install pipeline resolved everything up front, the local package cache absorbed those
  ///  repeats because each resolve was followed by a download - nothing is downloaded until every
  ///  project has resolved now.
  ///  Only active between BeginScope and the matching EndScope (scopes nest). Outside a scope nothing is
  ///  stored, so a long lived repository manager (the IDE keeps one) never serves stale results.
  ///  Lists are copied in and out - callers may modify what they get back.
  ///  Thread safe - the repository manager is shared by the IDE's background requests. </summary>
  TRepositoryRequestCache = class
  private
    FLock : TCriticalSection;
    FDepth : integer;
    FVersionsWithDependencies : IDictionary<string, IList<IPackageInfo>>;
    FVersions : IDictionary<string, IList<TPackageVersion>>;
    class function VersionsWithDependenciesKey(const packageId : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const includePrerelease : boolean) : string;
    class function VersionsKey(const packageId : string; const compilerVersion : TCompilerVersion; const includePrerelease : boolean) : string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginScope;
    procedure EndScope;
    function Active : boolean;

    function TryGetVersionsWithDependencies(const packageId : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange;
                                            const includePrerelease : boolean; out versions : IList<IPackageInfo>) : boolean;
    procedure AddVersionsWithDependencies(const packageId : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange;
                                          const includePrerelease : boolean; const versions : IList<IPackageInfo>);

    function TryGetVersions(const packageId : string; const compilerVersion : TCompilerVersion; const includePrerelease : boolean; out versions : IList<TPackageVersion>) : boolean;
    procedure AddVersions(const packageId : string; const compilerVersion : TCompilerVersion; const includePrerelease : boolean; const versions : IList<TPackageVersion>);
  end;

implementation

uses
  System.SysUtils;

{ TRepositoryRequestCache }

constructor TRepositoryRequestCache.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FDepth := 0;
  FVersionsWithDependencies := TCollections.CreateDictionary<string, IList<IPackageInfo>>;
  FVersions := TCollections.CreateDictionary<string, IList<TPackageVersion>>;
end;

destructor TRepositoryRequestCache.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TRepositoryRequestCache.Active : boolean;
begin
  FLock.Enter;
  try
    result := FDepth > 0;
  finally
    FLock.Leave;
  end;
end;

procedure TRepositoryRequestCache.BeginScope;
begin
  FLock.Enter;
  try
    Inc(FDepth);
  finally
    FLock.Leave;
  end;
end;

procedure TRepositoryRequestCache.EndScope;
begin
  FLock.Enter;
  try
    if FDepth = 0 then
      exit;
    Dec(FDepth);
    if FDepth = 0 then
    begin
      FVersionsWithDependencies.Clear;
      FVersions.Clear;
    end;
  finally
    FLock.Leave;
  end;
end;

class function TRepositoryRequestCache.VersionsWithDependenciesKey(const packageId : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const includePrerelease : boolean) : string;
begin
  result := LowerCase(packageId) + '|' + CompilerToString(compilerVersion) + '|' + versionRange.ToString + '|' + BoolToStr(includePrerelease, true);
end;

class function TRepositoryRequestCache.VersionsKey(const packageId : string; const compilerVersion : TCompilerVersion; const includePrerelease : boolean) : string;
begin
  result := LowerCase(packageId) + '|' + CompilerToString(compilerVersion) + '|' + BoolToStr(includePrerelease, true);
end;

function TRepositoryRequestCache.TryGetVersionsWithDependencies(const packageId : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange;
                                                                const includePrerelease : boolean; out versions : IList<IPackageInfo>) : boolean;
var
  cached : IList<IPackageInfo>;
begin
  versions := nil;
  FLock.Enter;
  try
    result := (FDepth > 0) and FVersionsWithDependencies.TryGetValue(VersionsWithDependenciesKey(packageId, compilerVersion, versionRange, includePrerelease), cached);
    if result then
    begin
      versions := TCollections.CreateList<IPackageInfo>;
      versions.AddRange(cached);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRepositoryRequestCache.AddVersionsWithDependencies(const packageId : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange;
                                                              const includePrerelease : boolean; const versions : IList<IPackageInfo>);
var
  copied : IList<IPackageInfo>;
begin
  copied := TCollections.CreateList<IPackageInfo>;
  copied.AddRange(versions);
  FLock.Enter;
  try
    if FDepth > 0 then
      FVersionsWithDependencies[VersionsWithDependenciesKey(packageId, compilerVersion, versionRange, includePrerelease)] := copied;
  finally
    FLock.Leave;
  end;
end;

function TRepositoryRequestCache.TryGetVersions(const packageId : string; const compilerVersion : TCompilerVersion; const includePrerelease : boolean; out versions : IList<TPackageVersion>) : boolean;
var
  cached : IList<TPackageVersion>;
begin
  versions := nil;
  FLock.Enter;
  try
    result := (FDepth > 0) and FVersions.TryGetValue(VersionsKey(packageId, compilerVersion, includePrerelease), cached);
    if result then
    begin
      versions := TCollections.CreateList<TPackageVersion>;
      versions.AddRange(cached);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRepositoryRequestCache.AddVersions(const packageId : string; const compilerVersion : TCompilerVersion; const includePrerelease : boolean; const versions : IList<TPackageVersion>);
var
  copied : IList<TPackageVersion>;
begin
  copied := TCollections.CreateList<TPackageVersion>;
  copied.AddRange(versions);
  FLock.Enter;
  try
    if FDepth > 0 then
      FVersions[VersionsKey(packageId, compilerVersion, includePrerelease)] := copied;
  finally
    FLock.Leave;
  end;
end;

end.
