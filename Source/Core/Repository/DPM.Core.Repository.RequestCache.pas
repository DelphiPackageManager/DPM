unit DPM.Core.Repository.RequestCache;

interface

uses
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
  ///  Lists are copied in and out - callers may modify what they get back. </summary>
  TRepositoryRequestCache = class
  private
    FDepth : integer;
    FVersionsWithDependencies : IDictionary<string, IList<IPackageInfo>>;
    FVersions : IDictionary<string, IList<TPackageVersion>>;
    class function VersionsWithDependenciesKey(const packageId : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const includePrerelease : boolean) : string;
    class function VersionsKey(const packageId : string; const compilerVersion : TCompilerVersion; const includePrerelease : boolean) : string;
  public
    constructor Create;
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
  FDepth := 0;
  FVersionsWithDependencies := TCollections.CreateDictionary<string, IList<IPackageInfo>>;
  FVersions := TCollections.CreateDictionary<string, IList<TPackageVersion>>;
end;

function TRepositoryRequestCache.Active : boolean;
begin
  result := FDepth > 0;
end;

procedure TRepositoryRequestCache.BeginScope;
begin
  Inc(FDepth);
end;

procedure TRepositoryRequestCache.EndScope;
begin
  if FDepth = 0 then
    exit;
  Dec(FDepth);
  if FDepth = 0 then
  begin
    FVersionsWithDependencies.Clear;
    FVersions.Clear;
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
  result := Active and FVersionsWithDependencies.TryGetValue(VersionsWithDependenciesKey(packageId, compilerVersion, versionRange, includePrerelease), cached);
  if result then
  begin
    versions := TCollections.CreateList<IPackageInfo>;
    versions.AddRange(cached);
  end;
end;

procedure TRepositoryRequestCache.AddVersionsWithDependencies(const packageId : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange;
                                                              const includePrerelease : boolean; const versions : IList<IPackageInfo>);
var
  copied : IList<IPackageInfo>;
begin
  if not Active then
    exit;
  copied := TCollections.CreateList<IPackageInfo>;
  copied.AddRange(versions);
  FVersionsWithDependencies[VersionsWithDependenciesKey(packageId, compilerVersion, versionRange, includePrerelease)] := copied;
end;

function TRepositoryRequestCache.TryGetVersions(const packageId : string; const compilerVersion : TCompilerVersion; const includePrerelease : boolean; out versions : IList<TPackageVersion>) : boolean;
var
  cached : IList<TPackageVersion>;
begin
  versions := nil;
  result := Active and FVersions.TryGetValue(VersionsKey(packageId, compilerVersion, includePrerelease), cached);
  if result then
  begin
    versions := TCollections.CreateList<TPackageVersion>;
    versions.AddRange(cached);
  end;
end;

procedure TRepositoryRequestCache.AddVersions(const packageId : string; const compilerVersion : TCompilerVersion; const includePrerelease : boolean; const versions : IList<TPackageVersion>);
var
  copied : IList<TPackageVersion>;
begin
  if not Active then
    exit;
  copied := TCollections.CreateList<TPackageVersion>;
  copied.AddRange(versions);
  FVersions[VersionsKey(packageId, compilerVersion, includePrerelease)] := copied;
end;

end.
