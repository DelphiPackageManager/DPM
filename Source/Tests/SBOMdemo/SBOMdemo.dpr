program SBOMdemo;

{$APPTYPE CONSOLE}

{ Minimal console app used as a fixture for `dpm sbom`. It references a handful of
  small DPM packages (Spring4D.Base, VSoft.SemanticVersion, VSoft.CancellationToken)
  and exercises each one in trivial ways so the linker actually pulls their units
  into the binary and emits them in the MAP file. }

uses
  System.SysUtils,
  Spring.Collections,
  VSoft.SemanticVersion,
  VSoft.CancellationToken;

var
  items : IList<string>;
  ver : TSemanticVersion;
  token : ICancellationTokenSource;
begin
  try
    items := TCollections.CreateList<string>;
    items.Add('hello');
    items.Add('world');

    if TSemanticVersion.TryParse('1.2.3-beta.4', ver) then
      Writeln('parsed version : ', ver.ToString);

    token := TCancellationTokenSourceFactory.Create;
    if not token.Token.IsCancelled then
      Writeln('cancellation token created, items: ', items.Count);
  except
    on e : Exception do
      Writeln(e.ClassName, ': ', e.Message);
  end;
end.
