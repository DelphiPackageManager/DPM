unit LockFileParserTests;

interface
uses
  DUnitX.TestFramework,
  DPM.Core.Logging;

type
  [TestFixture]
  TLockFileParseTests = class
  private
    FLogger : ILogger;
  public

  [Test]
  procedure Can_Parse_Sample_File;

  [Test]
  procedure Will_fail_on_bad_version;

  [SetupFixture]
  procedure FixtureSetup;

  end;

implementation

uses
  System.SysUtils,
  TestLogger,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.LockFile;

{ TLockFileParseTests }

procedure TLockFileParseTests.Can_Parse_Sample_File;
var
  fileName : string;
  logger : ILogger;
  lockFile : ILockFile;
  lockFileReader : ILockFileReader;
  aNode : IGraphNode;
  bNode : IGraphNode;
  cNode : IGraphNode;
  dNode : IGraphNode;
begin
  fileName := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\TestFiles\LockFiles\test.lock');
  logger := TTestLogger.Create;
  lockFileReader := TLockFileReader.Create(logger);
  Assert.IsTrue(lockFileReader.TryLoadFromFile(fileName, lockFile));
  Assert.IsNotNull(lockFile);
  Assert.IsNotNull(lockFile.Graph);
  Assert.IsTrue(lockFile.Graph.HasChildren);

  aNode := lockFile.Graph.FindNode('VSoft.A');
  Assert.IsNotNull(aNode);
  Assert.AreEqual('VSoft.A', aNode.Id);
  Assert.AreEqual('1.0.0', aNode.SelectedVersion.ToString);

  bNode := aNode.FindNode('VSoft.B');
  Assert.IsNotNull(bNode);
  Assert.AreEqual('VSoft.B', bNode.Id);
  Assert.AreEqual('1.0.2', bNode.SelectedVersion.ToString);

  dNode := bNode.FindNode('VSoft.D');
  Assert.IsNotNull(dNode);
  Assert.AreEqual('VSoft.D', dNode.Id);
  Assert.AreEqual('2.1.5', dNode.SelectedVersion.ToString);


  cNode := aNode.FindNode('VSoft.C');
  Assert.IsNotNull(cNode);
  Assert.AreEqual('VSoft.C', cNode.Id);
  Assert.AreEqual('1.2.2', cNode.SelectedVersion.ToString);

  dNode := cNode.FindNode('VSoft.D');
  Assert.IsNotNull(dNode);
  Assert.AreEqual('VSoft.D', dNode.Id);
  Assert.AreEqual('2.1.5', dNode.SelectedVersion.ToString);

end;

procedure TLockFileParseTests.FixtureSetup;
begin
  FLogger := TTestLogger.Create;
end;

procedure TLockFileParseTests.Will_fail_on_bad_version;
var
  fileName : string;
  lockFile : ILockFile;
  lockFileReader : ILockFileReader;
begin
  fileName := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\TestFiles\LockFiles\testBadVersion.lock');
  lockFileReader := TLockFileReader.Create(FLogger);
  Assert.IsFalse(lockFileReader.TryLoadFromFile(fileName, lockFile));
end;

initialization
  TDUnitX.RegisterTestFixture(TLockFileParseTests);
end.
