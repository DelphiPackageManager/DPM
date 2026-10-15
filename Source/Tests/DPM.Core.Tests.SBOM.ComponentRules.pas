unit DPM.Core.Tests.SBOM.ComponentRules;

interface

uses
  DUnitX.TestFramework,
  DPM.Core.SBOM.ComponentRules;

type
  //Which rule claims a unit, and which rules are accepted at all.
  [TestFixture]
  TSBOMComponentRulesTests = class
  private
    function Rule(const name : string; const units, prefixes : array of string) : TSBOMComponentRule;
    function MatchedName(const rules : ISBOMComponentRules; const unitName : string) : string;
  public
    [Test]
    procedure ExactUnitName_MatchesItsRule;
    [Test]
    procedure Prefix_MatchesIgnoringCase;
    [Test]
    procedure NoRule_MatchesNothing;
    [Test]
    procedure ExactName_BeatsAnEarlierPrefixRule;
    [Test]
    procedure OwnUnit_BeatsALibraryPrefix;
    [Test]
    procedure LibraryPrefix_BeatsAnOwnUnitPrefix;
    [Test]
    procedure OwnUnitPrefix_IsOwnCode;
    [Test]
    procedure FirstRuleWins_WithinAKind;
    [Test]
    procedure AddAll_AppendsTheOtherRulesAfterThese;

    [Test]
    procedure RuleWithoutName_IsRejected;
    [Test]
    procedure RuleType_IsNormalised;
    [Test]
    procedure RuleWithAnUnknownType_IsALibrary;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Logging,
  TestLogger;

{ TSBOMComponentRulesTests }

function TSBOMComponentRulesTests.Rule(const name : string; const units, prefixes : array of string) : TSBOMComponentRule;
var
  i : integer;
begin
  result.Name := name;
  SetLength(result.Units, Length(units));
  for i := 0 to Length(units) - 1 do
    result.Units[i] := units[i];
  SetLength(result.UnitPrefixes, Length(prefixes));
  for i := 0 to Length(prefixes) - 1 do
    result.UnitPrefixes[i] := prefixes[i];
end;

//'' for no match, '<own>' for own code, otherwise the name of the rule that claimed it
function TSBOMComponentRulesTests.MatchedName(const rules : ISBOMComponentRules; const unitName : string) : string;
var
  index : integer;
begin
  case rules.Match(unitName, index) of
    TSBOMUnitMatch.Component : result := rules.Rules[index].Name;
    TSBOMUnitMatch.OwnCode : result := '<own>';
  else
    result := '';
  end;
end;

procedure TSBOMComponentRulesTests.ExactUnitName_MatchesItsRule;
var
  rules : ISBOMComponentRules;
begin
  rules := TSBOMComponentRules.Create;
  rules.Add(Rule('GpDelphiUnits', ['GpLists', 'GpStuff'], []));

  Assert.AreEqual('GpDelphiUnits', MatchedName(rules, 'gpstuff'));
  Assert.AreEqual('', MatchedName(rules, 'GpListsEx'), 'exact means exact');
end;

procedure TSBOMComponentRulesTests.Prefix_MatchesIgnoringCase;
var
  rules : ISBOMComponentRules;
begin
  rules := TSBOMComponentRules.Create;
  rules.Add(Rule('OmniThreadLibrary', [], ['Otl']));

  Assert.AreEqual('OmniThreadLibrary', MatchedName(rules, 'OTLCommon'));
end;

procedure TSBOMComponentRulesTests.NoRule_MatchesNothing;
var
  rules : ISBOMComponentRules;
begin
  rules := TSBOMComponentRules.Create;
  rules.Add(Rule('OmniThreadLibrary', [], ['Otl']));

  Assert.AreEqual('', MatchedName(rules, 'SynEdit'));
end;

procedure TSBOMComponentRulesTests.ExactName_BeatsAnEarlierPrefixRule;
var
  rules : ISBOMComponentRules;
begin
  rules := TSBOMComponentRules.Create;
  rules.Add(Rule('Spring4D', [], ['Spring']));
  rules.Add(Rule('Spring Board', ['SpringBoard'], []));

  Assert.AreEqual('Spring Board', MatchedName(rules, 'SpringBoard'));
  Assert.AreEqual('Spring4D', MatchedName(rules, 'Spring.Collections'));
end;

procedure TSBOMComponentRulesTests.OwnUnit_BeatsALibraryPrefix;
var
  rules : ISBOMComponentRules;
begin
  rules := TSBOMComponentRules.Create;
  rules.Add(Rule('App Framework', [], ['App']));
  rules.AddOwnUnit('AppSettings');

  Assert.AreEqual('<own>', MatchedName(rules, 'AppSettings'));
  Assert.AreEqual('App Framework', MatchedName(rules, 'AppLogging'));
end;

procedure TSBOMComponentRulesTests.LibraryPrefix_BeatsAnOwnUnitPrefix;
var
  rules : ISBOMComponentRules;
begin
  rules := TSBOMComponentRules.Create;
  rules.AddOwnUnitPrefix('Acme');
  rules.Add(Rule('Acme Widgets', [], ['AcmeWidget']));

  Assert.AreEqual('Acme Widgets', MatchedName(rules, 'AcmeWidgetGrid'));
end;

procedure TSBOMComponentRulesTests.OwnUnitPrefix_IsOwnCode;
var
  rules : ISBOMComponentRules;
begin
  rules := TSBOMComponentRules.Create;
  rules.AddOwnUnitPrefix('acme');

  Assert.AreEqual('<own>', MatchedName(rules, 'AcmeMainForm'));
end;

procedure TSBOMComponentRulesTests.FirstRuleWins_WithinAKind;
var
  rules : ISBOMComponentRules;
begin
  rules := TSBOMComponentRules.Create;
  rules.Add(Rule('First', [], ['Syn']));
  rules.Add(Rule('Second', [], ['SynEdit']));

  Assert.AreEqual('First', MatchedName(rules, 'SynEditHighlighter'));
end;

procedure TSBOMComponentRulesTests.AddAll_AppendsTheOtherRulesAfterThese;
var
  rules : ISBOMComponentRules;
  groupRules : ISBOMComponentRules;
begin
  rules := TSBOMComponentRules.Create;
  rules.Add(Rule('Project Copy', [], ['Syn']));
  groupRules := TSBOMComponentRules.Create;
  groupRules.Add(Rule('Group Copy', [], ['Syn']));
  groupRules.Add(Rule('Indy', [], ['Id']));
  groupRules.AddOwnUnit('MainForm');

  rules.AddAll(groupRules);

  Assert.AreEqual('Project Copy', MatchedName(rules, 'SynEdit'), 'this list''s rules come first');
  Assert.AreEqual('Indy', MatchedName(rules, 'IdHTTP'));
  Assert.AreEqual('<own>', MatchedName(rules, 'MainForm'));
end;

procedure TSBOMComponentRulesTests.RuleWithoutName_IsRejected;
var
  candidate : TSBOMComponentRule;
  logger : TTestLogger;
  loggerRef : ILogger;
begin
  candidate := Rule('  ', [], ['Otl']);
  logger := TTestLogger.Create;
  //held as an interface, so passing it to ValidateComponentRule doesn't free it
  loggerRef := logger;
  //a component needs a name - a rule without one is reported and not used
  Assert.IsFalse(ValidateComponentRule(logger, 'App.sbom.yaml component 0', candidate));
  Assert.IsTrue(logger.Logged('warning', 'App.sbom.yaml component 0'), logger.Messages.Text);
end;

procedure TSBOMComponentRulesTests.RuleType_IsNormalised;
var
  candidate : TSBOMComponentRule;
  logger : TTestLogger;
  loggerRef : ILogger;
begin
  candidate := Rule('Spring4D', [], ['Spring']);
  candidate.ComponentType := ' Framework ';
  logger := TTestLogger.Create;
  //held as an interface, so passing it to ValidateComponentRule doesn't free it
  loggerRef := logger;
  Assert.IsTrue(ValidateComponentRule(logger, 'App.sbom.yaml component 0', candidate));
  //case sensitive - it is written as the CycloneDX type enum
  Assert.AreEqual('framework', candidate.ComponentType, false);
end;

procedure TSBOMComponentRulesTests.RuleWithAnUnknownType_IsALibrary;
var
  candidate : TSBOMComponentRule;
  logger : TTestLogger;
  loggerRef : ILogger;
begin
  candidate := Rule('Spring4D', [], ['Spring']);
  candidate.ComponentType := 'toolkit';
  logger := TTestLogger.Create;
  //held as an interface, so passing it to ValidateComponentRule doesn't free it
  loggerRef := logger;
  Assert.IsTrue(ValidateComponentRule(logger, 'App.sbom.yaml component 0', candidate), 'still used');
  Assert.AreEqual('', candidate.ComponentType, 'empty - the writers then use library');
  Assert.IsTrue(logger.Logged('warning', 'toolkit'), logger.Messages.Text);
end;

initialization
  TDUnitX.RegisterTestFixture(TSBOMComponentRulesTests);

end.
