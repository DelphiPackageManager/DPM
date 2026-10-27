unit DPM.Core.Tests.Utils.Json;

// Json text that has to be an object - anything else is refused, and nothing it parsed is left behind.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TJsonTextTests = class
  public
    [Test]
    procedure An_Object_Is_Read;
    [Test]
    [TestCase('Array', '[1, 2]', '|')]
    [TestCase('String', '"text"', '|')]
    [TestCase('NotJson', 'not json', '|')]
    procedure Anything_Else_Is_Refused(const text : string);
    [Test]
    procedure ParseObject_Raises_For_An_Array;
    //a service index that is an array (a server answering something else) is refused as json - not a cast error
    //that leaks the array.
    [Test]
    procedure Service_Index_That_Is_Not_An_Object_Is_Refused;
  end;

implementation

uses
  System.SysUtils,
  JsonDataObjects,
  DPM.Core.Sources.Interfaces,
  DPM.Core.Sources.ServiceIndex,
  DPM.Core.Utils.Json;

{ TJsonTextTests }

procedure TJsonTextTests.An_Object_Is_Read;
var
  jsonObj : TJsonObject;
  error : string;
begin
  Assert.IsTrue(TJsonText.TryParseObject('{"name":"value"}', jsonObj, error), error);
  try
    Assert.AreEqual('value', jsonObj.S['name']);
  finally
    jsonObj.Free;
  end;
end;

procedure TJsonTextTests.Anything_Else_Is_Refused(const text : string);
var
  jsonObj : TJsonObject;
  error : string;
begin
  Assert.IsFalse(TJsonText.TryParseObject(text, jsonObj, error));
  Assert.IsNull(jsonObj, 'nothing to free');
  Assert.IsTrue(error <> '', 'it says why');
end;

procedure TJsonTextTests.ParseObject_Raises_For_An_Array;
begin
  Assert.WillRaise(
    procedure
    begin
      TJsonText.ParseObject('[1, 2]').Free;
    end, EJsonException);
end;

procedure TJsonTextTests.Service_Index_That_Is_Not_An_Object_Is_Refused;
begin
  Assert.WillRaise(
    procedure
    begin
      TServiceIndex.LoadFromString('[{"@id":"x"}]');
    end, EJsonException);
end;

initialization
  TDUnitX.RegisterTestFixture(TJsonTextTests);

end.
