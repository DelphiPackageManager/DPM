unit DPM.Core.Utils.Json;

// Reading json text whose root has to be an object. TJsonBaseObject.Parse hands back whatever the root is, so
// 'Parse(text) as TJsonObject' leaked the array (or other value) when the cast failed - these free it.

interface

uses
  JsonDataObjects;

type
  TJsonText = class
  public
    //The object text holds - the caller frees it. False, with nothing to free and error saying why, when it isn't
    //one : not json, or json whose root is something else (an array, a string).
    class function TryParseObject(const text : string; out jsonObj : TJsonObject; out error : string) : boolean; static;
    //As TryParseObject, but raises when text isn't a json object - for callers that report a failure by raising.
    class function ParseObject(const text : string) : TJsonObject; static;
  end;

implementation

uses
  System.SysUtils;

{ TJsonText }

class function TJsonText.TryParseObject(const text : string; out jsonObj : TJsonObject; out error : string) : boolean;
var
  parsed : TJsonBaseObject;
begin
  result := false;
  jsonObj := nil;
  error := '';
  try
    parsed := TJsonBaseObject.Parse(text);
  except
    on e : Exception do
    begin
      error := e.Message;
      exit;
    end;
  end;
  if not (parsed is TJsonObject) then
  begin
    parsed.Free;
    error := 'it is not a json object';
    exit;
  end;
  jsonObj := TJsonObject(parsed);
  result := true;
end;

class function TJsonText.ParseObject(const text : string) : TJsonObject;
var
  error : string;
begin
  if not TryParseObject(text, result, error) then
    raise EJsonException.Create(error);
end;

end.
