program SPDXProcessor;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  JsonDataObjects;

// process https://raw.githubusercontent.com/spdx/license-list-data/main/json/licenses.json
// and convert to id=name,url list

procedure ProcessFile(const jsonFileName : string; const outputFileName : string);
var
  jsonObj : TJsonObject;
  sList : TStringList;
  licensesArray : TJsonArray;
begin
  try
    jsonObj := TJsonObject.ParseFromFile(jsonFileName) as TJsonObject;
    sList := TStringList.Create;
    try
      licensesArray := jsonObj.A['licenses'];

       for var i := 0 to licensesArray.Count - 1 do
       begin
          var license := licensesArray.O[i];
          if license.B['isDeprecatedLicenseId'] then
            continue;
          var id := license.S['licenseId'];
          WriteLn('processing ' + id);

          var name := license.S['name'];
          var url := license.S['reference'];
          sList.AddPair(id, name + ',' + url);

       end;
       sList.SaveToFile(outputFileName, TEncoding.ANSI);
    finally
      jsonObj.Free;
      sList.Free;
    end;
  except
    on e : Exception do
    begin
      WriteLn('Exception while loading json file [' + jsonFileName + ']' + #13#10 + e.Message);
      exit;
    end;
  end;


end;

//TODO : Actually download the file to process

begin
  try
    if ParamCount < 2 then
      raise EInOutArgumentException.Create('Expected 2 parameters - jsonfile outputfile');


    var spdxJsonFile := ParamStr(1);
    if (not FileExists(spdxJsonFile)) then
      raise EInOutArgumentException.Create(spdxJsonFile + ' - does not exist');

    var outputFile := ParamStr(2);


    ProcessFile(spdxJsonFile, outputFile);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
