unit DPM.Core.Sources.ServiceIndex;

interface

uses
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Sources.Interfaces;

type
  TServiceIndex = class(TInterfacedObject, IServiceIndex)
  private
    FItems : IList<IServiceIndexItem>;
  protected
    function FindItem(const resourceType : string) : IServiceIndexItem;
    function FindItems(const resourceType: string): IEnumerable<IServiceIndexItem>;
    function GetItems: Spring.Collections.IList<IServiceIndexItem>;
    procedure LoadFromJsonObject(const jsonObject : TJsonObject);
  public
    constructor Create;
    class function LoadFromJson(const jsonObject : TJsonObject) : IServiceIndex;
    class function LoadFromString(const jsonString : string) : IServiceIndex;
  end;

  TServiceIndexItem = class(TInterfacedObject, IServiceIndexItem)
  private
    FResourceType: string;
    FResourceUrl: string;
  protected
    function GetResourceType: string;
    function GetResourceUrl: string;
  public
    constructor Create(const resourceType : string; const resourceUrl : string);
  end;


implementation

uses
  System.SysUtils;

{ TServiceIndexItem }

constructor TServiceIndexItem.Create(const resourceType, resourceUrl: string);
begin
  FResourceType := resourceType;
  FResourceUrl := resourceUrl;
end;

function TServiceIndexItem.GetResourceType: string;
begin
  result := FResourceType;
end;

function TServiceIndexItem.GetResourceUrl: string;
begin
  result := FResourceUrl;
end;

{ TServiceIndex }

constructor TServiceIndex.Create;
begin
  FItems := TCollections.CreateList<IServiceIndexItem>;
end;

function TServiceIndex.FindItem(const resourceType: string): IServiceIndexItem;
begin
  result := FindItems(resourceType).FirstOrDefault;
end;

function TServiceIndex.FindItems(const resourceType: string): IEnumerable<IServiceIndexItem>;
begin
  result := FItems.Where(
    function(const item : IServiceIndexItem) : boolean
    begin
        result := SameText(item.ResourceType, resourceType);
    end);
end;

function TServiceIndex.GetItems: Spring.Collections.IList<IServiceIndexItem>;
begin
  result := FItems;
end;

class function TServiceIndex.LoadFromJson(const jsonObject: TJsonObject): IServiceIndex;
begin
  result := TServiceIndex.Create;
  (result as TServiceIndex).LoadFromJsonObject(jsonObject);
end;

procedure TServiceIndex.LoadFromJsonObject(const jsonObject: TJsonObject);
var
  resourcesArray : TJsonArray;
  resourceObj : TJsonObject;
  i: integer;
  resourceType : string;
  resourceUrl : string;
  item : IServiceIndexItem;
begin
  resourcesArray := jsonObject.A['resources'];
  if resourcesArray.Count = 0 then
    exit;
  for i := 0 to resourcesArray.Count -1 do
  begin
    resourceObj := resourcesArray.O[i];
    resourceType := resourceObj.S['@type'];
    resourceUrl := resourceObj.S['@id'];
    item := TServiceIndexItem.Create(resourceType, resourceUrl);
    FItems.Add(item);
  end;
end;

class function TServiceIndex.LoadFromString(const jsonString: string): IServiceIndex;
var
  jsonObj : TJsonObject;
begin
  jsonObj := TJsonObject.Parse(jsonString) as TJsonObject;
  try
    result := LoadFromJson(jsonObj);
  finally
    jsonObj.Free;
  end;
end;

end.
