unit VSoft.Controls.Menus.MRU;

interface

uses
  System.Classes,
  Vcl.Menus;

type
  TMRUMenuSelectionEvent = procedure(Sender: TObject; const Filename: string) of object;

  TMRUMenu = class(TMenuItem)
  private
    FMRUItems : TStringList;
    FMaxItems : integer;
    FHidePathExtension : boolean;
    FOnSelection : TMRUMenuSelectionEvent;
    procedure SetHidePathExtension(const Value: boolean);
  protected
    procedure SetMaxItems(const Value: integer);
    procedure UpdateCaptions;
    procedure ClickHandler(Sender: TObject);
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure LoadFromList(const list : TStrings);
    procedure SaveToList(const list : TStrings);
    procedure Add(const value : string);overload;
    procedure Remove(const value : string);overload;

    property MaxItems : integer read FMaxItems write SetMaxItems;
    property HidePathExtension : boolean read FHidePathExtension write SetHidePathExtension;
  published
    property OnSelection : TMRUMenuSelectionEvent read FOnSelection write FOnSelection;
  end;


implementation

uses
  System.SysUtils;

type
  TMRUMenuItem = class(TMenuItem)
  private
    FFileName : string;
  public
    property FileName : string read FFileName write FFileName;
  end;

{ TMRUMenu }

procedure TMRUMenu.Add(const value: string);
var
  A: TMRUMenuItem;
  I: Integer;
begin
  I := FMRUItems.IndexOf(value);
  //in case there are dupes - happened once but wasn't able to replicate.
  while I > -1 do
  begin
    FMRUItems.Delete(I);
    Items[i].Free;
    i := FMRUItems.IndexOf(value);
  end;

  // Add the new Filename, if it exceeds the MaxItems limit delete the bottom items
  A := TMRUMenuItem.Create(Self);
  A.FileName := value;
  A.Hint := value;
  A.OnClick := ClickHandler;
  Insert(0, A);
  FMRUItems.Insert(0, value);
  while Count > FMaxItems do
  begin
    FMRUItems.Delete(Count - 1);
    Items[Count - 1].Free;
  end;
  UpdateCaptions;
end;

procedure TMRUMenu.ClickHandler(Sender: TObject);
var
  I: Integer;
  A: TMRUMenuItem;
begin
  if Sender is TMRUMenuItem then
  begin
    A := TMRUMenuItem(Sender);
    I := IndexOf(A);
    if I > 0 then
    begin
      //move it toe the top of the list
      FMRUItems.Delete(I);
      FMRUItems.Insert(0, A.FFileName);
      Self.Remove(A);
      Self.Insert(0,A);
    end;
    UpdateCaptions;
    if Assigned(FOnSelection) then
      FOnSelection(Self, A.FFileName);
  end;
end;

constructor TMRUMenu.Create(AOwner: TComponent);
begin
  inherited;
  FMRUItems := TStringList.Create;
  FMaxItems := 8;
  FHidePathExtension := true;
end;

destructor TMRUMenu.Destroy;
begin
  FMRUItems.Free;
  inherited;
end;

procedure TMRUMenu.LoadFromList(const list: TStrings);
var
  A: TMRUMenuItem;
  i : integer;
begin
  FMRUItems.Clear;
  FMRUItems.AddStrings(list);
  if Count > FMaxItems then
  begin
    while Count > FMaxItems do
      Delete(Count - 1);
  end;
  i := Count;
  While i < FMRUItems.Count do
  begin
    A := TMRUMenuItem.Create(Self);
    A.FileName := FMRUItems[i];
    A.Hint := A.FileName;
    A.OnClick := ClickHandler;
    Self.Add(A);
    Inc(i);
  end;
  UpdateCaptions;

end;

procedure TMRUMenu.Remove(const value: string);
var
  i : integer;
begin
  i := FMRUItems.IndexOf(value);
  if i <> -1 then
  begin
    FMRUItems.Delete(i);
    Delete(i);
    UpdateCaptions;
  end;
end;

procedure TMRUMenu.SaveToList(const list: TStrings);
begin
  list.Clear;
  list.Assign(FMRUItems);
end;

procedure TMRUMenu.SetHidePathExtension(const Value: boolean);
begin
  if FHidePathExtension <> Value then
  begin
    FHidePathExtension := Value;
    UpdateCaptions;
  end;
end;

procedure TMRUMenu.SetMaxItems(const Value: integer);
begin
  if FMaxItems <> Value then
  begin
    FMaxItems := Value;
    if FMaxItems < 1 then
      FMaxItems := 1;

    if Count > FMaxItems then
    begin
      while Count > FMaxItems do
        Items[Count - 1].Free;
    end;
  end;

end;

procedure TMRUMenu.UpdateCaptions;
var
  i : integer;
  s : string;
begin
  for i := 0 to Count - 1 do
  begin
    s := FMRUItems[i];
    TMRUMenuItem(Items[i]).FileName := s;
    if FHidePathExtension then
     s := ExtractFileName(s);
    Items[i].Caption := '&' + IntToStr(I + 1) + ' ' +  s ;
  end;

end;

end.
