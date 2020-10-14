unit DPM.Core.Package.Icon;

interface

uses
  System.Classes,
  DPM.Core.Package.Interfaces;


  function CreatePackageIcon(const iconKind : TPackageIconKind; const stream : TStream) : IPackageIcon;

implementation

type
  TPackageIcon = class(TInterfacedObject, IPackageIcon)
  private
    FKind : TPackageIconKind;
    FStream : TStream;
  protected
    function GetKind: TPackageIconKind;
    function GetStream: TStream;
    procedure SetStream(const value: TStream);
  public
    constructor Create(const iconKind : TPackageIconKind; const stream : TStream);
    destructor Destroy;override;
  end;


function CreatePackageIcon(const iconKind : TPackageIconKind; const stream : TStream) : IPackageIcon;
begin
  result := TPackageIcon.Create(iconKind, stream);
end;



{ TPackageIcon }

constructor TPackageIcon.Create(const iconKind: TPackageIconKind; const stream: TStream);
begin
  FKind := iconKind;
  FStream := stream;
  FStream.Position := 0;
end;

destructor TPackageIcon.Destroy;
begin
  if FStream <> nil then
    FStream.Free;
  inherited;
end;

function TPackageIcon.GetKind: TPackageIconKind;
begin
  result := FKind;
end;

function TPackageIcon.GetStream: TStream;
begin
  result := FStream;
end;

procedure TPackageIcon.SetStream(const value: TStream);
begin
  FStream := value;
end;

end.
