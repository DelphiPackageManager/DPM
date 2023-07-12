unit DPM.IDE.ActivityIndicator;

interface

type
  TActivityIndicator = record
  private
    FFrameIndex : integer;
    function GetCurrentFrame : string;
    function GetIsActive : boolean;
  public
    procedure Step;
    procedure Stop;
    property CurrentFrame : string read GetCurrentFrame;
    property IsActive : boolean read GetIsActive;

  end;



implementation

const activityIndicatorFrames : Array of string =[
			'⣷',
			'⣯',
			'⣟',
			'⡿',
			'⢿',
			'⣻',
			'⣽',
			'⣾'
		];

{ TActivityIndicator }

function TActivityIndicator.GetCurrentFrame: string;
begin
  if FFrameIndex > -1 then
    result := activityIndicatorFrames[FFrameIndex]
  else
    result := '';
end;

function TActivityIndicator.GetIsActive: boolean;
begin
  result := FFrameIndex <> -1;
end;

procedure TActivityIndicator.Step;
begin
  if (FFrameIndex < Length(activityIndicatorFrames) - 1 ) then
    Inc(FFrameIndex)
  else
    FFrameIndex := 0;
end;

procedure TActivityIndicator.Stop;
begin
  FFrameIndex := -1;
end;




end.
