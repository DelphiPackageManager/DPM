unit VSoft.F;

interface

function F(const a : string) : string;

implementation

function F(const a : string) : string;
begin
  result := 'F = ' + a;
end;

end.
