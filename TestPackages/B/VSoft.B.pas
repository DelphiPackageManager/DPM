unit VSoft.B;

interface

function B(const b : string) : string;

implementation

uses
  VSoft.D;

function B(const b : string) : string;
begin
  result := 'B = ' + b + D('d');
end;

end.
