unit VSoft.C;

interface

function C(const c : string) : string;

implementation

uses
  VSoft.D;

function C(const c : string) : string;
begin
  result := 'A = ' + c + D('d');
end;

end.
