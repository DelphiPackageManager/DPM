unit VSoft.A;

interface

function A(const a : string) : string;

implementation

uses
  VSoft.B,
  VSoft.C;

function A(const a : string) : string;
begin
  result := 'A = ' + a + B('b') + C('c');
end;

end.
