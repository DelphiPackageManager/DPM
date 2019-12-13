unit Unit1;

interface

uses
  VSoft.B;

function Foo : string;

implementation

function Foo : string;
begin
  result := B('foo');
end;

end.
