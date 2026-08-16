program test4(output);
var n, f: integer;

function factorial(x: integer): integer;
begin
  if x <= 1 then
    factorial := 1
  else
    factorial := x * factorial(x - 1)
end;

begin
  for n := 1 to 7 do
  begin
    f := factorial(n);
    writeln(n, f)
  end
end.
