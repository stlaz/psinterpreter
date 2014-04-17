var x:integer, y : double;

function sideeffector(z : integer) : integer;
begin
    x := z + x + 1;
    sideeffector := z + x + 3; 
end


begin
    x := 3;
    x := sideeffector(5) + sideeffector(7) * x;
    writeln(x);
    sideeffector(4) + sideeffector(2);
    writeln(x);
end.
