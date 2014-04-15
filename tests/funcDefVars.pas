var x : integer, y :integer;

function funkce() : double;
begin
    x := x + 1;
    writeln(x);
    funkce := 5;
end

begin
    x := x + funkce(5+6);
    writeln(x);
    readln(x);
    writeln(funkce(5));
    funkce(3);
    x := 2;  
end. 
