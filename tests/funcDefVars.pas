var x : integer, y :integer;

function funkce() : integer;
begin
    x := x + 1;
    writeln(x);
    funkce := 5;
end

begin
    x := x + funkce();
    writeln(x);
    readln(x);
    writeln(funkce());
    funkce();
    x := 2;  
end. 
