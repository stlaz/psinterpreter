{ Program 2: Vypocet faktorialu (rekurzivne) }

var a : integer, vysl : integer;

function factorial (n : integer) : integer;

function factorial (n : integer) : integer;
    var temp_result : integer, decremented_n : integer,
    result : integer;
    begin
        if n < 2 then
        result := 1
    else
        begin
            decremented_n := n - 1;
            temp_result := factorial(decremented_n);
            result := n * temp_result
        end;
            factorial := result

end

begin
    writeln('Zadejte cislo pro vypocet faktorialu');
    readln(a);
    if a < 0 then
        writeln('Faktorial nelze spocitat')
    else
        begin
            vysl := factorial(a);
            writeln('Vysledek je:');
            writeln(vysl)
        end
end.
