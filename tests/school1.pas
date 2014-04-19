{ Program 1: Vypocet faktorialu }
{ globalni promenne }
var a : integer, vysl : integer;
begin
    writeln('Zadejte cislo pro vypocet faktorialu: ');
    readln(a);
    if a < 0 then
        writeln('Faktorial nelze spocitat')
    else
    begin
    vysl := 1;
    while a > 0 do
        begin
            vysl := vysl * a;
            a := a - 1
        end;
        writeln('Vysledek je:');
        writeln(vysl)
    end
end.  
