var
    x : integer,
    y : integer,
    mdi : integer,
    z : double,
    st : string; 

function zadecek(prd : integer) : string;
{begin
    writeln("nooo");
end}

function zadecek(prd : integer) : string;
var
    prdik : integer;
begin
    {writeln(prd);}
    if(prd > 100) then
        zadecek := 'bobek';
    else zadecek := 'hodne smradu';
end


begin 
    x := 5;
    y := (4+7)*9 - 2 + 3;
    mdi := 20 div 7;
    if (x + 2) >= 7 then writeln('zadecek');
    else x :=5;
    writeln('Enter a string');
    readln(st);
    writeln(st);
    writeln(y);
    x;
    writeln(mdi);
    st := zadecek(101);
    writeln(st);
end.
