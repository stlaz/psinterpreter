var
    x : integer,
    y : integer,
    mdi : integer,
    z : double,
    st : string; 

function prdel(prd : integer) : integer;
{begin
    writeln("nooo");
end}

function prdel(prd : integer) : string;
var
    prdik : integer;
begin
    {writeln(prd);}
    if(prd > 100) then
        prdel := 'hovno';
    else prdel := 'hodne smradu';
end


begin 
    x := 5;
    y := (4+7)*9 - 2 + 3;
    mdi := 20 div 7;
    if (x + 2) >= 7 then writeln('prd\nel');
    else x :=5;
    readln(st);
    writeln(st);
    writeln(y);
    x;
    writeln(mdi);
    st := prdel(101);
    writeln(st);
end.
