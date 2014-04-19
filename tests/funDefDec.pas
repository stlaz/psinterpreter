{ lolol }



var a : integer, srapnel : double, prdik : string;

function psicek (a : integer) : string;

function lolcode (n : double) : integer;
	var srapnel : integer, retezsrapnelu : string;
	begin
        srapnel := 3;
        retezsrapnelu := psicek(srapnel);
        writeln(retezsrapnelu);
        lolcode := srapnel;
	end

function psicek (a : integer) : string;
    begin
        if (a > 10) then begin
            writeln('n je vetsi nez 10');
            psicek := 'psicek';
            end
        else begin
            writeln('n je mensi nez 10');
            psicek := 'nepsicek';
        end
    end

begin
	if ('lol' > 'gold') then
        begin
            prdik := psicek(200000);
        end
    else
        prdik := 'neserepes';
    a := lolcode(3);
	writeln(prdik);
    while(a<20) do
        begin
        writeln('cyklipes');
        a := a + 1;
    end;
end.
