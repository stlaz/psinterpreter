{ lolol }



var a : integer, hovno : double, prdik : string;

function serepes (a : integer) : string;

function lolcode (n : double) : integer;
	var hovno : integer, retezhoven : string;
	begin
        hovno := 3;
        retezhoven := serepes(hovno);
        writeln(retezhoven);
        lolcode := hovno;
	end

function serepes (a : integer) : string;
    begin
        if (a > 10) then begin
            writeln('n je vetsi nez 10');
            serepes := 'serepes';
            end
        else begin
            writeln('n je mensi nez 10');
            serepes := 'neserepes';
        end
    end

begin
	if ('lol' > 'gold') then
        begin
            prdik := serepes(200000);
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
