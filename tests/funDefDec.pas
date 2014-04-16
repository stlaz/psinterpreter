{ lolol }



var a : integer, hovno : double, prdik : string;

function serepes (n : integer) : string;

function lolcode (n : double) : integer;
	var hovno : integer, retezhoven : string;
	begin
        hovno := 3;
        retezhoven := serepes(hovno);
        writeln(retezhoven);
        lolcode := hovno;
	end

function serepes (n : integer) : string;
    begin
        if (n > 10) then
            serepes := 'serepes';
        else
            serepes := 'neserepes';
    end

begin
	if (10<20) then
        begin
            prdik := serepes(20);
        end
    else
        prdik := 'neserepes';
    a := lolcode(3);
	writeln(prdik);
end.
