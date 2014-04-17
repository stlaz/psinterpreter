var x : double;

function globdec() : double;
begin
    x := x - 5;
    globdec := 0.0;
end

begin
    x:=2.0;
    if(x > globdec()) then begin
        writeln('x > 0');
        writeln(x);
        end
        else writeln(x)
end.

