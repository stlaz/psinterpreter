var x : integer;

begin
 x:=5;
 if (x > 4) then writeln(x)
 else begin
     x:= 3;
     if x < 4 then
         writeln(x);
     else x:=7
 end;
end.
