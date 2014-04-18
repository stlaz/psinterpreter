var n : integer, s : string;

function bar() : double;
function fun(n : string) : double;

function fun(n : string) : double;
begin
    if (n = 'lolcode') then
        begin
            writeln('Function fun with ''lolcode''');
            writeln('Returning 0.2');
            fun := 0.2
        end;
    else
        begin
            writeln('Function fun without ''lolcode''');
            writeln('Recursion with ''lolcode''');
            fun := fun('lolcode')
        end
end

function foo(n : integer) : double;
begin
   if (n < 10) then
       begin
            writeln('Calling bar');
            foo := bar()
        end;
    else
        begin
            writeln('Calling fun with the argument ''lolcode''');
            foo := fun('lolcode')
        end
end

function bar() : double;
begin
    writeln('Function bar');
    writeln('Calling fun with the argument ''none''');
    bar := fun('none')
end

begin
    writeln('Enter an integer:');
    readln(n);
    writeln(foo(n));
    writeln('Testing string concat');
    writeln('String1' + 'String2');
    writeln('');
    writeln('Testing string comparison with ''lolcode''');
    writeln('Enter a string:');
    readln(s);
    if (s < 'lolcode') then
        begin
            writeln(s + ' is lower than ''lolcode''')
        end;
    else
        begin
            if (s > 'lolcode') then
                begin
                    writeln(s +' is greater than ''lolcode''')
                end;
            else
                begin
                    writeln(s + ' equals lolcode')
                end
        end;

    writeln('Shutting Down...');
end.
