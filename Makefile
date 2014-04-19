#
# File: 	Makefile
# Author: 	Stanislav Laznicka <xlazni08@stud.fit.vutbr.cz>

CL = ghc

xsenko01: Main.hs Commons.hs Interpreter.hs PascalParser.hs SemCheck.hs
	$(CL) -o $@ Main.hs

clean:
	rm -rf *.hi *.o xsenko01 
