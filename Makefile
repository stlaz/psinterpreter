#
# File: 	Makefile
# Author: 	Stanislav Laznicka <xlazni08@stud.fit.vutbr.cz>

CL = ghc

interpreter: Interpreter.hs PascalParser.hs
	$(CL) -o $@ Interpreter

clean:
	rm -rf *.hi *.o interpreter
