module Main ( main ) where

import System.Environment( getArgs )

import PascalParser

type SymbolTable = [(String, Int)]

set :: SymbolTable -> String -> Int -> SymbolTable
set [] var val = [(var, val)]
set (s@(v,_):ss) var val =
	if v == var
		then (var, val):ss
		else s : set ss var val

get :: SymbolTable -> String -> Int		
get [] _ = error "Not found"
get (s@(var, val):ss) v =
	if v == var
		then val
		else get ss v

evaluate :: SymbolTable -> Expr -> Int
evaluate ts (IConst c) = c
evaluate ts (Var v) = get ts v
-- TODO: What happens when exp1 or exp2 each other evaluate to different type?
evaluate ts (Add exp1 exp2) = (evaluate ts exp1) + (evaluate ts exp2)
evaluate ts (Mult exp1 exp2) = (evaluate ts exp1) * (evaluate ts exp2)

interpret :: SymbolTable -> Command -> IO SymbolTable
interpret ts Empty = return ts	-- Empty expression, simple
interpret ts (Assign var expr) = return $ set ts var $ evaluate ts expr
interpret ts (Writeln expr) = do
	print $ evaluate ts expr
	return ts
interpret ts (Seq []) = return ts
interpret ts (Seq (com:coms)) = do
	ts' <- interpret ts com
	interpret ts' (Seq coms)


main = do
	args <- getArgs
	if length args /= 1
		then error "Wrong number of arguments."
		else do
			let fileName = args !! 0
			input <- readFile $ fileName
			let absyntree = parsePascal input fileName
			interpret [] (snd absyntree)
			print $ fst absyntree