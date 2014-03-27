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
evaluate ts (Sub exp1 exp2) = (evaluate ts exp1) - (evaluate ts exp2)
evaluate ts (Mult exp1 exp2) = (evaluate ts exp1) * (evaluate ts exp2)
evaluate ts (Div exp1 exp2) = (evaluate ts exp1) `div` (evaluate ts exp2)
evaluate ts (Pars exp) = evaluate ts exp

evalCond :: SymbolTable -> BoolExpr -> Bool
evalCond ts (Equal exp1 exp2)    = (evaluate ts exp1) == (evaluate ts exp2)
evalCond ts (NEqual exp1 exp2)   = (evaluate ts exp1) /= (evaluate ts exp2)
evalCond ts (IsLess exp1 exp2)   = (evaluate ts exp1) < (evaluate ts exp2)
evalCond ts (IsGreat exp1 exp2)  = (evaluate ts exp1) > (evaluate ts exp2)
evalCond ts (IsLessE exp1 exp2)  = (evaluate ts exp1) <= (evaluate ts exp2)
evalCond ts (IsGreatE exp1 exp2) = (evaluate ts exp1) >= (evaluate ts exp2)

interpret :: SymbolTable -> Command -> IO SymbolTable
interpret ts Empty = return ts	-- Empty expression, simple
interpret ts (Assign var expr) = return $ set ts var $ evaluate ts expr
interpret ts (Writeln expr) = do
	print $ evaluate ts expr
	return ts
interpret ts (Readln id) = do
	val <- readLn
	return $ set ts id val
interpret ts (Seq []) = return ts
interpret ts (Seq (com:coms)) = do
	ts' <- interpret ts com
	interpret ts' (Seq coms)
interpret ts (If cond coms1 coms2) =
	if(evalCond ts cond) then interpret ts coms1
		else interpret ts coms2
interpret ts (While cond coms) = do
	if(evalCond ts cond) then do
		ts' <- interpret ts coms
		interpret ts' (While cond coms)
	else return ts

main = do
	args <- getArgs
	if length args /= 1
		then error "Wrong number of arguments."
		else do
			let fileName = args !! 0
			input <- readFile $ fileName
			let absyntree = parsePascal input fileName
			--interpret [] (trd' absyntree)
			print $ fst' absyntree
			print $ snd' absyntree
			print $ trd' absyntree
			where
				fst' (x, _, _) = x
				snd' (_, x, _) = x
				trd' (_, _, x) = x