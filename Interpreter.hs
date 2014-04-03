module Main ( main ) where

import System.Environment( getArgs )

import PascalParser

type SymbolTable = [(String, Symbol)]
type Symbol = (Int, Int, Double, String)

fst''  (x,_,_,_) = x
snd''  (_,x,_,_) = x
trd''  (_,_,x,_) = x
frth'' (_,_,_,x) = x

getType x = fst'' x
getInt x = snd'' x
getDbl x = trd'' x
getStr x = frth'' x

set :: SymbolTable -> String -> Symbol -> SymbolTable
set [] var val = [(var, val)]
set (s@(v,_):ss) var val =
	if v == var	then (var, val):ss
		else s : set ss var val
{-
set (s@(v,u):ss) var val =
	if v == var	then
		if stype = stypeNew then
	else s : set ss var val
	where
		stype = getType u
		stypeNew = getType val
-}
get :: SymbolTable -> String -> Symbol	
get [] _ = error "Not found"
get (s@(var, val):ss) v =
	if v == var
		then val
		else get ss v

binTypes x y
	| (firstType == 1) && (secondType == 1) = 1		-- Int Int
	| (firstType == 1) && (secondType == 2) = 2		-- Int Dbl
	| (firstType == 2) && (secondType == 1) = 3		-- Dbl Int
	| (firstType == 2) && (secondType == 2) = 4		-- Dbl Dbl
	| (firstType == 3) && (secondType == 3) = 5		-- String String
	where
		firstType = getType x
		secondType = getType y

evaluate :: SymbolTable -> Expr -> Symbol
evaluate ts (IConst c) = (1, c, 0.0, "")
evaluate ts (DConst c) = (2, 0, c, "")
evaluate ts (SConst s) = (3, 0, 0.0, s)
evaluate ts (Var v) = get ts v
-- TODO: What happens when exp1 or exp2 each other evaluate to different type?
evaluate ts (Add exp1 exp2)
	| trinity == 1 = (1, (snd'' first) + (snd'' second), 0.0, "")
	| trinity == 2 = (2, 2, (fromIntegral (snd'' first)) + (trd'' second), "")
	| trinity == 3 = (2, 2, (trd'' first) + (fromIntegral (snd'' second)), "")
	| trinity == 4 = (2, 2, (trd'' first) + (trd'' second), "")
	| trinity == 5 = (3, 0, 0.0, (frth'' first) ++ (frth'' second))
	| otherwise = (0, 0, 0.0, "")
	where
		first = evaluate ts exp1
		second = evaluate ts exp2
		trinity = binTypes first second

evaluate ts (Sub exp1 exp2)
	| trinity == 1 = (1, (snd'' first) - (snd'' second), 0.0, "")
	| trinity == 2 = (2, 2, (fromIntegral (snd'' first)) - (trd'' second), "")
	| trinity == 3 = (2, 2, (trd'' first) - (fromIntegral (snd'' second)), "")
	| trinity == 4 = (2, 2, (trd'' first) - (trd'' second), "")
	| otherwise = (0, 0, 0.0, "")
	where
		first = evaluate ts exp1
		second = evaluate ts exp2
		trinity = binTypes first second

evaluate ts (Mult exp1 exp2)
	| trinity == 1 = (1, (snd'' first) * (snd'' second), 0.0, "")
	| trinity == 2 = (2, 2, (fromIntegral (snd'' first)) * (trd'' second), "")
	| trinity == 3 = (2, 2, (trd'' first) * (fromIntegral (snd'' second)), "")
	| trinity == 4 = (2, 2, (trd'' first) * (trd'' second), "")
	| otherwise = (0, 0, 0.0, "")
	where
		first = evaluate ts exp1
		second = evaluate ts exp2
		trinity = binTypes first second

evaluate ts (Div exp1 exp2)
	| trinity == 1 = (1, (snd'' first) `div` (snd'' second), 0.0, "")
	| otherwise = (0, 0, 0.0, "")
	where
		first = evaluate ts exp1
		second = evaluate ts exp2
		trinity = binTypes first second

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
	putStrLn $ id result
	return ts
	where result
			| (fst'' midres) == 1 = show $ snd'' midres
			| (fst'' midres) == 2 = show $ trd'' midres
			| (fst'' midres) == 3 = frth'' midres
			| otherwise = "WritelnError"
			where
				midres = evaluate ts expr

interpret ts (Readln id) = do
	val <- getLine
	return $ set ts id $ symval val
	where
		symval val
			| oldtype == 1 = (1, read val :: Int, 0.0, "")
			| oldtype == 3 = (3, 0, 0.0, val)
			| otherwise = (0 , 0, 0.0, "")
			where
				oldtype = getType $ get ts id

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

fillSymbols [] = []
fillSymbols (vh:tail) =
	if snd vh == "integer" then
		(fst vh, (1, 0, 0.0, "")):(fillSymbols tail)
	else if snd vh == "double" then
		(fst vh, (2, 0, 0.0, "")):(fillSymbols tail)
	else if snd vh == "string" then
		(fst vh, (3, 0, 0.0, " ")):(fillSymbols tail)
	else error "Unknown variable type.\n"

main = do
	args <- getArgs
	if length args /= 1
		then error "Wrong number of arguments."
		else do
			let fileName = args !! 0
			input <- readFile $ fileName
			let absyntree = parsePascal input fileName
			let symTable = fillSymbols (fst' absyntree)
			print symTable
			interpret symTable (trd' absyntree)
			print $ fst' absyntree
			print $ snd' absyntree
			print $ trd' absyntree
