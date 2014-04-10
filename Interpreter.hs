module Main ( main ) where

import System.Environment( getArgs )
import System.IO.Unsafe( unsafePerformIO )

import PascalParser

type SymbolTable = [(String, Symbol)]
type Symbol = (PasTypes, Int, Double, String)

type FunctionTable = [(String, [ (String, PasTypes) ], PasTypes,
					 [ (String, PasTypes) ], Command)]

fst''  (x,_,_,_) = x
snd''  (_,x,_,_) = x
trd''  (_,_,x,_) = x
frth'' (_,_,_,x) = x

getType = fst''
getInt = snd''
getDbl = trd''
getSt = frth''

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
	| (firstType == PasInt) && (secondType == PasInt) = 1		-- Int Int
	| (firstType == PasInt) && (secondType == PasDbl) = 2		-- Int Dbl
	| (firstType == PasDbl) && (secondType == PasInt) = 3		-- Dbl Int
	| (firstType == PasDbl) && (secondType == PasDbl) = 4		-- Dbl Dbl
	| (firstType == PasStr) && (secondType == PasStr) = 5		-- String String
	where
		firstType = getType x
		secondType = getType y

evaluate :: FunctionTable -> SymbolTable -> Expr -> Symbol
evaluate tf ts (IConst c) = (PasInt, c, 0.0, "")
evaluate tf ts (DConst c) = (PasDbl, 0, c, "")
evaluate tf ts (SConst s) = (PasStr, 0, 0.0, s)
evaluate tf ts (Var v) = get ts v
-- TODO: What happens when exp1 or exp2 each other evaluate to different type?
evaluate tf ts (Add exp1 exp2)
	| trinity == 1 = (PasInt, (snd'' first) + (snd'' second), 0.0, "")
	| trinity == 2 = (PasDbl, 0, (fromIntegral (snd'' first)) + (trd'' second), "")
	| trinity == 3 = (PasDbl, 0, (trd'' first) + (fromIntegral (snd'' second)), "")
	| trinity == 4 = (PasDbl, 0, (trd'' first) + (trd'' second), "")
	| trinity == 5 = (PasStr, 0, 0.0, (frth'' first) ++ (frth'' second))
	| otherwise = (PasNone, 0, 0.0, "")
	where
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		trinity = binTypes first second

evaluate tf ts (Sub exp1 exp2)
	| trinity == 1 = (PasInt, (snd'' first) - (snd'' second), 0.0, "")
	| trinity == 2 = (PasDbl, 0, (fromIntegral (snd'' first)) - (trd'' second), "")
	| trinity == 3 = (PasDbl, 0, (trd'' first) - (fromIntegral (snd'' second)), "")
	| trinity == 4 = (PasDbl, 0, (trd'' first) - (trd'' second), "")
	| otherwise = (PasNone, 0, 0.0, "")
	where
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		trinity = binTypes first second

evaluate tf ts (Mult exp1 exp2)
	| trinity == 1 = (PasInt, (snd'' first) * (snd'' second), 0.0, "")
	| trinity == 2 = (PasDbl, 0, (fromIntegral (snd'' first)) * (trd'' second), "")
	| trinity == 3 = (PasDbl, 0, (trd'' first) * (fromIntegral (snd'' second)), "")
	| trinity == 4 = (PasDbl, 0, (trd'' first) * (trd'' second), "")
	| otherwise = (PasNone, 0, 0.0, "")
	where
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		trinity = binTypes first second

evaluate tf ts (Div exp1 exp2)
	| trinity == 1 = (PasInt, (snd'' first) `div` (snd'' second), 0.0, "")
	| otherwise = (PasNone, 0, 0.0, "")
	where
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		trinity = binTypes first second

evaluate tf ts (Pars exp) = evaluate tf ts exp
evaluate tf ts (FuncCall name args) = evalFunc tf ts name $ evalList tf ts args 

evalList tf ts [] = []
evalList tf ts (expr:tail) = (evaluate tf ts expr):(evalList tf ts tail)

evalFunc :: FunctionTable -> SymbolTable -> String -> [Symbol] -> Symbol
evalFunc tf ts name args = get (unsafePerformIO $ interpret tf ts (Writeln (SConst "Standa je buh"))) name


evalCond :: FunctionTable -> SymbolTable -> BoolExpr -> Bool
evalCond tf ts (Equal exp1 exp2)    = (evaluate tf ts exp1) == (evaluate tf ts exp2)
evalCond tf ts (NEqual exp1 exp2)   = (evaluate tf ts exp1) /= (evaluate tf ts exp2)
evalCond tf ts (IsLess exp1 exp2)   = (evaluate tf ts exp1) < (evaluate tf ts exp2)
evalCond tf ts (IsGreat exp1 exp2)  = (evaluate tf ts exp1) > (evaluate tf ts exp2)
evalCond tf ts (IsLessE exp1 exp2)  = (evaluate tf ts exp1) <= (evaluate tf ts exp2)
evalCond tf ts (IsGreatE exp1 exp2) = (evaluate tf ts exp1) >= (evaluate tf ts exp2)

interpret :: FunctionTable -> SymbolTable -> Command -> IO SymbolTable
interpret tf ts Empty = return ts	-- Empty expression, simple
interpret tf ts (Assign var expr) = return $ set ts var $ evaluate tf ts expr
interpret tf ts (Writeln expr) = do
	putStrLn $ id result
	return ts
	where result
			| (getType midres) == PasInt = show $ snd'' midres
			| (getType midres) == PasDbl = show $ trd'' midres
			| (getType midres) == PasStr = frth'' midres
			| otherwise = "WritelnError"
			where
				midres = evaluate tf ts expr

interpret tf ts (Readln id) = do
	val <- getLine
	return $ set ts id $ symval val
	where
		symval val
			| oldtype == PasInt = (PasInt, read val :: Int, 0.0, "")
			| oldtype == PasDbl = (PasStr, 0, 0.0, val)
			| otherwise = (PasNone , 0, 0.0, "")
			where
				oldtype = getType $ get ts id

interpret tf ts (Seq []) = return ts
interpret tf ts (Seq (com:coms)) = do
	ts' <- interpret tf ts com
	interpret tf ts' (Seq coms)
interpret tf ts (If cond coms1 coms2) =
	if(evalCond tf ts cond) then interpret tf ts coms1
		else interpret tf ts coms2
interpret tf ts (While cond coms) = do
	if(evalCond tf ts cond) then do
		ts' <- interpret tf ts coms
		interpret tf ts' (While cond coms)
	else return ts

interpret tf ts (Expr expr) = return $ set ts "000" $ evaluate tf ts expr

fillSymbols [] = [("000", (PasNone, 0, 0.0, ""))]
fillSymbols (vh:tail) =
	if snd vh == PasInt then
		(fst vh, (PasInt, 0, 0.0, "")):(fillSymbols tail)
	else if snd vh == PasDbl then
		(fst vh, (PasDbl, 0, 0.0, "")):(fillSymbols tail)
	else if snd vh == PasStr then
		(fst vh, (PasStr, 0, 0.0, "")):(fillSymbols tail)
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
			let funcTable = []
			--print symTable
			newsym <- interpret funcTable symTable (trd' absyntree)
			print newsym
			--print $ snd' absyntree
			--print $ trd' absyntree
