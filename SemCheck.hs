module SemCheck ( semantic, evaluateSem, chkFunctions ) where
 
import PascalParser

type SymbolTable = [(String, Symbol)]
type FunctionTable = [(String, Symbol)]
type Symbol = (PasTypes, Int, Double, String, (PasTypes, [ (String, PasTypes) ], [ (String, PasTypes) ], Command ))


fst''  (x,_,_,_,_) = x
snd''  (_,x,_,_,_) = x
trd''  (_,_,x,_,_) = x
frth'' (_,_,_,x,_) = x
ffth'' (_,_,_,_,x) = x

fceRetType foo = fst foo
	where
		fst (x,_,_,_) = x

setNone = emptySym
setInt num = (PasInt, num, 0.0, "", emptyFuncDef)
setDbl num = (PasDbl, 0, num, "", emptyFuncDef)
setStr str = (PasStr, 0, 0.0, str, emptyFuncDef)
setFnc name t pars locals coms = (PasFunc, 0, 0.0, name, (t, pars, locals, coms))

fillSymbols [] = [("000", emptySym)]
fillSymbols (vh:tail) =
	if snd vh == PasInt then
		(fst vh, (setInt 0)):(fillSymbols tail)
	else if snd vh == PasDbl then
		(fst vh, (setDbl 0.0)):(fillSymbols tail)
	else if snd vh == PasStr then
		(fst vh, (setStr "")):(fillSymbols tail)
	else error "Unknown variable type.\n"

emptySym :: Symbol
emptySym = (PasNone, 0, 0.0, "", emptyFuncDef)
emptyFuncDef = (PasNone, [], [], Empty)

getType = fst''

binTypes x y
	| (x == PasInt) && (y == PasInt) = 1		-- Int Int
	| (x == PasInt) && (y == PasDbl) = 2		-- Int Dbl
	| (x == PasDbl) && (y == PasInt) = 3		-- Dbl Int
	| (x == PasDbl) && (y == PasDbl) = 4		-- Dbl Dbl
	| (x == PasStr) && (y == PasStr) = 5		-- String String
	| otherwise = error "Incompatible types in a binary operation!"

set :: SymbolTable -> String -> Symbol -> SymbolTable
set [] var val = [(var, val)]
set (s@(v,_):ss) var val =
	if v == var	then (var, val):ss
		else s : set ss var val

get :: SymbolTable -> String -> Symbol	
get [] _ = emptySym
get (s@(var, val):ss) v =
	if v == var
		then val
		else get ss v

chkFunctions ts tf (fce:fces) =
	 	if ( (getType $ get (funcSemantic tf ts ((fillSymbols (snd $ ffth'' $ get tf (fst fce))) ++ (fillSymbols (trd $ ffth'' $ get tf (fst fce) ))) (frth $ ffth'' $ get tf (fst fce))) "000") == PasNone) then
			chkFunctions ts tf fces
		else
			error "Function semantic failure."
	where
		fst (x,_) = x
		snd (_,x,_,_) = x
		trd (_,_,x,_) = x
		frth (_,_,_,x) = x
chkFunctions _ _ [] = PasNone

evaluateSem :: FunctionTable -> SymbolTable -> Expr -> PasTypes
evaluateSem tf ts (IConst c) = PasInt
evaluateSem tf ts (DConst c) = PasDbl
evaluateSem tf ts (SConst s) = PasStr
evaluateSem tf ts (FuncCall name args) =
	-- check if defined
	if ((getType $ get tf name) == PasNone) then
		error "Function undefined!"
	else
		if ((chkFceParams (snd $ ffth'' $ get tf name) (evalSemLst tf ts args)) == PasNone) then
			-- get ret val from function
			fceRetType $ ffth''$ get tf name
		else
			error "Function argument type mismatch!"
	where
			snd (_,x,_,_) = x
	
evaluateSem tf ts (Var v) = getType $ get ts v
evaluateSem tf ts (Add exp1 exp2) = do
	case trinity of
		1 -> PasInt
		2 -> PasDbl
		3 -> PasDbl
		4 -> PasDbl
		5 -> PasStr
		_ -> error "Addition type mismatch!"
	where
		first = evaluateSem tf ts exp1
		second = evaluateSem tf ts exp2
		trinity = binTypes first second

evaluateSem tf ts (Sub exp1 exp2) = do
	case trinity of
		1 -> PasInt
		2 -> PasDbl
		3 -> PasDbl
		4 -> PasDbl
		_ -> error "Sub type mismatch!"
	where
		first = evaluateSem tf ts exp1
		second = evaluateSem tf ts exp2
		trinity = binTypes first second

evaluateSem tf ts (Mult exp1 exp2) = do
	case trinity of
		1 -> PasInt
		2 -> PasDbl
		3 -> PasDbl
		4 -> PasDbl
		_ -> error "Mult type mismatch!"
	where
		first = evaluateSem tf ts exp1
		second = evaluateSem tf ts exp2
		trinity = binTypes first second

evaluateSem tf ts (Div exp1 exp2) = do
	case trinity of
		1 -> PasInt
		_ -> error "Div type mismatch!"
	where
		first = evaluateSem tf ts exp1
		second = evaluateSem tf ts exp2
		trinity = binTypes first second

evaluateSem tf ts (Pars exp) = evaluateSem tf ts exp

fceEvalSem :: FunctionTable -> SymbolTable -> SymbolTable -> Expr -> PasTypes
fceEvalSem tf gt lt (IConst c) = PasInt
fceEvalSem tf gt lt (DConst c) = PasDbl
fceEvalSem tf gt lt (SConst s) = PasStr
fceEvalSem tf gt lt (FuncCall name args) =
	-- check if defined
	if ((getType $ get tf name) == PasNone) then
		error "Function undefined!"
	else
		if ((chkFceParams (snd $ ffth'' $ get tf name) (fceEvalSemLst tf gt lt args)) == PasNone) then
			-- get ret val from function
				fceRetType $ ffth''$ get tf name
		else
			error "Function argument type mismatch!"
	where
			snd (_,x,_,_) = x
	
fceEvalSem tf gt lt (Var v) =
	if ((getType $ get lt v) /= PasNone) then
		(getType $ get lt v)
	else
		(getType $ get gt v)
fceEvalSem tf gt lt (Add exp1 exp2) = do
	case trinity of
		1 -> PasInt
		2 -> PasDbl
		3 -> PasDbl
		4 -> PasDbl
		5 -> PasStr
		_ -> error "Addition type mismatch!"
	where
		first = fceEvalSem tf gt lt exp1
		second = fceEvalSem tf gt lt exp2
		trinity = binTypes first second

fceEvalSem tf gt lt (Sub exp1 exp2) = do
	case trinity of
		1 -> PasInt
		2 -> PasDbl
		3 -> PasDbl
		4 -> PasDbl
		_ -> error "Sub type mismatch!"
	where
		first = fceEvalSem tf gt lt exp1
		second = fceEvalSem tf gt lt exp2
		trinity = binTypes first second

fceEvalSem tf gt lt (Mult exp1 exp2) = do
	case trinity of
		1 -> PasInt
		2 -> PasDbl
		3 -> PasDbl
		4 -> PasDbl
		_ -> error "Mult type mismatch!"
	where
		first = fceEvalSem tf gt lt exp1
		second = fceEvalSem tf gt lt exp2
		trinity = binTypes first second

fceEvalSem tf gt lt (Div exp1 exp2) = do
	case trinity of
		1 -> PasInt
		_ -> error "Div type mismatch!"
	where
		first = fceEvalSem tf gt lt exp1
		second = fceEvalSem tf gt lt exp2
		trinity = binTypes first second

fceEvalSem tf gt lt (Pars exp) = fceEvalSem tf gt lt exp


fceEvalSemLst tf gt lt [] = []
fceEvalSemLst tf gt lt (x:xs) = (fceEvalSem tf gt lt x) : (fceEvalSemLst tf gt lt xs)

evalSemLst tf ts [] = []
evalSemLst tf ts (x:xs) = (evaluateSem tf ts x) : (evalSemLst tf ts xs)

chkFceParams :: [(String, PasTypes)] -> [PasTypes] -> PasTypes
chkFceParams (x:xs) (y:ys)= 
	if ((snd x) == y) then
		chkFceParams xs ys
	else
		error "Function argument type mismatch!"
	where
			snd(_,x) = x
chkFceParams [] [] = PasNone
chkFceParams _ _ = error "Function argument count mismatch!"

--testTypes :: SymbolTable -> String -> Symbol -> IO SymbolTable
--testTypes ts var value = do
--		if ((getType value) == (getType $ get ts var)) then do
--			-- Type matches perfectly
--			return $ set ts var value
--		else
--			error "Assignment type mismatch!"

semantic :: FunctionTable -> SymbolTable -> Command -> IO SymbolTable
semantic tf ts Empty = return ts	-- Empty expression, simple
semantic tf ts (Assign var expr) = do
		if (getType $ get ts var) == PasNone then
			error "Variable not defined!"
		else if ((res) == (getType $ get ts var)) then do
			-- Type matches perfectly
			return ts
		else do
			print res
			print (getType $ get ts var)
			error "Assignment type mismatch!"
	where 
		res = evaluateSem tf ts expr

semantic tf ts (Writeln expr) = do
		if ((res) == PasNone) then
			error "Type mismatch!"
		else
			return ts
	where
		res = evaluateSem tf ts expr
semantic tf ts (Readln id) =
	if ((getType $ get ts id) == PasNone) then
		error "Variable undefined!"
	else
		-- Variable found in symbol table
		return ts
semantic tf ts (Seq []) = return ts
semantic tf ts (Seq (com:coms)) = do
	ts' <- semantic tf ts com
	semantic tf ts' (Seq coms)

semantic tf ts (Expr expr) = do
		if ((res) == PasNone) then
			error "Type mismatch!"
		else
			return ts
	where
		res = evaluateSem tf ts expr

-- serepes
funcSemantic :: FunctionTable -> SymbolTable -> SymbolTable -> Command -> SymbolTable
funcSemantic tf gt lt Empty = gt	-- Empty expression, simple
funcSemantic tf gt lt (Assign var expr) = do
		if ((getType $ get lt var) /= PasNone) then
			if ((res) == (getType $ get lt var)) then
				-- Type matches perfectly
				gt
			else
				error "Assignment type mismatch!"
		else if ((getType $ get gt var) /= PasNone) then
			if ((res) == (getType $ get gt var)) then
				-- Type matches perfectly
				gt
			else
				error "Assignment type mismatch!"
		else
			error "Variable undecared!"
	where 
		res = fceEvalSem tf gt lt expr

funcSemantic tf gt lt (Writeln expr) = do
		if ((res) == PasNone) then
			error "Type mismatch!"
		else
			gt
	where
		res = fceEvalSem tf gt lt expr
funcSemantic tf gt lt (Readln id) =
	if ((((getType $ get lt id) == PasNone) && (getType $ get gt id) == PasNone)) then
			error "Variable undefined!"
	else
		-- Variable found in symbol table
		gt
funcSemantic tf gt lt (Seq []) = gt
funcSemantic tf gt lt (Seq (com:coms)) = 
	funcSemantic tf (funcSemantic tf gt lt com) lt (Seq coms)

funcSemantic tf gt lt (Expr expr) = 
		if ((res) == PasNone) then
			error "Type mismatch!"
		else
			gt
	where
		res = fceEvalSem tf gt lt expr
