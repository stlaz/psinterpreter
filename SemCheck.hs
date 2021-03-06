{-
    Project: FLP-2014-Pascal
    
  	@file: Interpreter.hs
  	@authors: 
        Stanislav Laznicka  <xlazni08@stud.fit.vutbr.cz>
        Petr Kubat          <xkubat11@stud.fit.vutbr.cz>
-}

module SemCheck ( semantic, evaluateSem, chkFunctions ) where

import Commons

fceRetType foo = fst foo
	where
		fst (x,_,_,_) = x

-- returns the resulting type of a binary operation
fceBinTypes x y
	| (x == PasInt) && (y == PasInt) = 1		-- Int Int
	| (x == PasInt) && (y == PasDbl) = 2		-- Int Dbl
	| (x == PasDbl) && (y == PasInt) = 3		-- Dbl Int
	| (x == PasDbl) && (y == PasDbl) = 4		-- Dbl Dbl
	| (x == PasStr) && (y == PasStr) = 5		-- String String
	| otherwise = error "Incompatible types in a binary operation!"

-- semantic check for function body
chkFunctions ts tf (fce:fces) =

	 	-- This if builds a local symbol table from function params, local variables,
	 	-- checks the local table for multiple definitons.
	 	-- It then adds the return variable to the local table (name of the function)
	 	-- and checks the function body.
	 	-- If successful, it checks the rest of the function table.
        if ( (getType $ get (funcSemantic (fst fce) tf ts ((chkSymTables (fillSymbols ((snd' $ getFnc $ get tf (fst fce)) ++ (trd' $ getFnc $ get tf (fst fce) ))) tf)  ++ (fillSymbols ((fst fce, fst' $ getFnc $ snd fce):[]))) (frth' $ getFnc $ get tf (fst fce))) "000") == PasNone) then
			chkFunctions ts tf fces
		else
			error ("Function semantic failure in function: " ++ (fst fce))
	where
		fst' (x,_,_,_) = x
		snd' (_,x,_,_) = x
		trd' (_,_,x,_) = x
		frth' (_,_,_,x) = x

chkFunctions _ _ [] = PasNone

-- semantic Expr checkers for the main block
evaluateSem :: FunctionTable -> SymbolTable -> Expr -> PasTypes
evaluateSem tf ts (IConst c) = PasInt
evaluateSem tf ts (DConst c) = PasDbl
evaluateSem tf ts (SConst s) = PasStr
evaluateSem tf ts (FuncCall name args) =
	-- check if defined
	if ((getType $ get tf name) == PasNone) then
		error ("Function undefined: " ++ name)
	else
		if ((chkFceParams (snd $ getFnc $ get tf name) (evalSemLst tf ts args)) == PasNone) then
			-- get ret val from function
			fceRetType $ getFnc$ get tf name
		else
			error ("Function argument type mismatch: " ++ name)
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
		trinity = fceBinTypes first second

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
		trinity = fceBinTypes first second

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
		trinity = fceBinTypes first second

evaluateSem tf ts (Div exp1 exp2) = do
	case trinity of
		1 -> PasInt
		_ -> error "Div type mismatch!"
	where
		first = evaluateSem tf ts exp1
		second = evaluateSem tf ts exp2
		trinity = fceBinTypes first second

evaluateSem tf ts (Pars exp) = evaluateSem tf ts exp

-- semantic Expr checkers for the function block
-- has to have a global and a local symbol table, also the id of the checked function
-- so it can check for declaration of function calls inside the function
fceEvalSem :: String -> FunctionTable -> SymbolTable -> SymbolTable -> Expr -> PasTypes
fceEvalSem id tf gt lt (IConst c) = PasInt
fceEvalSem id tf gt lt (DConst c) = PasDbl
fceEvalSem id tf gt lt (SConst s) = PasStr
fceEvalSem id tf gt lt (FuncCall name args) =
	-- check if defined
	if ((getType $ get tf name) == PasNone) then
		error ("Function undefined: " ++ name)
	else if ((getIndex 0 name tf) > (getIndex 0 id tf)) then
            error ("Function declaration missing for function: " ++ name ++ " in function: " ++ id)
	else if ((chkFceParams (snd $ getFnc $ get tf name) (fceEvalSemLst name tf gt lt args)) == PasNone) then
			-- get ret val from function
				fceRetType $ getFnc$ get tf name
		else
			error ("Function argument type mismatch: " ++ name ++ " in function: " ++ id)
	where
			snd (_,x,_,_) = x

fceEvalSem id tf gt lt (Var v) =
	if ((getType $ get lt v) /= PasNone) then
		(getType $ get lt v)
	else
		(getType $ get gt v)
fceEvalSem id tf gt lt (Add exp1 exp2) = do
	case trinity of
		1 -> PasInt
		2 -> PasDbl
		3 -> PasDbl
		4 -> PasDbl
		5 -> PasStr
		_ -> error "Addition type mismatch!"
	where
		first = fceEvalSem id tf gt lt exp1
		second = fceEvalSem id tf gt lt exp2
		trinity = fceBinTypes first second

fceEvalSem id tf gt lt (Sub exp1 exp2) = do
	case trinity of
		1 -> PasInt
		2 -> PasDbl
		3 -> PasDbl
		4 -> PasDbl
		_ -> error "Sub type mismatch!"
	where
		first = fceEvalSem id tf gt lt exp1
		second = fceEvalSem id tf gt lt exp2
		trinity = fceBinTypes first second

fceEvalSem id tf gt lt (Mult exp1 exp2) = do
	case trinity of
		1 -> PasInt
		2 -> PasDbl
		3 -> PasDbl
		4 -> PasDbl
		_ -> error "Mult type mismatch!"
	where
		first = fceEvalSem id tf gt lt exp1
		second = fceEvalSem id tf gt lt exp2
		trinity = fceBinTypes first second

fceEvalSem id tf gt lt (Div exp1 exp2) = do
	case trinity of
		1 -> PasInt
		_ -> error "Div type mismatch!"
	where
		first = fceEvalSem id tf gt lt exp1
		second = fceEvalSem id tf gt lt exp2
		trinity = fceBinTypes first second

fceEvalSem id tf gt lt (Pars exp) = fceEvalSem id tf gt lt exp

-- functions for type evaluation of a list of argument
-- function version
fceEvalSemLst id tf gt lt [] = []
fceEvalSemLst id tf gt lt (x:xs) = (fceEvalSem id tf gt lt x) : (fceEvalSemLst id tf gt lt xs)

-- main block version
evalSemLst tf ts [] = []
evalSemLst tf ts (x:xs) = (evaluateSem tf ts x) : (evalSemLst tf ts xs)

-- checks if the function parameter types match the definition/declaration
chkFceParams :: [(String, PasTypes)] -> [PasTypes] -> PasTypes
chkFceParams (x:xs) (y:ys)=
	if ((snd x) == y) then
		chkFceParams xs ys
	else if (((snd x) == PasDbl) && (y == PasInt)) then
        chkFceParams xs ys
    else
		error "Function argument type mismatch!"
	where
			snd(_,x) = x

chkFceParams [] [] = PasNone
chkFceParams _ _ = error "Function argument count mismatch!"

-- main function of the semantic check in the main block
semantic :: FunctionTable -> SymbolTable -> Command -> IO SymbolTable
semantic tf ts Empty = return ts	-- Empty expression, simple
semantic tf ts (Assign var expr) = do
		if (getType $ get ts var) == PasNone then
			error ("Variable undefined: " ++ var)
		else if ((res) == (getType $ get ts var)) then do
			-- Type matches perfectly
			return ts
		else do
			error ("Assignment type mismatch: " ++ var)
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
		error ("Variable undefined: " ++ id)
	else
		-- Variable found in symbol table
		return ts
semantic tf ts (Seq []) = return ts
semantic tf ts (Seq (com:coms)) = do
	ts' <- semantic tf ts com
	semantic tf ts' (Seq coms)

semantic tf ts (If cond coms1 coms2) = 
	if ((semEvalCond tf ts cond) == PasNone) then
		error "Type mismatch in bool expression."
	else do
		ts' <- semantic tf ts coms1
		ts' <- semantic tf ts coms2
		return ts'

semantic tf ts (While cond coms) = do
  if ((semEvalCond tf ts cond) == PasNone) then
		error "Type mismatch in bool expression."
	else do
		ts' <- semantic tf ts coms
		return ts'

semantic tf ts (Expr expr) = do
		if ((res) == PasNone) then
			error "Type mismatch!"
		else
			return ts
	where
		res = evaluateSem tf ts expr

-- main function of the semantic check in the function body
-- basically the same as previous checker, has to have local table, global table, function id
funcSemantic :: String -> FunctionTable -> SymbolTable -> SymbolTable -> Command -> SymbolTable
funcSemantic id tf gt lt Empty = gt	-- Empty expression, simple
funcSemantic id tf gt lt (Assign var expr) = do
		if ((getType $ get lt var) /= PasNone) then
			if ((res) == (getType $ get lt var)) then
				-- Type matches perfectly
				gt
			else
				error ("Assignment type mismatch: " ++ var ++ " in function: " ++ id)
		else if ((getType $ get gt var) /= PasNone) then
			if ((res) == (getType $ get gt var)) then
				-- Type matches perfectly
				gt
			else
				error ("Assignment type mismatch: " ++ var ++ " in function: " ++ id)
		else
			error ("Variable undefined: " ++ var ++ " in function: " ++ id)
	where
		res = fceEvalSem id tf gt lt expr

funcSemantic id tf gt lt (Writeln expr) = do
		if ((res) == PasNone) then
			error "Type mismatch!"
		else
			gt
	where
		res = fceEvalSem id tf gt lt expr
funcSemantic id tf gt lt (Readln name) =
	if ((((getType $ get lt name) == PasNone) && (getType $ get gt name) == PasNone)) then
			error ("Variable undefined: " ++ name ++ " in function: " ++ id)
	else
		-- Variable found in symbol table
		gt
funcSemantic id tf gt lt (Seq []) = gt
funcSemantic id tf gt lt (Seq (com:coms)) =
	funcSemantic id tf (funcSemantic id tf gt lt com) lt (Seq coms)

funcSemantic id tf gt lt (If cond coms1 coms2) = 
	if ((semFceEvalCond id tf gt lt cond) == PasNone) then
		error ("Type mismatch in bool expression in function: " ++ id)
	else do
		ts' <- funcSemantic id tf gt lt coms1
		ts' <- funcSemantic id tf gt lt coms2
		return ts'

funcSemantic id tf gt lt (While cond coms) = do
  if ((semFceEvalCond id tf gt lt cond) == PasNone) then
		error ("Type mismatch in bool expression in function: " ++ id)
	else do
		ts' <- funcSemantic id tf gt lt coms
		return ts'

funcSemantic id tf gt lt (Expr expr) =
		if ((res) == PasNone) then
			error "Type mismatch!"
		else
			gt
	where
		res = fceEvalSem id tf gt lt expr

-- boolean expression type evaluation functions for the main body
semEvalCond :: FunctionTable -> SymbolTable -> BoolExpr -> PasTypes
semEvalCond tf ts (Equal exp1 exp2)    = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error "Type mismatch in bool expression."
	where
        fstCon = evaluateSem tf ts exp1
        sndCon = evaluateSem tf ts exp2

semEvalCond tf ts (NEqual exp1 exp2)   = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error "Type mismatch in bool expression."
	where
        fstCon = evaluateSem tf ts exp1
        sndCon = evaluateSem tf ts exp2

semEvalCond tf ts (IsLess exp1 exp2)   = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error "Type mismatch in bool expression."
	where
        fstCon = evaluateSem tf ts exp1
        sndCon = evaluateSem tf ts exp2

semEvalCond tf ts (IsGreat exp1 exp2)  = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error "Type mismatch in bool expression."
    where
        fstCon = evaluateSem tf ts exp1
        sndCon = evaluateSem tf ts exp2

semEvalCond tf ts (IsLessE exp1 exp2)  = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error "Type mismatch in bool expression."
	where
        fstCon = evaluateSem tf ts exp1
        sndCon = evaluateSem tf ts exp2

semEvalCond tf ts (IsGreatE exp1 exp2) = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error "Type mismatch in bool expression."
	where
        fstCon = evaluateSem tf ts exp1
        sndCon = evaluateSem tf ts exp2

-- boolean expression type evaluation functions for the function body
semFceEvalCond :: String -> FunctionTable -> SymbolTable -> SymbolTable -> BoolExpr -> PasTypes
semFceEvalCond id tf gt lt (Equal exp1 exp2)    = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error ("Type mismatch in bool expression in function: " ++ id)
	where
        fstCon = fceEvalSem id tf gt lt exp1
        sndCon = fceEvalSem id tf gt lt exp2

semFceEvalCond id tf gt lt (NEqual exp1 exp2)   = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error ("Type mismatch in bool expression in function: " ++ id)
	where
        fstCon = fceEvalSem id tf gt lt exp1
        sndCon = fceEvalSem id tf gt lt exp2

semFceEvalCond id tf gt lt (IsLess exp1 exp2)   = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error ("Type mismatch in bool expression in function: " ++ id)
	where
        fstCon = fceEvalSem id tf gt lt exp1
        sndCon = fceEvalSem id tf gt lt exp2

semFceEvalCond id tf gt lt (IsGreat exp1 exp2)  = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error ("Type mismatch in bool expression in function: " ++ id)
    where
        fstCon = fceEvalSem id tf gt lt exp1
        sndCon = fceEvalSem id tf gt lt exp2

semFceEvalCond id tf gt lt (IsLessE exp1 exp2)  = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error ("Type mismatch in bool expression in function: " ++ id)
	where
        fstCon = fceEvalSem id tf gt lt exp1
        sndCon = fceEvalSem id tf gt lt exp2

semFceEvalCond id tf gt lt (IsGreatE exp1 exp2) = do
    if((fstCon == sndCon)) then
		fstCon
	else if (((fstCon == PasDbl) || (sndCon == PasDbl)) && (((fstCon /= PasStr)) || (sndCon /= PasStr))) then
		PasDbl
	else
		error ("Type mismatch in bool expression in function: " ++ id)
	where
        fstCon = fceEvalSem id tf gt lt exp1
        sndCon = fceEvalSem id tf gt lt exp2        
