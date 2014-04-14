module Main ( main ) where

import System.Environment( getArgs )

import Commons
import PascalParser
--import SemCheck

data Operation = Plus | Minus | Times | Divide

{-
set (s@(v,u):ss) var val =
	if v == var	then
		if stype = stypeNew then
	else s : set ss var val
	where
		stype = getType u
		stypeNew = getType val
-}

--getF :: FunctionTable -> String -> 	 ([ (String, PasTypes) ], PasTypes, [ (String, PasTypes) ], Command)
--getF [] _ = error "Not found"
--getF (s@(var, val):ss) v =
--	if v == var
--		then val
--		else getF ss v

interFnc :: FunctionTable -> SymbolTable -> String -> IO SymbolTable
interFnc tf ts name = do
	symtab <- interpret tf ts $ getFncCom $ get tf name
	return $ [(name, get symtab name)] ++ symtab

evaluate :: FunctionTable -> SymbolTable -> Expr -> (Symbol, IO SymbolTable)
evaluate tf ts (IConst c) = (setInt c,emptyIOST)
evaluate tf ts (DConst c) = (setDbl c,emptyIOST)
evaluate tf ts (SConst s) = (setStr s,emptyIOST)
evaluate tf ts (FuncCall name args) = (emptyFunc, interFnc tf ts name)
evaluate tf ts (Var v) = (get ts v, emptyIOST)
-- TODO: What happens when exp1 or exp2 each other evaluate to different type?
evaluate tf ts (Add exp1 exp2) = do
	case trinity of
		1 -> (setInt $ (getInt firstSym) + (getInt secondSym), emptyIOST)
		2 -> (setDbl $ (fromIntegral (getInt firstSym)) + (getDbl secondSym), emptyIOST)
		3 -> (setDbl $ (getDbl firstSym) + (fromIntegral (getInt secondSym)), emptyIOST)
		4 -> (setDbl $ (getDbl firstSym) + (getDbl secondSym), emptyIOST)
		5 -> (setStr $ (getStr firstSym) ++ (getStr secondSym), emptyIOST)
		6 -> (emptyFunc, evalFuncExpr tf ts 6 Plus first second)
		7 -> (emptyFunc, evalFuncExpr tf ts 7 Plus first second)
		8 -> (emptyFunc, evalFuncExpr tf ts 8 Plus first second)
		_ -> (emptySym, emptyIOST)
	where
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		trinity = binTypes firstSym secondSym

evaluate tf ts (Sub exp1 exp2) = do
	case trinity of
		1 -> (setInt $ (getInt firstSym) - (getInt secondSym), emptyIOST)
		2 -> (setDbl $ (fromIntegral (getInt firstSym)) - (getDbl secondSym), emptyIOST)
		3 -> (setDbl $ (getDbl firstSym) - (fromIntegral (getInt secondSym)), emptyIOST)
		4 -> (setDbl $ (getDbl firstSym) - (getDbl secondSym), emptyIOST)
		6 -> (emptyFunc, evalFuncExpr tf ts 6 Minus first second)
		7 -> (emptyFunc, evalFuncExpr tf ts 7 Minus first second)
		8 -> (emptyFunc, evalFuncExpr tf ts 8 Minus first second)
		_ -> (emptySym, emptyIOST)
	where
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		trinity = binTypes firstSym secondSym

evaluate tf ts (Mult exp1 exp2) = do
	case trinity of
		1 -> (setInt $ (getInt firstSym) * (getInt secondSym), emptyIOST)
		2 -> (setDbl $ (fromIntegral (getInt firstSym)) * (getDbl secondSym), emptyIOST)
		3 -> (setDbl $ (getDbl firstSym) * (fromIntegral (getInt secondSym)), emptyIOST)
		4 -> (setDbl $ (getDbl firstSym) * (getDbl secondSym), emptyIOST)
		6 -> (emptyFunc, evalFuncExpr tf ts 6 Times first second)
		7 -> (emptyFunc, evalFuncExpr tf ts 7 Times first second)
		8 -> (emptyFunc, evalFuncExpr tf ts 8 Times first second)
		_ ->(emptySym, emptyIOST)
	where
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		trinity = binTypes firstSym secondSym

evaluate tf ts (Div exp1 exp2) = do
	case trinity of
		1 -> (setInt $ (getInt firstSym) `div` (getInt secondSym), emptyIOST)
		6 -> (emptyFunc, evalFuncExpr tf ts 6 Divide first second)
		7 -> (emptyFunc, evalFuncExpr tf ts 7 Divide first second)
		8 -> (emptyFunc, evalFuncExpr tf ts 8 Divide first second)
		_ -> (emptySym, emptyIOST)
	where
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		trinity = binTypes firstSym secondSym

evaluate tf ts (Pars exp) = evaluate tf ts exp

evalFuncExpr :: FunctionTable -> SymbolTable -> Int -> Operation  -> (Symbol, IO SymbolTable) -> (Symbol, IO SymbolTable)-> IO SymbolTable
-- I am so so sorry :(
evalFuncExpr tf ts binType op sym1 sym2 = do
	firsttab <- firstIO
	secondtab <- secondIO
	let symtail1 = tail firsttab
	let symtail2 = tail secondtab
	let fIO = fiIO firsttab
	let sIO = seIO secondtab
	if(binType == 6) then do
		case binTypes fIO sIO of
			1 -> case op of
				Plus -> return $ [("",(setInt $ (getInt fIO) + (getInt sIO)))] ++ symtail2
				Minus -> return $ [("",(setInt $ (getInt fIO) - (getInt sIO)))] ++ symtail2
				Times -> return $ [("",(setInt $ (getInt fIO) * (getInt sIO)))] ++ symtail2
				Divide -> return $ [("",(setInt $ (getInt fIO) `div` (getInt sIO)))] ++ symtail2
			2 -> case op of
				Plus -> return $ [("",(setDbl $ (fromIntegral $ getInt fIO) + (getDbl sIO)))] ++ symtail2
				Minus -> return $ [("",(setDbl $ (fromIntegral $ getInt fIO) - (getDbl sIO)))] ++ symtail2
				Times -> return $ [("",(setDbl $ (fromIntegral $ getInt fIO) * (getDbl sIO)))] ++ symtail2
				Divide -> return $ [("",emptySym)] ++ symtail2
			3 -> case op of
				Plus -> return $ [("",(setDbl $ (getDbl fIO) + (fromIntegral $ getInt sIO)))] ++ symtail2
				Minus -> return $ [("",(setDbl $ (getDbl fIO) - (fromIntegral $ getInt sIO)))] ++ symtail2
				Times -> return $ [("",(setDbl $ (getDbl fIO) * (fromIntegral $ getInt sIO)))] ++ symtail2
				Divide -> return $ [("",emptySym)] ++ symtail2
			4 -> case op of
				Plus -> return $ [("",(setDbl $ (getDbl fIO) + (getDbl sIO)))] ++ symtail2
				Minus -> return $ [("",(setDbl $ (getDbl fIO) - (getDbl sIO)))] ++ symtail2
				Times -> return $ [("",(setDbl $ (getDbl fIO) * (getDbl sIO)))] ++ symtail2
				Divide -> return $ [("",emptySym)] ++ symtail2
			5 -> case op of
				Plus -> return $ [("",(setStr $ (getStr fIO) ++ (getStr sIO)))] ++ symtail2
				_ -> return $ [("",emptySym)] ++ symtail2
			_ -> return $ [("",emptySym)] ++ symtail2	
	else if(binType == 7) then do
		case binTypes fIO secondSym of
			1 -> case op of
				Plus -> return $ [("",(setInt $ (getInt fIO) + (getInt secondSym)))] ++ symtail1
				Minus -> return $ [("",(setInt $ (getInt fIO) - (getInt secondSym)))] ++ symtail1
				Times -> return $ [("",(setInt $ (getInt fIO) * (getInt secondSym)))] ++ symtail1
				Divide -> return $ [("",(setInt $ (getInt fIO) `div` (getInt secondSym)))] ++ symtail1
			2 -> case op of
				Plus -> return $ [("",(setDbl $ (fromIntegral $ getInt fIO) + (getDbl secondSym)))] ++ symtail1
				Minus -> return $ [("",(setDbl $ (fromIntegral $ getInt fIO) - (getDbl secondSym)))] ++ symtail1
				Times -> return $ [("",(setDbl $ (fromIntegral $ getInt fIO) * (getDbl secondSym)))] ++ symtail1
				Divide -> return $ [("",emptySym)] ++ symtail1
			3 -> case op of
				Plus -> return $ [("",(setDbl $ (getDbl fIO) + (fromIntegral $ getInt secondSym)))] ++ symtail1
				Minus -> return $ [("",(setDbl $ (getDbl fIO) - (fromIntegral $ getInt secondSym)))] ++ symtail1
				Times -> return $ [("",(setDbl $ (getDbl fIO) * (fromIntegral $ getInt secondSym)))] ++ symtail1
				Divide -> return $ [("",emptySym)] ++ symtail1
			4 -> case op of
				Plus -> return $ [("",(setDbl $ (getDbl fIO) + (getDbl secondSym)))] ++ symtail1
				Minus -> return $ [("",(setDbl $ (getDbl fIO) - (getDbl secondSym)))] ++ symtail1
				Times -> return $ [("",(setDbl $ (getDbl fIO) * (getDbl secondSym)))] ++ symtail1
				Divide -> return $ [("",emptySym)] ++ symtail1
			5 -> case op of
				Plus -> return $ [("",(setStr $ (getStr fIO) ++ (getStr secondSym)))] ++ symtail1
				_ -> return $ [("",emptySym)] ++ symtail1
			_ -> return $ [("",emptySym)] ++ symtail1	
		else if(binType == 8) then do
		--print $ (show firstSym) ++ (show sIO)
		case binTypes firstSym sIO of
			1 -> case op of
				Plus -> return $ [("",(setInt $ (getInt firstSym) + (getInt sIO)))] ++ symtail2
				Minus -> return $ [("",(setInt $ (getInt firstSym) - (getInt sIO)))] ++ symtail2
				Times -> return $ [("",(setInt $ (getInt firstSym) * (getInt sIO)))] ++ symtail2
				Divide -> return $ [("",(setInt $ (getInt firstSym) `div` (getInt sIO)))] ++ symtail2
			2 -> case op of
				Plus -> return $ [("",(setDbl $ (fromIntegral $ getInt firstSym) + (getDbl sIO)))] ++ symtail2
				Minus -> return $ [("",(setDbl $ (fromIntegral $ getInt firstSym) - (getDbl sIO)))] ++ symtail2
				Times -> return $ [("",(setDbl $ (fromIntegral $ getInt firstSym) * (getDbl sIO)))] ++ symtail2
				Divide -> return $ [("",emptySym)] ++ symtail2
			3 -> case op of
				Plus -> return $ [("",(setDbl $ (getDbl firstSym) + (fromIntegral $ getInt sIO)))] ++ symtail2
				Minus -> return $ [("",(setDbl $ (getDbl firstSym) - (fromIntegral $ getInt sIO)))] ++ symtail2
				Times -> return $ [("",(setDbl $ (getDbl firstSym) * (fromIntegral $ getInt sIO)))] ++ symtail2
				Divide -> return $ [("",emptySym)] ++ symtail2
			4 -> case op of
				Plus -> return $ [("",(setDbl $ (getDbl firstSym) + (getDbl sIO)))] ++ symtail2
				Minus -> return $ [("",(setDbl $ (getDbl firstSym) - (getDbl sIO)))] ++ symtail2
				Times -> return $ [("",(setDbl $ (getDbl firstSym) * (getDbl sIO)))] ++ symtail2
				Divide -> return $ [("",emptySym)] ++ symtail2
			5 -> case op of
				Plus -> return $ [("",(setStr $ (getStr firstSym) ++ (getStr sIO)))] ++ symtail2
				_ -> return $ [("",emptySym)] ++ symtail2
			_ -> return $ [("",emptySym)] ++ symtail2	
	else return $ [("",emptySym)] ++ []
	where
		firstSym = fst sym1
		firstIO = snd sym1
		secondSym = fst sym2
		secondIO = snd sym2
		fiIO tab = snd $ head tab
		seIO tab = snd $ head tab

--evalList tf ts [] = []
--evalList tf ts (expr:tail) = (evaluate tf ts expr):(evalList tf ts tail)

evalCond :: FunctionTable -> SymbolTable -> BoolExpr -> (Bool, IO SymbolTable)
evalCond tf ts (Equal exp1 exp2)    = do
	if(types) < 5 then
		(evalBoolExpr (==) types firstSym secondSym, emptyIOST)
	else error "Incompatible types in boolean comparison"
	where 
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		types = binTypes firstSym secondSym

evalCond tf ts (NEqual exp1 exp2)   = do
	if(types) < 5 then
		(evalBoolExpr (/=) types firstSym secondSym, emptyIOST)
	else error "Incompatible types in boolean comparison"
	where 
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		types = binTypes firstSym secondSym

evalCond tf ts (IsLess exp1 exp2)   = do
	if(types) < 5 then
		(evalBoolExpr (<) types firstSym secondSym, emptyIOST)
	else error "Incompatible types in boolean comparison"
	where 
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		types = binTypes firstSym secondSym

evalCond tf ts (IsGreat exp1 exp2)  = do
	if(types) < 5 then
		(evalBoolExpr (>) types firstSym secondSym, emptyIOST)
	else error "Incompatible types in boolean comparison"
	where 
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		types = binTypes firstSym secondSym

evalCond tf ts (IsLessE exp1 exp2)  = do
	if(types) < 5 then
		(evalBoolExpr (<=) types firstSym secondSym, emptyIOST)
	else error "Incompatible types in boolean comparison"
	where 
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		types = binTypes firstSym secondSym

evalCond tf ts (IsGreatE exp1 exp2) = do
	if(types) < 5 then
		(evalBoolExpr (>=) types firstSym secondSym, emptyIOST)
	else error "Incompatible types in boolean comparison"
	where 
		first = evaluate tf ts exp1
		second = evaluate tf ts exp2
		firstSym = fst first
		secondSym = fst second
		types = binTypes firstSym secondSym

evalBoolExpr :: (Double -> Double -> Bool) -> Int -> Symbol -> Symbol -> Bool
evalBoolExpr f types s1 s2 = do
	case types of
		1 -> f (fromIntegral $ getInt s1) (fromIntegral $ getInt s2)
		2 -> f (fromIntegral $ getInt s1) (getDbl s2)
		3 -> f (getDbl s1) (fromIntegral $ getInt s2)
		4 -> f (getDbl s1) (getDbl s2)
		_ -> False

interpret :: FunctionTable -> SymbolTable -> Command -> IO SymbolTable
interpret tf ts Empty = return ts	-- Empty expression, simple
interpret tf ts (Assign var expr) = do
		symtab <- snd res
		if ((getType $ fst res) == PasFunc) then do
			let ts' = tail symtab
			return $ set ts' var $ snd $ head symtab
		else
			return $ set ts var $ fst res
	where 
		res = evaluate tf ts expr

interpret tf ts (Writeln expr) = do
	posSym <- snd midres 	-- this is probably the last spot to turn IO Symbol to Symbol here
	if((getType $ midresVal) == PasFunc) then do
		putStrLn $ id result $ snd $ head posSym  -- therefore we need to pass it on, in case Expr is of type PasFunc
		let ts' = tail posSym
		return ts'
	else do
		putStrLn $ id result $ fst midres  -- therefore we need to pass it on, in case Expr is of type PasFunc
		return ts
	where 
		midres = evaluate tf ts expr
		midresVal = fst midres
		argtype = getType $ fst midres
		result sym = do
			case argtype of
				PasInt -> show $ getInt midresVal
				PasDbl -> show $ getDbl midresVal
				PasStr -> getStr midresVal
				PasFunc -> do
					case getType sym of
						PasInt -> show $ getInt sym
						PasDbl -> show $ getDbl sym
						PasStr -> getStr sym
						_ -> "WritelnError"
				_ -> "WritelnError"
	
interpret tf ts (Readln id) = do
	val <- getLine
	return $ set ts id $ symval val
	where
		symval val = do
			case oldtype of
				PasInt -> setInt (read val :: Int)
				PasDbl -> setDbl (read val :: Double)
				PasStr -> setStr val
				_ -> (PasNone, 0, 0.0, "", emptyFuncDef)
			where
				oldtype = getType $ get ts id

interpret tf ts (Seq []) = return ts
interpret tf ts (Seq (com:coms)) = do
	ts' <- interpret tf ts com
	interpret tf ts' (Seq coms)
interpret tf ts (If cond coms1 coms2) = do
	if(condRes) then interpret tf ts coms1
		else interpret tf ts coms2
	where
		condRes = fst $ evalCond tf ts cond
--interpret tf ts (While cond coms) = do
--	if(evalCond tf ts cond) then do
--		ts' <- interpret tf ts coms
--		interpret tf ts' (While cond coms)
--	else return ts

interpret tf ts (Expr expr) = do
	sym <- snd $ res
	-- We need to pass the second part for possible prints and reads in function
	-- Lazy eval wouldn't care for them if this is done differently
	if((getType $ fst res) == PasFunc) then do
		let ts' = tail sym
		return $ set ts' "000" $ snd $ head sym
	else return $ set ts "000" $ fst res
	where
		res = evaluate tf ts expr

main = do
	args <- getArgs
	if length args /= 1
		then error "Wrong number of arguments."
		else do
			let fileName = args !! 0
			input <- readFile $ fileName
			let absyntree = parsePascal input fileName
			let symTable = fillSymbols (fst' absyntree)
			let funcTable = fillFunc $ snd' absyntree
			--chkFunctions symTable funcTable funcTable
			--print symTable
			print $ trd' absyntree
			--semantic funcTable symTable (trd' absyntree)
			newsym <- interpret funcTable symTable (trd' absyntree)
			print newsym
			--print $ snd' absyntree
			--print $ trd' absyntree
