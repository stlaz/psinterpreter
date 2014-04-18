{-
  	@file: Interpreter.hs
	@authors: 
        Stanislav Laznicka  <xlazni08@stud.fit.vutbr.cz>
        Petr Kubat          <xkubat11@stud.fit.vutbr.cz>
	
	@brief: This file describes some common structures and functions for
			the whole interpreter
-}

module Commons (PasTypes(..), Command(..), Expr(..), Functions(Function),
			BoolExpr(..), SymbolTable, FunctionTable, Symbol, fillSymbols,
			fillFunc, emptyIOSym, emptyIOST, emptyST, emptySym, emptyFunc,
			emptyFuncDef, getType, getInt, getDbl, getStr, getFnc,
			getFncCom, getFncParams, getFncLocvars, setNone, setInt,
			setDbl,	setStr, setFnc, binTypes, get, set, chkSymTables,
			chkFuncDefs, chkFncTables, getIndex, getFncDef, getFncRet)
			where

-- Data types of the given language
data PasTypes = PasNone | PasInt | PasDbl | PasStr | PasFunc
	deriving (Show, Eq, Ord)

-- Possible commands of the language
data Command = Empty
	| Assign String Expr
	| Writeln Expr
	| Readln String
	| Seq [ Command ]
	| If BoolExpr Command Command
	| While BoolExpr Command
	| Expr Expr
	deriving (Show, Eq)

-- Function identifier [(parameters)] type [local variables] function_body
data Functions = Function String [ (String, PasTypes) ] PasTypes [ (String, PasTypes) ] Command
	deriving Show

-- Possible expressions of the language
data Expr = IConst Int
	| SConst String
	| DConst Double
	| Var String
	| Add Expr Expr
	| Sub Expr Expr
	| Mult Expr Expr
	| Div Expr Expr
	| Pars Expr 		-- parens
	| FuncCall String [ Expr ]
	deriving (Show, Eq)

data BoolExpr = Equal Expr Expr
	| NEqual Expr Expr
	| IsGreat Expr Expr
	| IsLess Expr Expr
	| IsGreatE Expr Expr
	| IsLessE Expr Expr
	deriving (Show, Eq)

type SymbolTable = [(String, Symbol)]
type FunctionTable = [(String, Symbol)]
type Symbol = (PasTypes, Int, Double, String, (PasTypes, [ (String, PasTypes) ], [ (String, PasTypes) ], Command ))

get :: SymbolTable -> String -> Symbol
get [] _ = emptySym
get (s@(var, val):ss) v =
	if v == var
		then val
		else get ss v

set :: SymbolTable -> String -> Symbol -> SymbolTable
set [] var val = [(var, val)]
set (s@(v,_):ss) var val =
	if v == var	then (var, val):ss
		else s : set ss var val

-- looks up a function declaration in a symbol table and returns it if the name matches
getFncDec :: SymbolTable -> String -> Symbol
getFncDec [] _ = emptySym
getFncDec (s@(var, val):ss) v =
	if ((v == var) && ((getFncCom val) == Empty))
		then val
		else getFncDec ss v

-- looks up a function definition in a symbol table and returns it if the name matches
getFncDef :: SymbolTable -> String -> Symbol
getFncDef [] _ = emptySym
getFncDef (s@(var, val):ss) v =
	if ((v == var) && ((getFncCom val) /= Empty))
		then val
		else getFncDef ss v

-- Fills a symbol table from the structure returned by parser
-- Adds a null variable for interpreter purposes
fillSymbols [] = [("000", emptySym)]
fillSymbols (vh:tail) =
	if snd vh == PasInt then
		(fst vh, (setInt 0)):(fillSymbols tail)
	else if snd vh == PasDbl then
		(fst vh, (setDbl 0.0)):(fillSymbols tail)
	else if snd vh == PasStr then
		(fst vh, (setStr "")):(fillSymbols tail)
	else error "Unknown variable type.\n"

-- Fills a symbol table for functions
fillFunc [] = []
fillFunc ((Function name pars t locals coms):tail) =
	(name, (setFnc name t pars locals coms)):(fillFunc tail)

-- returns the index of an symbol table element in a symbol table
getIndex _ _ [] = 0
getIndex count name (table:tables) =
    if (name == (fst table)) then
        count
    else
        getIndex (count + 1) name tables

-- checks declarations for mismatches and mising definitions
chkFuncDefs tf [] = tf
chkFuncDefs tf (table:tables) =
    if (getFncDec tf (fst table) == emptySym) then
        -- def
        chkFuncDefs tf tables
    else
        if (getFncDef tf (fst table) == emptySym) then
            error ("Declaration without definition: " ++ (fst table))
        else if (getFncParams (getFncDec tf (fst table)) /= getFncParams (getFncDef tf (fst table))) then
            error ("Function declaration doesnt match the function definition: " ++ (fst table))
        else if (getFncRet (getFncDec tf (fst table)) /= getFncRet (getFncDef tf (fst table))) then
            error ("Function declaration doesnt match the function definition: " ++ (fst table))
        else
            chkFuncDefs tf tables

-- checks the function symbol table for multiple declarations/definitions
chkFncTables :: SymbolTable -> PasTypes
chkFncTables [] = PasNone
chkFncTables (table:tables) = do
    if (((getFncCom $ snd table) == Empty)) then
        if ((getFncDec tables (fst table)) == emptySym) then
            chkFncTables tables
        else 
            error "Multiple function declarations of the same name."
    else
        if ((getFncDef tables (fst table)) == emptySym) then
            chkFncTables tables
        else
            error "Multiple function definitions of the same name."

-- checks the symbol table for multiple definitions
-- also checks for matches with the function definitions 
chkSymTables [] _ = []
chkSymTables ftl@(lt:lts) tf =
    if ((getType (get (chkSymTables lts tf) (fst lt))) == PasNone) then
        if ((getType (get tf (fst lt))) == PasNone) then
            ftl
        else
            error ("Variable has the same name as a function: " ++ (fst lt))
    else
        error ("Multiple definition of the same variable: " ++ (fst lt))


-- Some useful functions follow, mostly to set/create a symbol
emptyIOSym :: IO Symbol
emptyIOSym = do
	return emptySym

emptyIOST :: IO SymbolTable
emptyIOST = do
	return [("", emptySym)]

emptyST :: SymbolTable
emptyST = [("", emptySym)]

emptySym :: Symbol
emptySym = (PasNone, 0, 0.0, "", emptyFuncDef)

emptyFunc :: Symbol
emptyFunc = setFnc "" PasNone [] [] Empty

emptyFuncDef = (PasNone, [], [], Empty)

fst''  (x,_,_,_,_) = x
snd''  (_,x,_,_,_) = x
trd''  (_,_,x,_,_) = x
frth'' (_,_,_,x,_) = x
ffth'' (_,_,_,_,x) = x

getType = fst''
getInt :: Symbol -> Int
getInt = snd''
getDbl :: Symbol -> Double
getDbl = trd''
getStr = frth''
getFnc = ffth''

getFncRet :: Symbol -> PasTypes
getFncRet (_,_,_,_,(x,_,_,_)) = x

getFncCom :: Symbol -> Command
getFncCom (_,_,_,_,(_,_,_,x)) = x

getFncParams :: Symbol -> [(String, PasTypes)]
getFncParams (_,_,_,_,(_,x,_,_)) = x

getFncLocvars :: Symbol -> [(String, PasTypes)]
getFncLocvars (_,_,_,_,(_,_,x,_)) = x

setNone = emptySym
setInt num = (PasInt, num, 0.0, "", emptyFuncDef)
setDbl num = (PasDbl, 0, num, "", emptyFuncDef)
setStr str = (PasStr, 0, 0.0, str, emptyFuncDef)
setFnc name t pars locals coms = (PasFunc, 0, 0.0, name, (t, pars, locals, coms))

-- This function returns a number that depends on the type of two symbols given to it
binTypes x y
	| (firstType == PasInt) && (secondType == PasInt) = 1		-- Int Int
	| (firstType == PasInt) && (secondType == PasDbl) = 2		-- Int Dbl
	| (firstType == PasDbl) && (secondType == PasInt) = 3		-- Dbl Int
	| (firstType == PasDbl) && (secondType == PasDbl) = 4		-- Dbl Dbl
	| (firstType == PasStr) && (secondType == PasStr) = 5		-- String String
	| (firstType == PasFunc) && (secondType == PasFunc) = 6		-- Function Function
	| (firstType == PasFunc) = 7
	| (secondType == PasFunc) = 8
	where
		firstType = getType x
		secondType = getType y
