module Commons (PasTypes(..), Command(..), Expr(..), Functions(Function),
			BoolExpr(..), SymbolTable, FunctionTable, Symbol, fillSymbols,
			fillFunc, emptyIOSym, emptyIOST, emptySym, emptyFunc,
			emptyFuncDef, getType, getInt, getDbl, getStr, getFnc,
			getFncCom, setNone, setInt, setDbl,	setStr, setFnc, binTypes,
			get, set, chkSymTables ) where

data PasTypes = PasNone | PasInt | PasDbl | PasStr | PasFunc
	deriving (Show, Eq, Ord)

data Command = Empty 	-- this should describe the program structure
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
	| BPars BoolExpr
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

fillSymbols [] = [("000", emptySym)]
fillSymbols (vh:tail) =
	if snd vh == PasInt then
		(fst vh, (setInt 0)):(fillSymbols tail)
	else if snd vh == PasDbl then
		(fst vh, (setDbl 0.0)):(fillSymbols tail)
	else if snd vh == PasStr then
		(fst vh, (setStr "")):(fillSymbols tail)
	else error "Unknown variable type.\n"

fillFunc [] = []
fillFunc ((Function name pars t locals coms):tail) =
	(name, (setFnc name t pars locals coms)):(fillFunc tail)

chkSymTables [] _ = []
chkSymTables ftl@(lt:lts) tf =
    if ((getType (get (chkSymTables lts tf) (fst lt))) == PasNone) then
        if ((getType (get tf (fst lt))) == PasNone) then
            ftl
        else
            error "Variable has the same name as a function."
    else
        error "Multiple definition of the same variable."

emptyIOSym :: IO Symbol
emptyIOSym = do
	return emptySym

emptyIOST :: IO SymbolTable
emptyIOST = do
	return [("", emptySym)]

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
