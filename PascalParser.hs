{-
@file: 		parser.hs
@author: 	Stanislav Laznicka <xlazni08@stud.fit.vutbr.cz>
-}

module PascalParser ( Command(..), Functions(), Expr(..),
					 BoolExpr(..), parsePascal, fst',snd',trd',
					 PasTypes(..) )  where
-- module Main ( main ) where

import System.IO

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

tokDef = emptyDef
	{
		commentStart 	= "{"
	,	commentEnd		= "}"
	,	identStart		= letter <|> char '_'
	,	identLetter		= alphaNum <|> char '_'
	,	opStart 		= opLetter emptyDef
	,	opLetter		= oneOf "+-=:div<>"
	,	reservedOpNames	= [ ":=", "+", "*", "div", "=", "<>" ]
	,	reservedNames 	= [	"begin", "do", "double", "else", "end",
							"if", "integer", "readln", "string", "then", "var",
							"while", "writeln", "function" ]
	}

lexal = P.makeTokenParser tokDef

whiteSpace		= P.whiteSpace lexal
integer			= P.integer lexal
stringLiteral	= P.stringLiteral lexal
double 			= P.float lexal
parens 			= P.parens lexal
semi			= P.semi lexal
identifier		= P.identifier lexal
reserved		= P.reserved lexal
reservedOp		= P.reservedOp lexal
dot 			= P.dot lexal
comma 			= P.comma lexal
colon			= P.colon lexal

data Command = Empty 	-- this should describe the program structure
	| Assign String Expr
	| Writeln Expr
	| Readln String
	| Seq [ Command ]
	| If BoolExpr Command Command
	| While BoolExpr Command
	| Expr Expr
	deriving Show

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
	deriving Show

data BoolExpr = Equal Expr Expr
	| NEqual Expr Expr
	| IsGreat Expr Expr
	| IsLess Expr Expr
	| IsGreatE Expr Expr
	| IsLessE Expr Expr
	| BPars BoolExpr
	deriving Show

data PasTypes = PasNone | PasInt | PasDbl | PasStr
	deriving (Show, Eq, Ord)

-- starting non-terminal, removes all spaces and comments at the start of the file
pascalp = do
	whiteSpace
	vars <- option [] variables
	functionDeclares <- many parseFuncBody
	reserved "begin"
	absyntree <- (many cmd)	-- cmd is the actual parsing function
	reserved "end"
	dot
	eof						-- EOF should occur after parsing the whole file
	return (vars, functionDeclares, Seq absyntree)
	<?> "pascalp"


fst' (x, _, _) = x
snd' (_, x, _) = x
trd' (_, _, x) = x

variables = do 		-- dodelat pro double
	reserved "var"
	v <- parseVariable `sepBy1` comma  -- possible project assignment mistake here!!!
	semi
	return v
	--option comma

parseVariable = do
	v <- identifier
	colon
	name <- resName
	return (v, name)
	where
		resName =
				rn' "integer"
			<|> rn' "double"
			<|> rn' "string"
			<?> "parseVariable"
			where rn' typeof = do
				reserved typeof
				if typeof == "integer" then do
					return PasInt
				else if typeof == "double" then do
					return PasDbl
				else do
					return PasDbl


parseFuncBody = do
	reserved "function"
	id <- identifier
	params <- parens (parseVariable `sepBy1` comma)
	colon
	t <- resName
	semi	
	vars <- option [] variables
	coms <- option Empty cmd
	return (Function id params t vars coms)
	where
		resName =
				rn' "integer"
			<|> rn' "double"
			<|> rn' "string"
			<?> "parseVariable"
			where rn' typeof = do
				reserved typeof
				if typeof == "integer" then do
					return PasInt
				else if typeof == "double" then do
					return PasDbl
				else do
					return PasDbl

-- This function deals with commands in the body of the programme and in functions
cmd = do
		semi
		return Empty
    <|> do 
        try parseAssignment
    <|> do
    	reserved "begin"
    	commands <- (many cmd)
    	reserved "end"
    	return (Seq commands)
    <|> do
    	reserved "writeln"
    	e <- parens expr
    	semi
    	return (Writeln e)
    <|> do
    	reserved "readln"
    	id <- parens identifier
    	semi
    	return (Readln id)
    <|> do
    	reserved "if"
    	cond <- boolExpr
    	reserved "then"
    	coms1 <- cmd
    	reserved "else"
    	coms2 <- cmd
    	return (If cond coms1 coms2)
    <|> do
    	reserved "while"
    	cond <- boolExpr
    	reserved "do"
    	coms <- cmd
    	return (While cond coms)
   	<|> do
   		e <- expr
   		semi
   		return (Expr e)
    <?> "cmd"

parseAssignment = do
        v <- identifier
        reservedOp ":="
        e <- expr
        semi
        return (Assign v e)



expr = buildExpressionParser operators term where
	operators = [
			[ op "*" Mult, op "div" Div ],
			[ op "+" Add, op "-" Sub ]
		]
	op name func = 
		Infix (do { reservedOp name; return func }) AssocLeft

term = 
	do
		try(parseDouble)
	<|> do		
		try(parseInteger)
	<|> do
		s <- stringLiteral
		return (SConst s)
	<|> do
		try parseIdExpr
	<|> do
		e <- parens expr
		return (Pars e)
	<|> do
		try parseFuncExpr
	<?> "term error"

parseDouble = do
		sign <- option 1 (do s <- oneOf "+-"
                                     return $ if s == '-' then-1.0 else 1.0)
		d <- double
		return (DConst $ fromRational sign*d)

parseInteger = do
		sign <- option 1 (do s <- oneOf "+-"
                                     return $ if s == '-' then-1 else 1)
		i <- integer
		return (IConst $ fromInteger $ sign*i)

parseFuncExpr = do
	id <- identifier
	params <- parens $ option [] $ expr `sepBy1` comma
	return (FuncCall id params)

parseIdExpr = do
		v <- identifier
		notFollowedBy $ parens $ option [] $ expr `sepBy1` comma
		return (Var v)

boolExpr =
	do
		e1 <- expr
		v <- cmpOp
		e2 <- expr
		return (v e1 e2)
	<?> "boolExpr error."
	where
		cmpOp = resOp "=" Equal
			<|> resOp "<>" NEqual
			<|> resOp "<" IsLess
			<|> resOp ">" IsGreat
			<|> resOp "<=" IsLessE
			<|> resOp ">=" IsGreatE
			<?> "resOp error."
			where
				resOp name cls = do
					reservedOp name
					return cls


parsePascal input file =
	case parse pascalp file input of
		Left e -> error "Parser error."
		Right absyntree -> absyntree