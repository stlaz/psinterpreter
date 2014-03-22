{-
@file: 		parser.hs
@author: 	Stanislav Laznicka <xlazni08@stud.fit.vutbr.cz>
-}

module PascalParser ( Command(..), Expr(..), BoolExpr(..), parsePascal )  where
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
							"while", "writeln" ]
	}

lexal = P.makeTokenParser tokDef

whiteSpace	= P.whiteSpace lexal
integer		= P.integer lexal
parens 		= P.parens lexal
semi		= P.semi lexal
identifier	= P.identifier lexal
reserved	= P.reserved lexal
reservedOp	= P.reservedOp lexal
dot 		= P.dot lexal
comma 		= P.comma lexal
colon		= P.colon lexal

data Command = Empty 	-- this should describe the program structure
	| Assign String Expr
	| Writeln Expr
	| Seq [ Command ]
	| If BoolExpr Command Command
	| While BoolExpr Command
	deriving Show

data Expr = IConst Int
	| Var String
	| Add Expr Expr
	| Sub Expr Expr
	| Mult Expr Expr
	| Div Expr Expr
	| Pars Expr
	deriving Show

data BoolExpr = Equal Expr Expr
	| NEqual Expr Expr
	| IsGreat Expr Expr
	| IsLess Expr Expr
	| IsGreatE Expr Expr
	| IsLessE Expr Expr
	| BPars BoolExpr
	deriving Show

-- starting non-terminal, removes all spaces and comments at the start of the file
pascalp = do
	whiteSpace
	vars <- option [] variables
	reserved "begin"
	absyntree <- (many cmd)	-- cmd is the actual parsing function
	reserved "end"
	dot
	eof						-- EOF should occur after parsing the whole file
	return (vars, Seq absyntree)
	<?> "pascalp"


variables = do 		-- dodelat pro double
	reserved "var"
	v <- parseVariable `sepBy1` comma
	semi
	return v
	--option comma

parseVariable = do
	v <- identifier
	colon
	reserved "integer"
	return (v, "integer")

-- This function deals with commands in the body of the programme and in functions
cmd = do
		semi
		return Empty
    <|> do 
        v <- identifier
        reservedOp ":="
        e <- expr
        semi
        return (Assign v e)
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
    <?> "cmd"

expr = buildExpressionParser operators term where
	operators = [
			[ op "*" Mult, op "div" Div ],
			[ op "+" Add, op "-" Sub ]
		]
	op name func = 
		Infix (do { reservedOp name; return func }) AssocLeft

term = 
	do
		i <- integer
		return (IConst $ fromInteger i)
	<|> do
		v <- identifier
		return (Var v)
	<|> do
		e <- parens expr
		return (Pars e)
	<?> "term error"

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