{-
@file: 		parser.hs
@author: 	Stanislav Laznicka <xlazni08@stud.fit.vutbr.cz>
-}

module PascalParser ( parsePascal )  where
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
	,	opLetter		= oneOf "+-=.:"
	,	reservedOpNames	= [ ":=", "+", "*", "." ]
	,	reservedNames 	= [	"begin", "div", "do", "double", "else", "end",
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
	deriving Show

data Expr = IConst Int
	| Var String
	| Add Expr Expr
	| Sub Expr Expr
	| Mult Expr Expr
	| Div Expr Expr
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
	return (vars, absyntree)
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
    <?> "cmd"

expr = buildExpressionParser operators term where
	operators = [
			[ op "*" Mult, op "/" Div ],
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
	<?> "term error"

parsePascal input file =
	case parse pascalp file input of
		Left e -> error "parse"
		Right absyntree -> absyntree

--main = do
--	args <- getArgs
--	if length args /= 1
--		then error "Wrong number of arguments."
--		else do
--			let fileName = args !! 0
--			input <- readFile $ fileName
--			let absyntree = parsePascal input fileName
--			print absyntree