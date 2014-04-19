{-
    Project: FLP-2014-Pascal

    @file: Interpreter.hs
    @authors: 
        Stanislav Laznicka  <xlazni08@stud.fit.vutbr.cz>
        Petr Kubat          <xkubat11@stud.fit.vutbr.cz>
-}

module Main( main ) where

import System.Environment( getArgs )

import Interpreter
import Commons
import PascalParser
import SemCheck

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
            if ( (getType (get (chkSymTables symTable funcTable) "000" )) /= PasNone) then
                error "Error when checking global table."
            else if (chkFncTables funcTable /= PasNone) then
                error "Error when checking function table."
            else if ((getType (get (chkFuncDefs funcTable funcTable) "000")) /= PasNone) then
                error "Error when checking function defs and decs."
            else if ((chkFunctions symTable funcTable funcTable) == PasNone) then do
                semantic funcTable symTable (trd' absyntree)
                newsym <- interpret funcTable symTable (trd' absyntree)
                return newsym
				else
					error "Function semantic failure."
