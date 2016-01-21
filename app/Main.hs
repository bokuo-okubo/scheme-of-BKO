module Main where

import Text.ParserCombinators.Parsec
import Parser
import System.Environment

{-
parsecをリンクさせるためにbuildは
$ ghc -package parsec -o simple_parser simple-parser.hs
-}
main :: IO ()
main = do args <- getArgs
          putStrLn $ "arg: " ++ show args ++ "\n"
          putStrLn (readExpr (head args) parseExpr)
