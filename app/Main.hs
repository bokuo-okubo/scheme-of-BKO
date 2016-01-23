module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import Parser

{-
parsecをリンクさせるためにbuildは
---- $ ghc -package parsec -o simple_parser simple-parser.hs
-}
main :: IO ()
main = do args <- getArgs
          putStrLn $ "arg: " ++ show args ++ "\n"
          print(eval (readExpr (head args) parseExpr))
