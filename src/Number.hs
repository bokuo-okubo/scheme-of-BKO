module Number
    (
    parseNumber
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex

import LispType

-- Parse Numbers
-- |
-- >>> readExpr "123"
-- "Found value"
parseNumber :: Parser LispVal
parseNumber = parseDecimal
          <|> parseHex
          <|> parseOct
          <|> parseBin
          <|> parseRatio
          <|> parseComplex

parseDecimal = parseDecimal1 <|> parseDecimal2

parseDecimal1 :: Parser LispVal
parseDecimal1 = liftM (Number . read) $ many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  _ <- try $ string "#d"
  x <- many1 digit
  return $ Number $ read x

parseHex :: Parser LispVal
parseHex = do
  _ <- try $ string "#x"
  x <- many1 hexDigit
  return $ Number $ hex2dec x

-- |
-- >>> readExpr "#o12345"
-- "Found value"
parseOct :: Parser LispVal
parseOct = do
  _ <- try $ string "#o"
  x <- many1 octDigit
  return $ Number $ oct2dec x

-- |
-- >>> readExpr "#b1010"
-- "Found value"
parseBin :: Parser LispVal
parseBin = do
  _ <- try $ string "#b"
  x <- many1 $ oneOf "01"
  return $ Number $ bin2dec x

hex2dec :: String -> Integer
hex2dec x = fst $ head $ readHex x

oct2dec :: String -> Integer
oct2dec x = fst $ head $ readOct x

bin2dec :: String -> Integer
bin2dec = bin2dec' 0
  where
    bin2dec' digint "" = digint
    bin2dec' digint (x:xs) =
      let old = 2 * digint + (if x == '0' then 0 else 1) in
        bin2dec' old xs

-- |
-- >>> readExpr "10.123"
-- "Found value"
parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  return $ Float $ fst . head $ readFloat $ x ++ "." ++ y

-- |
-- >> readExpr "/ 3 4"
-- "Found value"
parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  _ <- char '/'
  y <- many1 digit
  return $ Ratio $ read x % read y

{- https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Complex.html -}

-- |
-- >> readExpr "44.2+33i"
-- "Found value"

{- on lisp
> (+ 12+1i 12+1i)
24+2i
>
-}

toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) = realToFrac n

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal
  _ <- char '+'
  y <- try parseFloat <|> parseDecimal
  _ <- char 'i'
  return $ Complex (toDouble x :+ toDouble y)
