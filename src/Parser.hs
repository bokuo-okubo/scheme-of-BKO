module Parser
    (
    readExpr
  , parseExpr
  , module X
    ) where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array

import Number as X
import LispType as X
import Eval as X

-- | @parse p filePath input@ runs a parser @p@ over Identity without user
-- state. The @filePath@ is only used in error messages and may be the
-- empty string. Returns either a 'ParseError' ('Left')
-- or a value of type @a@ ('Right').
--
-- >  main    = case (parse numbers "" "11, 2, 43") of
-- >             Left err  -> print err
-- >             Right xs  -> print (sum xs)
-- >
-- >  numbers = commaSep integer

-- parse :: (Stream s Identity t)
--       => Parsec s () a -> SourceName -> s -> Either ParseError a
-- parse p = runP p ()

-- |
-- >>> readExpr "hoge"
-- "Found value"
readExpr :: String -> Parser LispVal -> LispVal
readExpr input parser = case parse parser "lisp" input of
  Left  err -> String $ "No match: " ++ show err
  Right val -> val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseBool
        <|> parseCharacter
        <|> parseQuoted
        <|> parseQuasiQuated
        <|> parseUnQuote
        <|> parseVector
        <|> parseLists

-- 引数で与えられた文字列中のどれか一文字を 認識する
--
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

{-
lispのAtom(それ以上分割できないデータ型)をパースする
-}
-- >>> readExpr "a!fdsafdsa" parseAtom
-- "Found value"
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ Atom atom

escapeChars :: Parser Char
escapeChars = do
  _ <- char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _   -> x

-- >>>readExpr "\"abc\"" parseString
-- "Found value"
parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many $ escapeChars <|> noneOf("\"" ++ "\\")
  _ <- char '"'
  return $ String x

-- >>> readExpr "#\\j" parseCharacter
-- "Found value"
-- >>> readExpr "#\\newline" parseCharacter
-- "Found value"
-- >>> readExpr "#\\space" parseCharacter
-- "Found value"
parseCharacter :: Parser LispVal
parseCharacter = do
  _     <- try $ string "#\\"
  value <- try (string "newline" <|> string "space")
          <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
  return $ Character $ case value of
    "space"   -> ' '
    "newline" -> '\n'
    _         -> head value

-- >>> readExpr "#t" parseBool
-- "Found value"
-- >>> readExpr "#f" parseBool
-- "Found value"
parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False)) -- なんか微妙

parseLists :: Parser LispVal
parseLists = do
  _ <- char '('
  x <- try parseList <|> parseDottedList
  _ <- char ')'
  return x

-- >>>  readExpr  "(a test)" parseExpr
-- "Found value"
-- >>>  readExpr  "(a (nested) test)" parseExpr
-- "Found value"
-- >>>  readExpr  "(a (dotteed . list) test)" parseExpr
-- "Found value"
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]



-- >>> readExpr "`(list val val)" parseQuasiQuated
-- "Found value"
parseQuasiQuated :: Parser LispVal
parseQuasiQuated = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

-- >>> readExpr ",(list val val)" parseUnQuote
-- "Found value"
parseUnQuote :: Parser LispVal
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- >>> readExpr "#(1 2 3)" parseVector
-- "Found value"
parseVector :: Parser LispVal
parseVector = do
  _ <- string "#("
  x <- try parseVector'
  _ <- char ')'
  return x

parseVector' :: Parser LispVal
parseVector' = do
  arrayValues <- sepBy parseExpr spaces
  return $ Vector $ listArray (0, length arrayValues - 1 ) arrayValues


{-
式の列
む または ドットと一つの指揮
-}
