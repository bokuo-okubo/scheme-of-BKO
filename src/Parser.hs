module Parser
    (
    readExpr
  , parseExpr
    ) where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex


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
readExpr :: String -> Parser LispVal -> String
readExpr input parser = case parse parser "lisp" input of
  Left err -> "No match" ++ show err
  Right _  -> "Found value"


parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseBool
        <|> parseCharacter
        <|> parseQuoted
        <|> do
          _ <- char '('
          x <- try parseList <|> parseDottedList
          _ <- char ')'
          return x

{-
台数的データ型の一例
LispVal型の変数が持つことのできる値の集合を定めている
選択肢のそれぞれ(コンストラクタ、と呼ばれ、| で区切られる)は、
、コンストラクタのタグとそのコンストラクタが持つことのできるデータの型を含む
この例では、LispValは次のどれか
  1. Atom - そのアトムの示す文字列を格納します。
  2. List - 他のLispValのリストを保持します(Haskellのリストは角括弧で表されます)。properリストとも呼ばれます。
  3. DottedList - Schemeの(a b . c)を表し、improperリストとも呼ばれます。これは最後以外全ての要素のリストを持ち、最後の要素を別に格納します。
  4. Number - Haskellの整数を保持します。
  5. String - Haskellの文字列を保持します。
  6. Bool - Haskellの真偽値を保持します。
-}
data LispVal =  Atom        String
              | List       [LispVal]
              | DottedList [LispVal] LispVal
              | Number      Integer
              | Float       Double
              | Complex    (Complex Double)
              | Ratio       Rational
              | Bool        Bool
              | String      String
              | Character   Char

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


toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) = realToFrac n

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
parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal
  _ <- char '+'
  y <- try parseFloat <|> parseDecimal
  _ <- char 'i'
  return $ Complex (toDouble x :+ toDouble y)
