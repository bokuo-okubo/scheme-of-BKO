module LispType
    (
    LispVal(..)
  , Array
    ) where
import Data.Ratio
import Data.Complex
import Data.Array

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
              | String      String
              | Character   Char
              | Bool        Bool
              | Number      Integer
              | Float       Double
              | Complex    (Complex Double)
              | Ratio       Rational
              | Vector     (Array Int LispVal)
              | List       [LispVal]
              | DottedList [LispVal] LispVal
