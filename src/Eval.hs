module Eval
    (
    eval
    ) where
import LispType

{-
今の所、我々のパーサは与えられたプログラム片を認識するかしないかを出力していただけ
プログラム片に値を割り当てることによって機能するSchemeインタプリタに向かって最初の一歩を取ろうとしている
-}
-- パターンマッチは台数的データ型を分解し、そのコンストラクタに応じてコード節を選び、
-- それぞれの部分を変数に束縛する手立てになる
{-
unwordslistの定義は引数を含まない
これはpoint-freestyle, つまり関数定義を関数合成と部分適用のみで描く手法の例だ。

ここの値や"points"にかかずらわる代わりに、幾つかの組み込み関数の組み合わせを使う。

-> まず、mapをshowValに部分適用すると、
LispValのリストを取り、それらの文字列表現のリストを返す関数ができます。

haskell の関数はカリー化されている
kれは、mapのような二引数関数が実際は一引数関数を得ることができます
-}

-- 上で暑かったshowはどんな`Show`クラスのインスタンス型の値も文字列に変換する変換する関数。
-- LispValも同じように扱いたいので、`show`メソッドとしてshowValを用いる
-- Showクラスのメンバーにする
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom   name)          = name
showVal (Number contents)      = show contents
showVal (Bool   True)          = "#t"
showVal (Bool   False)         = "#f"
showVal (List   contents)      = "(" ++ unwordsList contents ++")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

{-
## 評価器作成の手始め
評価機の目標は、コードの型を、評価結果であるデータの型に写すことだ
Lispでは、コードとデータ両方の型が同じなので、評価機はLispValを返す.
-}
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _)   = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args
{-
この記法val@(String _)は文字列であるLispVal全てにマッチし、ValにそのLispVal全体
(Stringコンストラクタの中身だけでなく)を束縛する.
戻り値hString型ではなくLispVal型を持つ。
アンダースコアは「どうでもいい」変数で，どんな値にもマッチするが，どんな値にも束縛されない．

リテラルのリストではなくコンス演算子 : に対してマッチします
Haskellにおいてリストは実際のところ数珠繋がりになったコンス適用とからリストの構文糖衣。
`[1,2,3,4] = 1:(2:(3:(4: []))).`

リテラルのリストではなくコンス自体にパターンマッチさせることで、私たちは「リストの二番目の要素」
要素の代わりに「リストの残り」を指定していることになる
たとえば、(+ 2 2)をevalに渡したとすると、funcは+に束縛され、
argsは[number 2, number 2]に束縛される
-}

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives
{-
maybe              :: b -> (a -> b) -> Maybe a -> b
maybe 1: b:Nothingの時の型
maybe 2: Nothingじゃなかった時のbind
戻り値: Maybe型の値aをとり、それを適用orNothingの時の値を返すアレ

maybe n f Nothing  =  n
maybe n f (Just x) =  f x
-}
{-
組み込み関数lookupはペアのリストからキーを探す。
しかし、リスト中のどのペアもキーにマッチしない場合、lookupは失敗する。
これを実現するため、lookupは組み込みのmaybeインスタンスを返す。
maybe関数を使って成功・中黒失敗のときにそれぞれどうするかをしていする
もし関数が見つからなければ、#fに相当するBool false型を返す。
もしも疲れば、 $ argsを使ってそれを引数に適用する
-}

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol)]
{-
primitivesはlookupに期待されるようにペアのリストだが、
ペアの値は[LispVal]からLispValへの関数


numericBinopは、
Haskellのプリミティブ関数を取り、
引数のリストを解すコードでラップし、
プリミティブ関数を適用し、
結果をNumberコンストラクタでラップして返す.

-}
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""
