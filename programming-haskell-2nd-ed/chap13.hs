import Control.Applicative
import Data.Char 

newtype Parser a = P (String -> [(a, String)])

-- 型を区別するための構成子Pを単純に取り除く関数を定義
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp 

item :: Parser Char 
item = P (\inp -> case inp of 
                    [] -> []
                    (x:xs) -> [(x, xs)])

-- 13.4 パーサーの連結
-- パーサーを、functor, applicative, monadにする
instance Functor Parser where 
    -- fmap :: (a -> b) -> Parser a -> Parser b 
    fmap g p = P (\inp -> case parse p inp of 
                            [] -> []
                            [(v, out)] -> [(g v, out)])

instance Applicative Parser where 
    -- pure :: a -> Parser a 
    -- どんな値が来ても、与えられた引数を返すparserを生成
    pure v = P (\inp -> [(v, inp)])

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
    -- 関数を返すパーサーを、引数を返すパーサーに適用し、その関数をその引数に適用した結果を返すパーサーを生成
    pg <*> px = P (\inp -> case parse pg inp of
                            [] -> []
                            [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- returnはpureと同じ
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b 
    p >>= f = P (\inp -> case parse p inp of 
                            [] -> []
                            [(v, out)] -> parse (f v) out)
-- ３文字を消費し、２つ目の文字を捨てて、１つ目と２つ目を組にして返すパーサー
three :: Parser (Char, Char)
-- three = pure g <*> item <*> item <*> item
--     where g x y z = (x , z)

three = do x <- item 
           item 
           z <- item
           return (x, z)

-- 13.5 選択
-- Alternative 
instance Alternative Parser where 
    -- empty :: Parser a 
    empty = P (\inp -> []) -- 常に失敗するパーサー
    -- (<|>) :: Parser a -> Parser a -> Parser a 
    -- パーサーの選択、最初のパーサーが失敗なら、次へ次へ
    p <|> q = P (\inp -> case parse p inp of 
                            [] -> parse q inp 
                            [(v, out)] -> [(v, out)]) --あれ、パースの結果っていつの間にか1つになってたっけ？

-- 13.6 派生関数
-- 述語pを満たす1文字用のパーサーsat 
sat :: (Char -> Bool) -> Parser Char 
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char 
digit = sat isDigit 

lower :: Parser Char 
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char 
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs --文字列全体が利用された場合にのみ成功する
                   return (x:xs)

ident :: Parser String 
ident = do x <- lower 
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser () --あとでつかうspaceを考えてmanyを使うことで、1つ空白でも２つ空白でも０個空白でも通るようになっている
space = do many (sat isSpace)
           return ()
            
-- 整数のパーサー
int :: Parser Int
int  = do char '-'
          n <- nat
          return (-n)
        <|> nat

-- 13.7 空白の扱い
-- 前後の空白を無視する
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- 前後の空白を無視する識別子
identifier :: Parser String
identifier = token ident

-- 前後の空白を無視する自然数
natural :: Parser Int
natural = token nat

-- 前後の空白を無視する整数
integer :: Parser Int
integer = token int

-- 特定の文字列のパーサー
symbol :: String -> Parser String 
symbol xs = token (string xs)

-- ここまでくるとかっこいいね
nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

-- 13.8 数式
expr :: Parser Int 
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             <|> return t


term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
             <|> return f 

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> natural 

eval :: String -> Int
eval xs = case (parse expr xs) of 
            [(n, [])] -> n
            [(_, out)] -> error ("Unused input" ++ out)
            [] -> error "Invalid input"

