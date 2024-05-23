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