import Data.Char --isDigit, digitToIntのため

inc :: Functor f => f Int -> f Int 
inc = fmap (+1)

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show 

-- Functorを関手というの
instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b 
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node  (fmap g l) (fmap g r)

-- GHC.Baseにあり
-- instance Functor IO where 
--     -- fmap :: (a -> b) -> IO a -> IO b
--     fmap g mx = do {x <- mx; return (g x)}:

-- アプリカティブスタイル
-- 関数を複数の引数に適用する方法の一般
-- 逐次処理と繰り返し

-- モナドが分岐

prods :: [Int] -> [Int] -> [Int ]
-- prods xs ys = [x*y | x <- xs, y <- ys]
prods xs ys = pure (*) <*> xs <*> ys

getChars :: Int -> IO String 
-- getChars 0 = return []
-- getChars n = pure (:) <*> getChar <*> getChars (n-1)
getChars n = sequenceA (replicate n getChar)

-- 12.3 モナド
data Expr = Val Int | Div Expr Expr 

eval :: Expr -> Maybe Int
-- eval (Val a) = pure a

-- 冗長な定義
-- eval (Div x y) = case eval x of
--                     Nothing -> Nothing
--                     Just n -> case eval y of
--                                 Nothing -> Nothing
--                                 Just m -> safeDiv n m 

-- アプリカティブによる定義
-- safeDivの型は、 Int -> Int -> Maybe Int 
-- だけど、アプリカティブスタイルを使ったときは、Int -> Int -> Intが期待されている
-- アプリカティブスタイルはfmapの一般化であったことを注意
-- eval (Div x y) = pure safeDiv <*> eval x <*> eval y


-- monad での定義
eval (Val n) = Just n 
-- eval (Div x y) = eval x >>= \n -> --xを評価してnを取り出す 失敗していたらNothing
--                  eval y >>= \m -> --yを評価してmを取り出す 失敗していたらNothing
--                  safeDiv n m
-- do記法
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safeDiv n m

safeDiv :: Int -> Int -> Maybe Int 
safeDiv _ 0 = Nothing 
safeDiv n m = Just (n `div` m)

-- 要素のすべての組み合わせを返す関数
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)

-- ☆12.3.2 Stateモナド
type State = Int 

-- 状態変換器の型
newtype ST a = S (State -> (a, State))

-- 状態変換器の型STから型を区別する校正子Sをと除くもの
app :: ST a -> State -> (a, State)
-- app (S st) state = st state
app (S st) = st --こっちのほうがわかりやすくない？

-- STをFunctorのインスタンスにする
-- 変換器で得られた結果の値に関数を適用する
instance Functor ST where 
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x, s') = app st s --状態変化
                     in (g x, s')) --状態変化に対してgを作用


-- STをApplicativeにする
instance Applicative ST where 
    -- pure :: a -> ST a
    -- ある値から状態変化機をつくるけど、これは状態を変化させないもの　
    pure x = S (\state -> (x, state))
    
    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    -- 「関数を返す状態変換器」を、「値を返す状態変換器」に適用して、「関数を適用した結果を返す状態変換器」を作る
    stf <*> stx = S (\s ->
        let (f, s') = app stf s --functionを取り出して
            (x, s'') = app stx s' --stxをstateに作用させるのね
             in (f x, s'') --
        )

-- STをMonadにする
instance Monad ST where
    -- return :: a -> ST a
    -- return x = S (\s -> (x, s))
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    -- 状態変換器stを初期状態sに適用して、次に関数fを結果のxに提供する、新しい状態変換器を作る
    st >>= f = S (\s -> --ST bの\sを定義しているだけ　これが初期値に対応
                  let (x, s') = app st s --ST aからSを取り除き
                  in app (f x) s' -- a -> ST bを適用する
                 )

-- ☆12.3.3 木構造のラベル付け
-- data Tree a = Leaf a | Node (Tree a ) (Tree a ) deriving Show 

tree :: Tree Char 
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- 木の葉を未使用の一意な整数でラベル付け
-- 整数の状態を引き回す
-- 初歩的な再帰で表現
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r' , n'')
                      where
                        (l', n') = rlabel l n
                        (r', n'') = rlabel r n'


-- 現在の状態を返し、次の整数が次の状態となる状態変換器を定義
fresh :: ST Int --freshは状態変換器という値に過ぎない
fresh = S (\n -> (n, n+1))
-- 例：app fresh 0は(0, 1)になるし、app fresh 10は、(10, 11)になる

-- fresh STはアプリカティブ
-- アプリカティブはfmapの一般化
-- 今回の問題は木の葉から整数へのfmap 

-- g <$> x = fmap g xと定義されている
-- 一方、fmap g x = pure g <*> xはアプリカティブ則から満たす
-- よって、g <$> x = pure g <*> x

alabel :: Tree a -> ST (Tree Int) --Tree aをTree Intな、状態変換器に変換する
-- freshが、ST Intであることがわかれば、Leafを適用して、ST (Tree Int)に近づけられえる
alabel (Leaf _) = Leaf <$> fresh 
-- pure Leaf <*> fresh = fmap Leaf fresh = Leaf <$> fresh
-- :t Leaf は、 Leaf :: a -> Tree a である
-- fmap Leaf fresh = S (\n -> (Leaf n, n + 1))になる
-- それは、fmapを定義した、STがFunctorのインスタンスであることからわかる
-- 末尾のLeafをどういう風に表現したいかを考える
-- 与えた数字をそのままLeaf にするようにしたい
-- 状態変換器 ST (Tree Int)は数字を与えたらそのままの数字をLeafに持たせて返ってくる
-- 例えば、app (Leaf <$> fresh) 0は、(Leaf 0, 1)になる

alabel (Node l r) = Node <$> alabel l <*> alabel r 
-- 再帰部
-- すでに、alabel lとalabel rで、部分木は完成している。
-- これにNode構成子を適用したと考える
-- アプリカティブスタイルは関数適用と盲目的に考えると理解した気持ちになる

-- STがモナドであることも考えてみる
mlabel :: Tree a -> ST (Tree Int)
-- freshが、Monadであることがわかれば、do記法を使える
mlabel (Leaf _) = do n <- fresh --freshは、ST Intなので、nは、Intになる
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')


-- 12.3.4 汎用的な関数
conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing


filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p [] = return []
filterM p (x:xs) = do b <- p x --bは、True, Falseを取得する
                      ys <- filterM p xs
                      return (if b then x:ys else ys)
--  filterM (\x -> [True, False]) [1,2,3]
-- これからべき集合を作れるのは、Monadの強力さを感じる

-- 入れ子のmonadを平たんにする
-- flatternって名前つけたい
join :: Monad m => m (m a) -> m a 
-- do記法
-- join mmx = do mx <- mmx
--               x <- mx 
--               return x 

join mmx = mmx >>= \mx ->
           mx >>= \x ->
           return x

-- 12.3.5 モナド則
