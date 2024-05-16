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