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