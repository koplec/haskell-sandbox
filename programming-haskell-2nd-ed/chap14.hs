import Data.Monoid

-- 14.2 Foldable 
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show 

-- Monoidを使うと、foldableな関数を定義できる
fold :: Monoid a => Tree a -> a 
fold (Leaf x) = x 
fold (Node l r) = fold l `mappend` fold r

-- 14.2.1 例
-- TreeはMonoid a をもつTree a だから下記のようにFoldable Treeを作れる
instance Foldable Tree where 
    -- foldはFoldableにはないので、上で定義
    -- fold :: Monoid a => Tree a -> a
    -- fold (Leaf x) = x 
    -- fold (Node l r) = fold l `mappend` fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap f (Leaf x) = f x 
    foldMap f (Node l r) = foldMap f l `mappend` foldMap f r 

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b 
    foldr f v (Leaf x) = f x v 
    foldr f v (Node l r) = foldr f (foldr f v r ) l --右から計算

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl f v (Leaf x) = f v x
    foldl f v (Node l r) = foldl f (foldl f v l) r --左から計算

tree :: Tree Int 
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

-- 14.2.3 汎用的な関数
average :: Foldable t => t Int -> Int 
average ns = sum ns `div` length ns

-- 14.3 Traversable 
dec :: Int -> Maybe Int 
dec n = if n > 0 then Just (n - 1) else Nothing

-- traverse dec [1,2,0]で一つNothingになったらなんでぜんぶNothingになるのか
-- それは、decがApplicativeを持っているから
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- traverseは、Applicativeを返すから、一つでもNothingがあると、それ以降の計算は行われない




-- 14.3.1 例
-- TreeをTraversableにするため、Functorを定義する
instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b 
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Traversable Tree where 
    -- traverse :: Applicative f => ( a -> f b ) -> Tree a -> f (Tree b)
    traverse g (Leaf x) = pure Leaf <*> g x -- Leaf <$> g x でもいい
    traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r
