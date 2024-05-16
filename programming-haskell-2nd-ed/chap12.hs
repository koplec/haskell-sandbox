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

data Expr = Val Int | Div Expr Expr 

eval :: Expr -> Maybe Int
-- eval (Val a) = pure a
-- eval (Div x y) = case eval x of
--                     Nothing -> Nothing
--                     Just n -> case eval y of
--                                 Nothing -> Nothing
--                                 Just m -> safeDiv n m 
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