
-- 8-1 
type Assoc k v = [(k, v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k]

assocs :: Assoc Int String
assocs = [(1, "taro"), (2, "hanako"), (3, "suzuki")]

-- 8-2 
type Pos = (Int, Int)
data Move = North | South | East | West deriving Show

move :: Move -> Pos -> Pos 
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos 
moves [] p = p 
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move 
rev North = South 
rev South = North
rev East = East
rev West = West 


data Shape = Circle Float | Rect Float Float
square :: Float -> Shape
square n = Rect n n 

area :: Shape -> Float 
area (Circle r) = pi * r ^ 2
arae (Rect a b) = a * b

-- 8-3
-- newtype Nat = N Int

-- 8-4 
data Nat = Zero | Succ Nat deriving (Show)
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n 

int2nat :: Int -> Nat 
int2nat 0 = Zero 
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat 
add Zero n = n
add (Succ m) n = add m (Succ n)


data List a = Nil | Cons a (List a)
len :: List a -> Int 
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
t :: Tree Int 
t = Node (Node (Leaf 1) 3 (Leaf 4))
         5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool 
occurs x (Leaf y) = x == y 
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Ord a => Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- 8-5 

-- 8-6 恒震式検査器
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop deriving (Show)
p1 :: Prop 
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop 
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop 
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop 
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

-- 置換表を書き下しただけ
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q 
eval s (Imply p q) = eval s p <= eval s q -- 論理包含

-- 命題に含まれるすべての変数をリストとして返す
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p 
vars (And p q) = vars p ++ vars q 
vars (Imply p q) = vars p ++ vars q 

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bbs ++ map (True:) bbs
          where bbs = bools (n-1)

-- 値に対する真理値のすべてのリストの生成
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p) --vsはすべての変数をuniqueにもつリスト

-- 7章より
---- unique
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)


isTaut :: Prop -> Bool 
isTaut p = and [eval s p | s <- substs p]

-- ex01
mult :: Nat -> Nat -> Nat 
mult Zero _ = Zero 
mult _ Zero = Zero
mult (Succ n) m = add m (mult n m)

one = Succ Zero 
two = add one one 
three = add two one
