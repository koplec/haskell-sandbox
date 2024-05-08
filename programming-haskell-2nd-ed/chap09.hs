data Op = Add | Sub | Mul | Div 
instance Show Op where 
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- 2つの正の整数に演算子を適用したときに正の整数が生成されるか調べる
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True 
valid Sub x y = x > y
valid Mul _ _ = True 
valid Div x y = x `mod` y == 0 --分数はダメ

-- 演算子を適用
apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


data Expr = Val Int | App Op Expr Expr 
instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                         brak (Val n) = show n
                         brak e = "(" ++ show e ++ ")"

-- 式の中の数値をリストとして返す
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r 

-- 式全体の値を返す
eval :: Expr -> [Int] 
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- 9-4 組み合わせ関数

-- リストの部分リストをすべて返す
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

-- 新しい要素をリストに挿入
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- 順列
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))


-- リストから選択肢を返す
-- すべての部分列の順列を計算して、それらを連結
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- 9-5
-- 与えられた式は、与えられた数のリストと目標の数に対して解を見つける
solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n = 
    elem (values e) (choices ns) && eval e == [n]
    -- valuesで数値のリストを返す
    -- choicesの中にそのリストがあるか（順列も含めていることがポイント？）
    -- evalした値が目標の数値と同じであること

-- (1+50)*(25-10)
e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))