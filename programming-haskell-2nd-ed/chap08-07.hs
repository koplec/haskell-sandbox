-- 8-7 抽象機械
data Expr = Val Int | Add Expr Expr 

value :: Expr -> Int 
value (Val n) = n 
value (Add x y) = value x + value y

type Cont = [Op]
data Op = EVAL Expr | ADD Int 

eval :: Expr -> Cont -> Int
eval 