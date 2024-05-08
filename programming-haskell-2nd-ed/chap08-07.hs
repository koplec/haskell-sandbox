-- 8-7 抽象機械
data Expr = Val Int | Add Expr Expr 

-- value :: Expr -> Int 
-- value (Val n) = n 
-- value (Add x y) = value x + value y

type Cont = [Op]
data Op = EVAL Expr | ADD Int 

-- 制御スタックにある命令に従って式を評価する
eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n --式が整数の時は、評価済みなので、制御スタックの命令を実行
eval (Add x y) c = eval x (EVAL y : c) --式が加算の時はxを評価して、EVAL yを制御スタックに載せる

-- 命令を実行していく
exec :: Cont -> Int -> Int 
exec [] n = n --制御スタックが空の時は、結果を返す
exec (EVAL y : c) n = eval y (ADD n : c) --制御スタックの一番上が、命令EVALの時は、式yを評価して、現在のnをADDする制御を載せる
exec (ADD n : c) m = exec c (n+m) --ADD n のときは加算する2つの整数は評価済みなので、両者を足し合わせてexecを続ける

value :: Expr -> Int 
value e = eval e []