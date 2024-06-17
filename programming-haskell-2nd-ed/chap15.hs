-- 15.1 導入
inc :: Int -> Int 
inc n = n + 1

-- 命令型言語では評価の順番によって結果が変わることがある　代入のタイミング

-- 15.2 評価戦略
-- mult :: (Int, Int ) -> Int 
-- mult (x, y) = x * y

-- 最内簡約　値渡し
-- 最外簡約　名前渡し
-- 正格

-- 15.2.1 ラムダ式
-- 関数に対して許される唯一の操作は、関数を引数に適用すること
-- 関数の本体における、簡約は、引数の適用後
mult :: Int -> Int -> Int 
mult x = \y -> x * y

-- 15.3 停止性


-- 15.4 簡約の回数
-- Haskellの遅延評価　名前渡し　ポインタ渡し

-- 15.5 無限のデータ構造
ones :: [Int]
ones = 1 : ones


-- 15.6 部品プログラム
-- 遅延評価は、計算の際にデータから制御を切り離す
-- replicateの例が、「制御とデータを単一の関数として組み合わせて」と説明しているところ

-- エラトステネスの篩
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- 15.7 正格適用
sumwith :: Int -> [Int] -> Int 
sumwith v [] = v 
sumwith v (x:xs) = sumwith (v+x) xs --vは蓄積変数、accumulator 遅延評価だったら、スタックに積まれそう

sumwith2 :: Int -> [Int] -> Int 
sumwith2 v [] = v
sumwith2 v (x:xs) = (sumwith2 $! (v+x)) xs --正格評価