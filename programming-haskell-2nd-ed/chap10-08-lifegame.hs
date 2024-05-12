-- 10.8 lifegame 

-- 画面クリア
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs 

-- 画面移動
goto :: Pos -> IO()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++"H")

width :: Int 
width = 10

height :: Int 
height = 10

-- 生きているセルの位置リスト
type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO()
showcells b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board -> Pos -> Bool 
isAlive b p = elem p b 

isEmpty :: Board -> Pos -> Bool 
isEmpty b p = not (isAlive b p)

-- 周囲のセルの位置を返す関数を定義
neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x-1, y-1), (x, y-1),
                           (x+1, y-1), (x-1, y),
                           (x+1, y), (x-1, y+1),
                           (x, y+1), (x+1, y+1)]

wrap :: Pos -> Pos 
wrap (x, y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

-- 与えたPosの周辺の生きたセルの数をカウント
liveneighbs :: Board -> Pos -> Int 
liveneighbs b = length . filter (isAlive b) . neighbs 

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]] --周囲の生きたセルが2,3の時のみ生き残るposを絞り込む

-- 新しく誕生するセルのリスト
births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)), --現在の生きたセルの周辺のセルが候補
                     isEmpty b p, -- 元々空
                     liveneighbs b p == 3] --周囲に生きたセルが3つある必要がある


-- duplicateをremove 
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

-- 次の世代のボード生成
nextgen :: Board -> Board 
nextgen b = survivors b ++ births b 

-- ライフゲーム本体
life :: Board -> IO ()
life b = do cls
            showcells b
            wait 50000
            life (nextgen b)

wait :: Int -> IO()
wait n = sequence_ [return () | _ <- [1..n]]

main :: IO()
main = life glider