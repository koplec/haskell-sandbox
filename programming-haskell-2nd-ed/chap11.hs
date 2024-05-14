import Data.Char 
import Data.List  
import System.IO 

size :: Int
size = 3

-- 格子はプレイヤーの値のリスト
type Grid = [[Player]]
data Player = O | B | X deriving (Eq, Ord, Show)
-- Bは空白 Blank

next :: Player -> Player 
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat --concatでGridを一次元のlistに変換している

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
            os = length (filter (== O) ps)
            xs = length (filter (== X) ps)
            ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where 
             line = all (== p)
             rows = g --横に並んでいるもの
             cols = transpose g --縦に並んでいるもの
             dias = [diag g, diag (map reverse g)] --斜め
-- transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]

-- 左上と右下を結ぶ対角線上にあるプレイヤーの値を返す
diag :: Grid -> [Player] 
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool 
won g = wins O g  || wins X g 


-- 11.4 格子を表示する
putGrid :: Grid -> IO()
putGrid = 
    putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-']
    --unlinesはリストの間に改行を挟んで１つの文字列にする
    -- map showRowでGridの中の１つずつのRowを

-- ３行で3目ならべの1行を表す
showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|" -- ["|", "|", "|"]という[String]

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

-- リストの要素の間に値を差し込む
interleave :: a -> [a] -> [a]
interleave x [] = [x]
interleave x [y] = [y]
interleave x (y:ys) = y: x : interleave x ys

-- 11-5 手を決める

-- 指し手は、適切な範囲の番号で、かつ、その番号の位置が空白の時に有効
valid :: Grid -> Int -> Bool 
valid g i = 0 <= i && i < size^2 && concat g !! i == B -- concat使うのがまだ頭にすっと入らない

-- 指し手を格子に適用して、結果を格子のリストで返す
move :: Grid -> Int -> Player -> [Grid]
move g i p =
    if valid g i then [chop size (xs ++ [p] ++ ys)] else [] -- chop size ...で、3つずつのリストのリストにする
    where (xs,B:ys) = splitAt i (concat g)
-- splitAtは与えられたところで要素を2分割する

-- 指定した長さでリストを断片化する
chop :: Int -> [a] -> [[a]]
chop n [] = [] 
chop n xs = take n xs : chop n (drop n xs)

-- 11.6 番号を読み込む
-- プレイヤーの指し手を読み込む

getNat :: String -> IO Int 
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt


-- 11-7 人間vs人間

type Pos = (Int, Int)

-- 画面クリア
cls :: IO ()
cls = putStr "\ESC[2J"


-- 画面移動
goto :: Pos -> IO()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++"H")


tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

-- 
run' :: Grid -> Player -> IO()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise =
             do i <- getNat (prompt p)
                case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             run' g p
                    [g'] -> run g' (next p)

prompt :: Player -> String 
prompt p = "Player " ++ show p ++ ", enter your move:"

-- 11.8 ゲームの木
data Tree a = Node a [Tree a] deriving Show

-- 与えられた局面を木の根として、現在のプレイヤーによる有効な指し手によって得られる格子を再帰的に生成
gametree :: Grid -> Player -> Tree Grid 
gametree g p = Node g [gametree g' (next p)| g' <- moves g p] --movesが空だったら生成されなくなる　Node a []になる

-- 有効な手をリストとしてかえす
moves :: Grid -> Player -> [Grid]
moves g p
  | won g = [] --すでに勝負がついている
  | full g = [] 
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]


-- 11.9 枝を刈る
-- 深くなりすぎないように
prune :: Int -> Tree a -> Tree a 
prune 0 (Node x _) = Node x [] 
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int 
depth = 9

-- 11.10 mini-max法

---- ゲームの木のラベル付け
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g []) --grid自体は下記変わらない Girdが(Grid, Player)に変わっていることに注意
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts) -- Treeの形は　Node g [Tree g]であることに注意 
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
                  where
                    ts' = map minimax ts
                    ps = [p | Node (_, p) _ <- ts'] --ts'はすでにラベル付けがされているから、そこからplayerを抜き出す


-- moveが次の盤面を生成する関数であったことを考える
-- 次の手ではなく、次の手で生成されるボードの生成
bestmove :: Grid -> Player -> Grid 
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best] --根と同じラベルを持つ格子の先頭をとる 根と同じだから最善手？
               where
                 tree = prune depth (gametree g p) --指定した範囲でゲーム木を生成
                 Node (_, best) ts = minimax tree  --minimax法でラベル付け 


-- 11-11 人間vsコンピュータ

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO()
play g p = do cls
              goto (1, 1)
              putGrid g
              play' g p 

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do i <- getNat (prompt p)
                case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             play' g p 
                    [g'] -> play g' (next p)
  | p == X = do putStr "Player X is thinking..."
                (play $! (bestmove g p)) (next p)