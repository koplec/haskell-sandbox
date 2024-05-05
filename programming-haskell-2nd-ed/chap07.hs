import Data.Char
import Data.List --7.7でsortを使うため

-- 7章のfoldr, foldlの説明は秀逸
-- foldrは右結合
-- (1:(2:(3:[])))を 1+(2+(3+0))と書き換えることと同義
-- foldlは左結合
-- ((0+1)+2)+3)と左側結合が強くなる
-- 積算に使う感じになりそう
-- 結合順位が結果に影響を及ぼさない話、結合則、モノイドの話

reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x:xs) []

sumsqreven ns = sum (map (^2) (filter even ns))
sumsqreven2 = sum . map (^2) . filter even

-- 関数合成には、単位元がある。
-- 関数合成はモノイドかも
-- composeは右から適用させる、foldrがよい


type Bit = Int 
--tsならBit = 0 | 1とか書ける。書きたい
bin2int :: [Bit] -> Int 
bin2int bits = sum [w*b | (w, b) <- zip weights bits]
               where weights = iterate (*2) 1

bin2int2 :: [Bit] -> Int 
bin2int2 = foldr (\x y -> x + 2*y) 0 

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String 
decode = map (chr . bin2int) . chop8

transmit :: String -> String 
transmit = decode . channel . encode 
channel :: [Bit] -> [Bit]
channel = id

-- 7.7 投票アルゴリズム
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

result :: (Ord a) => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots = [["Red", "Green"]
          ,["Blue"]
          ,["Green", "Red", "Blue"]
          ,["Blue", "Green", "Red"]
          ,["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/=[])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head 

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
            [c] -> c
            (c:cs) -> winner' $ elim c bs

-- ex01
-- map f . filter p

-- ex04
dec2int :: [Int] -> Int 
dec2int = foldl (\b a -> 10*b + a) 0