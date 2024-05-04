import Data.Char

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