import Data.Char 

lowers :: String -> Int 
lowers str = length [ch | ch <- str , ch >= 'a' && ch <= 'z']

count :: Char -> String -> Int 
count ch str = length [c | c <- str , c == ch]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

let2int :: Char -> Int 
let2int c = ord c - ord 'a'

int2let :: Int -> Char 
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char 
shift n c | isLower c  = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n str = [ shift n ch | ch <- str ]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float 
percent n m = (fromIntegral n / fromIntegral m) * 100.0 

freqs :: String -> [Float ]

freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

-- χ二乗検定
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e| (o,e) <- zip os es ] --zipでまとめると1:1の計算対応ができる

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String 
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs 