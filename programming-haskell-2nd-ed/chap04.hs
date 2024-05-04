even :: Integral a => a -> Bool 
even n = n `mod` 2 == 0

-- splitAt :: Int -> [a] -> ([a], [a])
-- splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a 
recip n = 1 / n

abs' n | n >= 0 = n
       | otherwise = -n

test :: [Char] -> Bool 
test ('a':_) = True 
test _ = False

odds :: Int -> [Int]
odds n = map (\n -> n * 2 + 1) [0..n-1]

-- ex 1
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = splitAt (length xs `div` 2) xs

myHalve :: [a] -> ([a], [a])
myHalve xs = (take n xs, drop n xs)
    where n = length xs `div` 2


-- ex 2 
third1 :: [a] -> a 
third1 xs = head (tail (tail xs))

third2 :: [a] -> a 
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x

-- ex 3 
safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

safetail2 :: [a] -> [a] 
safetail2 xs = if null xs then [] else tail xs

safetail3 xs = case xs of
    [] -> []
    (_:xs) -> xs 

safetail4 xs 
    | null xs = []
    | otherwise = tail xs 

-- ex 4 
(|||) :: Bool -> Bool -> Bool 
-- True ||| True = True 
-- True ||| False = True
-- False ||| False = False
-- False ||| True = True

-- True ||| _ = True 
-- False ||| b = b

b ||| c
    | b == c = b
    | otherwise = True

-- ex 5 
(&&&) :: Bool -> Bool -> Bool
b &&& c = if b == c then c else False


-- ex 7 
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-- ex 8 
luhnDouble :: Int -> Int 
luhnDouble n = let nn = n * 2 
                in if nn > 9 then nn - 9 else nn

luhn :: Int -> Int -> Int -> Int -> Bool 
luhn a b c d = 
    let total = sum [luhnDouble a, b , luhnDouble c , d]
    in total `mod` 10 == 0
