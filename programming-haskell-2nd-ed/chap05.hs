concat2 :: [[a]] -> [a]
concat2 xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts ps = [ a | (a, _) <- ps]

length2 :: [a] -> Int 
length2 xs = sum [1| _ <- xs]

factors :: Int -> [Int]
factors n = [p | p <- [1..n], n `mod` p == 0]

prime :: Int -> Bool 
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [p | p <- [1..n], prime p]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- 5.3
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool 
sorted xs = and [x <= y| (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

-- 5.4
lowers :: String -> Int 
lowers str = length [ch | ch <- str , ch >= 'a' && ch <= 'z']

count :: Char -> String -> Int 
count ch str = length [c | c <- str , c == ch]

-- ex01 
a = sum [x^2 | x <- [1..100]]

-- ex02 
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

-- ex03 
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n , x /= y]

-- ex04 
replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

-- ex05 
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n] , a^2 + b^2 == c^2]

-- ex06 
perfects :: Int -> [Int]
perfects m = [x | x <- [1..m], sum (filter (/= x) (factors x)) == x  ] 

-- ex09 
scalarProduct :: [Int] -> [Int] -> Int 
scalarProduct xs ys = sum [x*y | (x, y) <- zip xs ys]