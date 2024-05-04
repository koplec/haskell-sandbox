reverse2 :: [a] -> [a]
reverse2 xs = snd (iterReverse (xs, []))


iterReverse :: ([a], [a]) -> ([a], [a])
iterReverse ([], ys) = ([], ys)
iterReverse (x:xs, ys) = iterReverse (xs, x:ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

--6.5 
evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs 

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

--ex07 
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys 
merge xs [] = xs 
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- ex08 
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort s = let (xs, ys) = halve s in merge (msort xs) (msort ys)