qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a<= x]
        larger = [b | b <- xs, b > x]

-- seqn :: [IO a] -> IO [a]
seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do 
    x <- act
    xs <- seqn acts
    return (x:xs)
--  seqn ["123", "abc"]
-- ["1a","1b","1c","2a","2b","2c","3a","3b","3c"]

-- 練習問題 5
-- <=がないと、同じ値があった時、smallerに値が含まれなくなり、srotされなくなる