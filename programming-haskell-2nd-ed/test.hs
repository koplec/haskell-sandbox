factorial n = product [1..n]
average ns = sum ns `div` length ns

-- 練習問題3 
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- 練習問題4
last' xs = head (reverse xs)
last'' xs = xs !! (length xs - 1)


-- 練習問題5 
init' xs = take (length xs - 1) xs
init'' xs = reverse (tail (reverse xs))