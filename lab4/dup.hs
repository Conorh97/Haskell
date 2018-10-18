dup :: Eq a => [a] -> Bool

count :: Eq a => a -> [a] -> Int

count n as
   | as == [] = 0
   | head as == n = 1 + count n (tail as)
   | otherwise = count n (tail as)

dup as = sum [count x as | x <- as] /= length as
