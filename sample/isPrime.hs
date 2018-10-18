isPrime :: Int -> Bool

isPrime n
   | n < 2 = False
   | otherwise = [x | x <- [2..n], (mod n x) == 0] == [n]
