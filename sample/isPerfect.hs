isPerfect :: Int -> Bool

isPerfect n = (sum [x | x <- [1..(n - 1)], (mod n x) == 0]) == n
