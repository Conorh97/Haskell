factors :: Int -> [Int]

factors n = [x | x <- [1..n], mod n x == 0]
