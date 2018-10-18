isSquare :: Int -> Bool

isSquare 1 = True
isSquare n = n `elem` [x ^ 2 | x <- [1..(n - 1)]]
