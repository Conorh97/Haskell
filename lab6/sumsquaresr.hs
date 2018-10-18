sumsquaresr :: Int -> Int

getSquares [] = []
getSquares (x:xs) = [x*x] ++ (getSquares xs) 

sumsquaresr n = foldr (+) 0 (getSquares [1..n])
