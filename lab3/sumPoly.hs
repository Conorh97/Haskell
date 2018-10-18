sumPoly :: [Int] -> [Int] -> [Int] 

sumPoly as bs
   | (length as) == (length bs) = zipWith (+) as bs
   | (length as) > (length bs) = sumPoly as (bs ++ [0])
   | otherwise = sumPoly (as ++ [0]) bs
