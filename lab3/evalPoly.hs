evalPoly :: Int -> [Int] -> Int

evalPoly x ns
   | length ns == 0 = 0
   | otherwise = n + x * evalPoly x s
   where (n:s) = ns
