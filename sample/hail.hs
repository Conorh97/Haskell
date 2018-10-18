hail :: Int -> [Int]

hail 0 = []
hail 1 = [1]
hail n 
   | even n = [n] ++ hail (div n 2)
   | otherwise = [n] ++ hail ((n * 3) + 1)
