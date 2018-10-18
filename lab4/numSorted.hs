numSorted :: Ord a => a -> [a] -> Int

numSorted n as
   | as == [] = 0
   | head as > n = 0
   | head as == n = 1 + (numSorted n (tail as))
   | otherwise = (numSorted n (tail as)) 
