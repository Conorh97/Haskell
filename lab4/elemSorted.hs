elemSorted :: Ord a => a -> [a] -> Bool

elemSorted n as
   | as == [] = False
   | head as > n = False 
   | head as == n = True
   | otherwise = elemSorted n (tail as) 
