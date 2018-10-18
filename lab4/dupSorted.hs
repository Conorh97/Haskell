dupSorted :: Eq a => [a] -> Bool

dupSorted as
   | as == [] = False
   | length as == 1 = False
   | head as == head (tail as) = True
   | otherwise = dupSorted (tail as)

