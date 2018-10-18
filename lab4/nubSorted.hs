nubSorted :: Ord a => [a] -> [a]

nubSorted as
   | as == [] = []
   | length as == 1 = as
   | head as == head (tail as) = nubSorted ([head as] ++ tail (tail as))
   | otherwise = [head as] ++ nubSorted (tail as)
