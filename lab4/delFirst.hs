delFirst :: Eq a => a -> [a] -> [a]

delFirst n as 
   | as == [] = []
   | head as == n = tail as
   | otherwise = [head as] ++ delFirst n (tail as)
