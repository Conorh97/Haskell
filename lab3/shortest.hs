shortest :: [[a]] -> [a]

shortest [] = []

shortest as
   | (length as) == 1 = (head as)
   | (length b) > (length c) = shortest (c:s)
   | otherwise = shortest (b:s)
   where (b:c:s) = as
