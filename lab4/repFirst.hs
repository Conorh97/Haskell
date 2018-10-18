repFirst :: Eq a => a -> a -> [a] -> [a]

repFirst n x as
   | as == [] = []
   | head as == n = [x] ++ (tail as)
   | otherwise = [head as] ++ (repFirst n x (tail as))
