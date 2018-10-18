repAll :: Eq a => a -> a -> [a] -> [a]

repAll n x as
   | as == [] = []
   | head as == n = [x] ++ (repAll n x (tail as))
   | otherwise = [head as] ++ (repAll n x (tail as))

