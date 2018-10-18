pairswap :: [a] -> [a]

pairswap [] = []
pairswap xs = reverse (take 2 xs) ++ pairswap (drop 2 xs) 
