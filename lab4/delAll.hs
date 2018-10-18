delAll :: Eq a => a -> [a] -> [a]

delAll n as = [x | x <- as, x /= n]
