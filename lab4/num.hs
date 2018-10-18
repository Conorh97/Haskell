num :: Eq a => a -> [a] -> Int

num n as = sum [1 | x <- as, x == n]
