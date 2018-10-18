myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myzipWith f x [] = []
myzipWith f [] y = []
myzipWith f (x:xs) (y:ys) = [(f x y)] ++ (myzipWith f xs ys)
