iter :: Int -> (a -> a) -> a -> a 

iter 0 f x = x
iter n f x = f (iter (n - 1) f x)

pow :: Int -> Int -> Int

pow x 0 = 1
pow x n = iter (n - 1) (* x) x 
