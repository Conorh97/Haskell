myProduct :: [Int] -> Int

myProduct [] = 1
myProduct [n] = n
myProduct ns = (head ns) * (myProduct (tail ns))
