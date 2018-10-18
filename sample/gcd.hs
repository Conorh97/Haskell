myGCD :: Int -> Int -> Int

myGCD x 0 = x
myGCD x y = gcd y (mod y x)
