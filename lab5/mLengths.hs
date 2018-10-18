leap :: Int -> Bool

leap n 
   | mod n 400 == 0 = True
   | mod n 100 == 0 = False
   | mod n 4 == 0 = True
   | otherwise = False

mLengths :: Int -> [Int]

mLengths n 
   | leap n == True = [31,29,31,30,31,30,31,31,30,31,30,31]
   | otherwise = [31,28,31,30,31,30,31,31,30,31,30,31]
