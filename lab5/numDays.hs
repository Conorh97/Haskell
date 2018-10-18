data Day =  Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday
            deriving (Enum, Show)

data Month = Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec
             deriving (Enum, Read)

type Date = (Int,Month,Int)

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

getLeaps :: [Int] -> Int

getLeaps as = sum [1 | x <- as, leap x] 

getNonLeap n a = sum [1 | x <- [n..a]] - getLeaps [n..a]

numDays :: Date -> Int

numDays (d, m, y) = (getLeaps [1753..y-1]) * 366 + (getNonLeap 1753 (y-1)) * 365 + d + sum (take (fromEnum m) (mLengths y))
