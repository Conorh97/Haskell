import Data.Char(digitToInt)
bintodec :: String -> Int

bintodec "" = 0
bintodec s = ((digitToInt (head s)) * (2 ^ ((length s) - 1))) + bintodec (tail s)

