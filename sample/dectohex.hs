dectohex :: Int -> String

getHex :: Int -> String 

getHex 10 = "A"
getHex 11 = "B"
getHex 12 = "C"
getHex 13 = "D"
getHex 14 = "E"
getHex 15 = "F"
getHex n = show n

dectohex n 
   | n < 16 = getHex n
   | otherwise = dectohex (quot n 16) ++ getHex (mod n 16)
