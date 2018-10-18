dectobin :: Int -> String


dectobin n 
   | n == 0 = "0"
   | n == 1 = "1"
   | mod n 2 == 1 = dectobin (quot n 2) ++ "1"
   | otherwise = dectobin (quot n 2) ++ "0"
