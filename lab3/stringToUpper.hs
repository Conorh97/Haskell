import Data.Char(toUpper)

stringToUpper :: String -> String

stringToUpper [] = ""
stringToUpper cs = [toUpper (head cs)] ++ stringToUpper (tail cs)
