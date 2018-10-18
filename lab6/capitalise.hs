import Data.Char(toUpper)

capitalise :: String -> String

capitalise [] = ""
capitalise (c:cs) = [toUpper c] ++ capitalise cs
