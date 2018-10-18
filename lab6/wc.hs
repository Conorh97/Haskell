import Data.Char(isUpper)

wc :: String -> String

wc x = filter (not . isUpper) x

