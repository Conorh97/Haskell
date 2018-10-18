wv :: String -> String
wv x = filter (not . (`elem` "aeiouAEIOU")) x
