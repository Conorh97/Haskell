isSum :: Float -> Float -> Float -> Bool
isSum a b c = ((a + b) == c) || ((b + c) == a) || ((c + a) == b)
