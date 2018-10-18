getS :: Float -> Float -> Float -> Float
getS a b c = (a + b + c) / 2

isGreaterEqual :: Float -> Float -> Float -> Bool
isGreaterEqual a b c = (a + b) >= c

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c 
    | ((not (isGreaterEqual a b c)) 
      || (not (isGreaterEqual b c a)) 
      || (not (isGreaterEqual c a b))) = error "Not a triangle!"
    | otherwise                        = sqrt ((getS a b c) * ((getS a b c) - a) * ((getS a b c) - b) * ((getS a b c) - c))
