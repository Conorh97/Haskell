foldr (*) 1 [1..5]

foldr (*) 1 [1..30]

foldl (+) 1 [1..100]

foldl (+) 1 [x | x <- [1..99], odd x]
