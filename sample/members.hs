data Intset = Empty | Singleton Int | Union Intset Intset | Intersection Intset Intset

members :: Intset -> [Int]

members Empty = []
members (Singleton n) = [n]
members (Union s1 s2) = [x | x <- (members s1), x `notElem` (members s2)] ++ (members s2)
members (Intersection s1 s2) = [x | x <- (members s1), x `elem` (members s2)]
