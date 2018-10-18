insert :: Int -> [Int] -> [Int]

insert n [] = [n]
insert n ns
   | n <= head ns = [n] ++ ns
   | otherwise = [head ns] ++ insert n (tail ns) 
