data Tree a =  Null | Node a (Tree a) (Tree a)
               deriving(Read, Show)

addNode :: Ord a => a -> Tree a -> Tree a

addNode n Null = (Node n Null Null)
addNode n (Node v l r) 
   | n < v = Node v (addNode n l) r
   | otherwise = Node v l (addNode n r)

makeTree :: Ord a => [a] -> Tree a

makeTree a
   | a == [] = Null 
   | otherwise = addNode (head a) (makeTree (tail a))
