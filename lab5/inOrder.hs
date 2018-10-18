data Tree a =  Null | Node a (Tree a) (Tree a)
               deriving(Read, Show)

inOrder :: Tree a -> [a]

inOrder Null = []
inOrder (Node v l r) = (inOrder l) ++ [v] ++ (inOrder r)
