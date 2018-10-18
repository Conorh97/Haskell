prepend :: [a] -> [[a]] -> [[a]]

prepend x ys = map (x ++) ys
