type PDA = (Int,[Int],[Transition])

type Transition = ((Int,String,String),(Int,String))

type Configuration = (Int,String,String)

data Result = Accept | Reject deriving (Eq, Show)

run :: PDA -> String -> Result

recRun :: PDA -> Configuration -> Result

match :: Int -> String -> ((Int, String, String), (Int, String)) -> (String, Int, String)

match a b ((v, w, x), (y, z))
   | a == v && b == w = (x, y, z)
   | otherwise = ("", 0, "")

findMatch a b xs
   | [match a b x | x <- xs, (match a b x) /= ("", 0, "")] == [] = []
   | otherwise = [match a b x | x <- xs, (match a b x) /= ("", 0, "")]

run x "" = Accept
run (n, ns, ts) s = recRun (n, ns, ts) (n, s, "")

recRun (n, ns, ts) (na, s, st)
   | s == "" && st == "" = Accept
   | elem Accept [recRun (n, ns, ts) (b, (drop 1 s), (c ++ st)) | (a, b, c) <- ms, c /= ""] == True
      || elem Accept [recRun (n, ns, ts) (b, (drop 1 s), (drop 1 st)) | (a, b, c) <- ms, a /= "" && (take 1 st) == a] == True
      || elem Accept [recRun (n, ns, ts) (b, s, st) | (a, b, c) <- ms, a == "" && c == ""] == True
      || elem Accept [recRun (n, ns, ts) (b, (drop 1 s), st) | (a, b, c) <- ms, a == "" && c == ""] == True = Accept
   | otherwise = Reject
   where ms = findMatch na (take 1 s) ts

ijk = (1, [2,3], [((1, "a", ""), (1, "a")), ((1, "", ""), (2, "")), ((1, "", ""), (3, "")), ((2, "b", "a"), (2, "")), ((2, "c", ""), (2, "")), ((3, "b", ""), (3, "")), ((3, "c", "a"), (3, ""))])

abc = (1, [2], [((1, "a", ""), (1, "x")), ((1, "a", "c"), (1, "")), ((1, "b", ""), (1, "x")), ((1, "b", "c"), (1, "")), ((1, "c", "x"), (1, "")), ((1, "c", ""), (1, "c")), ((1, "", ""), (2, ""))])
