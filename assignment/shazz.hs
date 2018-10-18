type PDA = (Int,[Int],[Transition])

type Transition = ((Int,String,String),(Int,String))

type Configuration = (Int,String,String)

data Result = Accept | Reject deriving Show

get1st (x, _, _) = x
get2nd (_, y, _) = y
get3rd (_, _, z) = z

getTransitions :: Int -> String -> [Transition] -> [Transition]

getTransitions state next_input transitions
   | transitions == [] = []
   | state == get1st (fst (head transitions)) && next_input == get2nd (fst (head transitions)) = [(head transitions)] ++ (getTransitions state next_input (drop 1 transitions))
   | otherwise = (getTransitions state next_input (tail transitions))

getOutput transitions = [(c, d, e) | ((a, b, c), (d, e)) <- transitions]

callTransitionType pda (input, stack) (a, b, c)
   | a == "" && c == "" = (pushdown pda (b, input, stack)) || (pushdown pda (b, (drop 1 input), stack))
   | c /= "" = (pushdown pda (b, (drop 1 input), c ++ stack))
   | a == (take 1 stack) = (pushdown pda (b, (drop 1 input), (drop 1 stack)))
   | otherwise = False

pushdown :: PDA -> Configuration -> Bool

pushdown (start, accepting, transitions) (curr_state, input, stack)
   | input == "" && stack == "" = True
   | elem True [(callTransitionType (start, accepting, transitions) (input, stack) (a, b, c)) | (a, b, c) <- valid_transitions] = True
   | otherwise = False
   where valid_transitions = getOutput (getTransitions curr_state (take 1 input) transitions)

run :: PDA -> String -> Result

run pda "" = Accept
run (start, accepting, transitions) input
   | pushdown (start, accepting, transitions) (start, input, "") == True = Accept
   | otherwise = Reject

ijk = (1, [2,3], [((1, "a", ""), (1, "a")), ((1, "", ""), (2, "")), ((1, "", ""), (3, "")), ((2, "b", "a"), (2, "")), ((2, "c", ""), (2, "")), ((3, "b", ""), (3, "")), ((3, "c", "a"), (3, ""))])

abc = (1, [2], [((1, "a", ""), (1, "x")), ((1, "a", "c"), (1, "")), ((1, "b", ""), (1, "x")), ((1, "b", "c"), (1, "")), ((1, "c", "x"), (1, "")), ((1, "c", ""), (1, "c")), ((1, "", ""), (2, ""))])
