data Day =  Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday
            deriving (Enum, Show)

data Month = Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec
             deriving (Enum, Read)

type Date = (Int,Month,Int)
