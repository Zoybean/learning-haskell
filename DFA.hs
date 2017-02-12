module DFA where
import qualified Data.Map as Map (Map, empty, insert, lookup)

type State = Int
data DFA = DFA State (Map.Map (State,Char) State) [State] deriving Show

empty :: DFA -- start at 0, no accept, no arcs
empty = DFA 0 Map.empty []

accepts :: [State] -> DFA -> DFA -- add a number of accept states
accepts qs (DFA qi d f) = DFA qi d (qs ++ f)

arcs :: [(State,Char,State)] -> DFA -> DFA -- add arcs between states
arcs [] a = a
arcs ((qf,c,qt):as) a = arcs as $ arc qf c qt a
    where
        arc :: State -> Char -> State -> DFA -> DFA -- add a single arc from qf on c to qt
        arc qf c qt (DFA qi d f) = DFA qi (Map.insert (qf,c) qt d) f

test :: String -> DFA -> Maybe Bool -- test a string with the DFA. Nothing indicates an unrecognised character was received
test [] (DFA q _ f) = Just $ elem q f
test (c:cs) a = increment c a >>= test cs

increment :: Char -> DFA -> Maybe DFA -- return a new DFA starting at the state attained by following the arc at c, if it exists
increment c (DFA q d f) = Map.lookup (q,c) d >>= (\q -> Just $ DFA q d f)

main :: IO ()
main = do
    let tests = ["011000" -- bounded, div by 3
                ,"111" -- bounded, all 1, div by 3
                ,"11" -- bounded, all 1
                ,"0" -- single character
                ,"" -- all 1, div by 3
                ,"0100101"
                ]
    do
        let
            a = [(0,'1',1)
                ,(1,'1',2)
                ,(2,'1',0)
                ,(0,'0',4)
                ,(1,'0',4)
                ,(2,'0',4)
                ,(4,'0',4)
                ,(4,'1',4)
                ]
            d = arcs a $
                accepts [0] $
                empty -- any string with a length divisible by 3, that only contains 1s
        mapM_ print $ map (flip test d) $ tests
    putStrLn ""
    do
        let
            a = [(0,'0',1)
                ,(1,'0',1)
                ,(1,'1',2)
                ,(2,'1',2)
                ,(2,'0',1)
                ,(0,'1',3)
                ,(3,'1',3)
                ,(3,'0',4)
                ,(4,'0',4)
                ,(4,'1',3)
                ]
            d = arcs a $
                accepts [1,3] $
                empty -- any string starting and ending in the same character: x|x(.*)x
        mapM_ print $ map (flip test d) $ tests
        
        putStrLn ""
        -- this will return Nothing if you use an unrecognised character, or Just true/false to indicate if the string was accepted
        print $ test "your-string-here" d
