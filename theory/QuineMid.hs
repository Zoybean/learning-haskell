-- Print the source code
main :: IO ()
main = do
    putStr $ unlines $ file !! 0 -- the leading half
    putStr $ unlines $ dent <$> (rewrap $ wrap . (escape <$>) <$> file) -- the string copy
    putStr $ unlines $ file !! 1 -- and the trailing half

-- The internal representation of the program's own source code
file :: [[String]]
file =
    [["-- Print the source code"
    ,"main :: IO ()"
    ,"main = do"
    ,"    putStr $ unlines $ file !! 0 -- the leading half"
    ,"    putStr $ unlines $ dent <$> (rewrap $ wrap . (escape <$>) <$> file) -- the string copy"
    ,"    putStr $ unlines $ file !! 1 -- and the trailing half"
    ,""
    ,"-- The internal representation of the program's own source code"
    ,"file :: [[String]]"
    ,"file ="
    ]
    ,[""
    ,"-- Indent a line with 4 spaces"
    ,"dent :: String -> String"
    ,"dent = (\"    \" ++)"
    ,""
    ,"-- Perform some operation to the first list"
    ,"pre :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"pre f []     = [f []]"
    ,"pre f (x:xs) = [f x] ++ xs"
    ,""
    ,"-- Perform some operation to all but the first list"
    ,"fil :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"fil _ []     = []"
    ,"fil f (x:xs) = [x] ++ (f <$> xs)"
    ,""
    ,"-- Perform some operation to the last list"
    ,"app :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"app f []     = [f []]"
    ,"app f [x]    = [f x]"
    ,"app f (x:xs) = [x] ++ app f xs"
    ,""
    ,"-- Make a list of strings resemble the lines of a list of strings in haskell syntax"
    ,"wrap ss = (pre (\"[\" ++) -- prepend a '[' to the first line"
    ,"          $ fil (\",\" ++) $ ss) -- prepend a ',' to all but the first line"
    ,"          ++ [\"]\"] -- append a line which contains a ']'"
    ,""
    ,"-- Make a list of lists of strings resemble the lines of a list of lists of strings in haskell syntax"
    ,"rewrap :: [[String]] -> [String]"
    ,"rewrap = concat -- Concatenate the lists"
    ,"         . pre (pre (\"[\" ++)) -- Prepend a '[' to the first line of the first list"
    ,"         . fil (pre (\",\" ++)) -- Prepend a ',' to the first line of all but the first list"
    ,"         . app (app (++ \"]\")) -- Append a ']' to the last line of the last list"
    ,""
    ,"-- Escape a string"
    ,"escape :: String -> String"
    ,"escape s = [qot] ++ (s >>= escChar) ++ [qot]"
    ,"    where"
    ,"        esc = '\\\\'"
    ,"        qot = '\"'"
    ,"        escChar :: Char -> String"
    ,"        escChar chr"
    ,"            | chr == esc = [esc, esc]"
    ,"            | chr == qot = [esc, qot]"
    ,"            | otherwise  = [chr]"
    ]]

-- Indent a line with 4 spaces
dent :: String -> String
dent = ("    " ++)

-- Perform some operation to the first list
pre :: ([a] -> [a]) -> [[a]] -> [[a]]
pre f []     = [f []]
pre f (x:xs) = [f x] ++ xs

-- Perform some operation to all but the first list
fil :: ([a] -> [a]) -> [[a]] -> [[a]]
fil _ []     = []
fil f (x:xs) = [x] ++ (f <$> xs)

-- Perform some operation to the last list
app :: ([a] -> [a]) -> [[a]] -> [[a]]
app f []     = [f []]
app f [x]    = [f x]
app f (x:xs) = [x] ++ app f xs

-- Make a list of strings resemble the lines of a list of strings in haskell syntax
wrap ss = (pre ("[" ++) -- prepend a '[' to the first line
          $ fil ("," ++) $ ss) -- prepend a ',' to all but the first line
          ++ ["]"] -- append a line which contains a ']'

-- Make a list of lists of strings resemble the lines of a list of lists of strings in haskell syntax
rewrap :: [[String]] -> [String]
rewrap = concat -- Concatenate the lists
         . pre (pre ("[" ++)) -- Prepend a '[' to the first line of the first list
         . fil (pre ("," ++)) -- Prepend a ',' to the first line of all but the first list
         . app (app (++ "]")) -- Append a ']' to the last line of the last list

-- Escape a string
escape :: String -> String
escape s = [qot] ++ (s >>= escChar) ++ [qot]
    where
        esc = '\\'
        qot = '"'
        escChar :: Char -> String
        escChar chr
            | chr == esc = [esc, esc]
            | chr == qot = [esc, qot]
            | otherwise  = [chr]
