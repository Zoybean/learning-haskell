-- Print the string copy of the source code
main :: IO ()
main = do
    putStr $ unlines $ file -- the program body
    putStr $ unlines $ dent <$> (wrap $ escape <$> file) -- the string copy

-- Indent a line with 4 spaces
dent :: String -> String
dent = ("    " ++)

-- Make a list of strings resemble the lines of a list of strings in haskell syntax
wrap :: [String] -> [String]
wrap ss = (pre ("[" ++) -- prepend a '[' to the first line
          $ fil ("," ++) $ ss) -- prepend a ',' to all but the first line
          ++ ["]"] -- append a line which contains a ']'
    where
        pre :: ([a] -> [a]) -> [[a]] -> [[a]]
        pre f []     = [f []]
        pre f (x:xs) = [f x] ++ xs
        fil :: ([a] -> [a]) -> [[a]] -> [[a]]
        fil _ []     = []
        fil f (x:xs) = [x] ++ (f <$> xs)

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

-- The internal representation of the program's own source code
file :: [String]
file =
    ["-- Print the string copy of the source code"
    ,"main :: IO ()"
    ,"main = do"
    ,"    putStr $ unlines $ file -- the program body"
    ,"    putStr $ unlines $ dent <$> (wrap $ escape <$> file) -- the string copy"
    ,""
    ,"-- Indent a line with 4 spaces"
    ,"dent :: String -> String"
    ,"dent = (\"    \" ++)"
    ,""
    ,"-- Make a list of strings resemble the lines of a list of strings in haskell syntax"
    ,"wrap :: [String] -> [String]"
    ,"wrap ss = (pre (\"[\" ++) -- prepend a '[' to the first line"
    ,"          $ fil (\",\" ++) $ ss) -- prepend a ',' to all but the first line"
    ,"          ++ [\"]\"] -- append a line which contains a ']'"
    ,"    where"
    ,"        pre :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"        pre f []     = [f []]"
    ,"        pre f (x:xs) = [f x] ++ xs"
    ,"        fil :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"        fil _ []     = []"
    ,"        fil f (x:xs) = [x] ++ (f <$> xs)"
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
    ,""
    ,"-- The internal representation of the program's own source code"
    ,"file :: [String]"
    ,"file ="
    ]
