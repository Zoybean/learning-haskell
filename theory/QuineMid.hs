main :: IO ()
main = do
    putStr $ unlines $ file !! 0
    putStr $ unlines $ dent <$> (rewrap $ wrap . (escape <$>) <$> file)
    putStr $ unlines $ file !! 1

file :: [[String]]
file =
    [["main :: IO ()"
    ,"main = do"
    ,"    putStr $ unlines $ file !! 0"
    ,"    putStr $ unlines $ dent <$> (rewrap $ wrap . (escape <$>) <$> file)"
    ,"    putStr $ unlines $ file !! 1"
    ,""
    ,"file :: [[String]]"
    ,"file ="
    ]
    ,[""
    ,"dent :: String -> String"
    ,"dent = (\"    \" ++)"
    ,""
    ,"rewrap :: [[String]] -> [String]"
    ,"rewrap = concat . pre (pre (\"[\" ++)) . fil (pre (\",\" ++)) . app (app (++ \"]\"))"
    ,""
    ,"pre :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"pre f []     = [f []]"
    ,"pre f (x:xs) = [f x] ++ xs"
    ,""
    ,"fil :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"fil _ []     = []"
    ,"fil f (x:xs) = [x] ++ (f <$> xs)"
    ,""
    ,"app :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"app f []     = [f []]"
    ,"app f [x]    = [f x]"
    ,"app f (x:xs) = [x] ++ app f xs"
    ,""
    ,"wrap :: [String] -> [String]"
    ,"wrap ss = (pre (\"[\" ++) $ fil (\",\" ++) $ ss) ++ [\"]\"]"
    ,""
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

dent :: String -> String
dent = ("    " ++)

rewrap :: [[String]] -> [String]
rewrap = concat . pre (pre ("[" ++)) . fil (pre ("," ++)) . app (app (++ "]"))

pre :: ([a] -> [a]) -> [[a]] -> [[a]]
pre f []     = [f []]
pre f (x:xs) = [f x] ++ xs

fil :: ([a] -> [a]) -> [[a]] -> [[a]]
fil _ []     = []
fil f (x:xs) = [x] ++ (f <$> xs)

app :: ([a] -> [a]) -> [[a]] -> [[a]]
app f []     = [f []]
app f [x]    = [f x]
app f (x:xs) = [x] ++ app f xs

wrap :: [String] -> [String]
wrap ss = (pre ("[" ++) $ fil ("," ++) $ ss) ++ ["]"]

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
