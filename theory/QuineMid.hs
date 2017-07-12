main :: IO ()
main = do
    putStr $ unlines $ (list !! 0)
    putStr $ unlines $ dent <$> (rewrap $ wrap <$> list)
    putStr $ unlines $ (list !! 1)

list :: [[String]]
list =
    [["main :: IO ()"
    ,"main = do"
    ,"    putStr $ unlines $ (list !! 0)"
    ,"    putStr $ unlines $ dent <$> (rewrap $ wrap <$> list)"
    ,"    putStr $ unlines $ (list !! 1)"
    ,""
    ,"list :: [[String]]"
    ,"list ="
    ]
    ,[""
    ,"dent :: String -> String"
    ,"dent = (\"    \" ++)"
    ,""
    ,"rewrap :: [[String]] -> [String]"
    ,"rewrap (l:ls) = app \"]\" $ (pure l >>= pre \"[\") ++ (ls >>= pre \",\")"
    ,""
    ,"pre :: [a] -> [[a]] -> [[a]]"
    ,"pre x []     = [x]"
    ,"pre x (s:ss) = [x ++ s] ++ ss"
    ,""
    ,"fil :: [a] -> [[a]] -> [[a]]"
    ,"fil x []     = []"
    ,"fil x (s:ss) = [s] ++ ((x ++) <$> ss)"
    ,""
    ,"app :: [a] -> [[a]] -> [[a]]"
    ,"app x []     = [x]"
    ,"app x [t]    = [t ++ x]"
    ,"app x (s:ss) = [s] ++ app x ss"
    ,""
    ,"wrap :: [String] -> [String]"
    ,"wrap ss = (fil \",\" $ pre \"[\" $ escape <$> ss) ++ [\"]\"]"
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
rewrap []     = ["[]"]
rewrap (l:ls) = app "]" $ (pure l >>= pre "[") ++ (ls >>= pre ",")

pre :: [a] -> [[a]] -> [[a]]
pre x []     = [x]
pre x (s:ss) = [x ++ s] ++ ss

fil :: [a] -> [[a]] -> [[a]]
fil x []     = []
fil x (s:ss) = [s] ++ ((x ++) <$> ss)

app :: [a] -> [[a]] -> [[a]]
app x []     = [x]
app x [t]    = [t ++ x]
app x (s:ss) = [s] ++ app x ss

wrap :: [String] -> [String]
wrap ss = (fil "," $ pre "[" $ escape <$> ss) ++ ["]"]

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
