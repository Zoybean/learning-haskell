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
    ,["dent :: String -> String"
    ,"dent s = \"    \" ++ s"
    ,""
    ,"rewrap :: [[String]] -> [String]"
    ,"rewrap (l:ls) = app \"]\" $ ([l] >>= ins \"[\") ++ (ls >>= ins \",\")"
    ,"    where"
    ,"        ins :: String -> [String] -> [String]"
    ,"        ins x (s:ss) = [x ++ s] ++ ss"
    ,"        app :: String -> [String] -> [String]"
    ,"        app x [t] = [x ++ t]"
    ,"        app x (s:ss) = [s] ++ app x ss"
    ,"wrap :: [String] -> [String]"
    ,"wrap ss = go $ escape <$> ss"
    ,"    where"
    ,"        go :: [String] -> [String]"
    ,"        go []     = [\"[]\"]"
    ,"        go (s:ss) = [\"[\" ++ s] ++ ((\",\" ++) <$> ss) ++ [\"]\"]"
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
dent s = "    " ++ s

rewrap :: [[String]] -> [String]
rewrap (l:ls) = app "]" $ ([l] >>= ins "[") ++ (ls >>= ins ",")
    where
        ins :: String -> [String] -> [String]
        ins x (s:ss) = [x ++ s] ++ ss
        app :: String -> [String] -> [String]
        app x [t] = [x ++ t]
        app x (s:ss) = [s] ++ app x ss
wrap :: [String] -> [String]
wrap ss = go $ escape <$> ss
    where
        go :: [String] -> [String]
        go []     = ["[]"]
        go (s:ss) = ["[" ++ s] ++ (("," ++) <$> ss) ++ ["]"]

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
