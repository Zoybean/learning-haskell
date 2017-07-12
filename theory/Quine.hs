main :: IO ()
main = do
    putStr $ unlines $ header
    putStr $ unlines $ dent <$> wrap header

dent :: String -> String
dent = ("    " ++)

wrap :: [String] -> [String]
wrap ss = (fil "," $ pre "[" $ escape <$> ss) ++ ["]"]
    where
        pre :: [a] -> [[a]] -> [[a]]
        pre x []     = [x]
        pre x (s:ss) = [x ++ s] ++ ss
        fil :: [a] -> [[a]] -> [[a]]
        fil x []     = []
        fil x (s:ss) = [s] ++ ((x ++) <$> ss)

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

header :: [String]
header =
    ["main :: IO ()"
    ,"main = do"
    ,"    putStr $ unlines $ header"
    ,"    putStr $ unlines $ dent <$> wrap header"
    ,""
    ,"dent :: String -> String"
    ,"dent = (\"    \" ++)"
    ,""
    ,"wrap :: [String] -> [String]"
    ,"wrap ss = (fil \",\" $ pre \"[\" $ escape <$> ss) ++ [\"]\"]"
    ,"    where"
    ,"        pre :: [a] -> [[a]] -> [[a]]"
    ,"        pre x []     = [x]"
    ,"        pre x (s:ss) = [x ++ s] ++ ss"
    ,"        fil :: [a] -> [[a]] -> [[a]]"
    ,"        fil x []     = []"
    ,"        fil x (s:ss) = [s] ++ ((x ++) <$> ss)"
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
    ,""
    ,"header :: [String]"
    ,"header ="
    ]
