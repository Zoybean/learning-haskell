main :: IO ()
main = do
    putStr $ unlines $ file
    putStr $ unlines $ dent <$> (wrap $ escape <$> file)

dent :: String -> String
dent = ("    " ++)

wrap :: [String] -> [String]
wrap ss = (pre ("[" ++) $ fil ("," ++) $ ss) ++ ["]"]
    where
        pre :: ([a] -> [a]) -> [[a]] -> [[a]]
        pre f []     = [f []]
        pre f (x:xs) = [f x] ++ xs
        fil :: ([a] -> [a]) -> [[a]] -> [[a]]
        fil _ []     = []
        fil f (x:xs) = [x] ++ (f <$> xs)

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

file :: [String]
file =
    ["main :: IO ()"
    ,"main = do"
    ,"    putStr $ unlines $ file"
    ,"    putStr $ unlines $ dent <$> (wrap $ escape <$> file)"
    ,""
    ,"dent :: String -> String"
    ,"dent = (\"    \" ++)"
    ,""
    ,"wrap :: [String] -> [String]"
    ,"wrap ss = (pre (\"[\" ++) $ fil (\",\" ++) $ ss) ++ [\"]\"]"
    ,"    where"
    ,"        pre :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"        pre f []     = [f []]"
    ,"        pre f (x:xs) = [f x] ++ xs"
    ,"        fil :: ([a] -> [a]) -> [[a]] -> [[a]]"
    ,"        fil _ []     = []"
    ,"        fil f (x:xs) = [x] ++ (f <$> xs)"
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
    ,"file :: [String]"
    ,"file ="
    ]
