main :: IO ()
main = do
    mapM_ putStrLn header
    mapM_ putStrLn footer

footer :: [String]
footer = wrap header
    where
        wrap :: [String] -> [String]
        wrap (s:ss) = ("    " ++) <$> ["[" ++ escape s] ++ (("," ++) . escape <$> ss) ++ ["]"]

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
    ,"    mapM_ putStrLn header"
    ,"    mapM_ putStrLn footer"
    ,""
    ,"footer :: [String]"
    ,"footer = wrap header"
    ,"    where"
    ,"        wrap :: [String] -> [String]"
    ,"        wrap (s:ss) = (\"    \" ++) <$> [\"[\" ++ escape s] ++ ((\",\" ++) . escape <$> ss) ++ [\"]\"]"
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
