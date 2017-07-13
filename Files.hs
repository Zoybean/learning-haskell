module Files where
import Prelude hiding (lines)
import System.Environment (getArgs)

-- delimit lines with unix line breaks
eolU :: [String] -> String
eolU ls = ls >>= (++ "\n")

-- delimit lines with windows line breaks
eolW :: [String] -> String
eolW ls = ls >>= (++ "\r\n")

-- split string on any style of line breaks
lines :: String -> [String]
lines "" = []
lines s = line : lines (trim rest)
    where   
        (line,rest) = break (`elem` "\r\n") s
        trim :: String -> String
        trim ('\r':'\n':rest) = rest -- windows
        trim ('\n':rest)      = rest -- unix
        trim ('\r':rest)      = rest -- other
        trim ""               = ""   -- end of string

-- enforce unix-style line breaks
eoltoU :: String -> String
eoltoU = eolU . lines

-- enforce windows-style line breaks
eoltoW :: String -> String
eoltoW = eolW . lines

editWith :: (String -> String) -> FilePath -> FilePath -> IO ()
editWith f ifile ofile = do
    text <- readFile ifile
    writeFile ofile $ f text

catWith :: (String -> String) -> FilePath -> IO ()
catWith f ifile = do
    text <- readFile ifile
    print $ f text

main :: IO ()
main = do
    args <- getArgs
    let f = case args of
            ("W":_) -> eoltoW
            ("U":_) -> eoltoU
            _       -> fail "2 - 3 arguments required"
    case args of
        [_,i,o] -> editWith f i o
        [_,i]   -> catWith  f i
        [_]     -> fail "2 - 3 arguments required"
