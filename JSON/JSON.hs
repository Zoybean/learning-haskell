module JSON (JValue (..)
            ,parseJSON
            ,prettyJSON) where
import System.Environment (getArgs)
import ParseJSON
import PrettyJSON
import JValue

main :: IO ()
main = do
    [ifile] <- getArgs
    text <- readFile ifile
    case parseJSON ifile text of
        Left err -> putStrLn $ err
        Right json -> putStr $ show $ prettyJSON json
