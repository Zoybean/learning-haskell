module JSON (parseJSON
            ,prettyJSON
            ,JValue (..)
            ) where
import Text.Parsec
import Data.List (intercalate)
import System.Environment (getArgs)

data JValue = JObject [(String,JValue)]
            | JArray [JValue]
            | JNumber Float
            | JString String
            | JBool Bool
            | JNull
            deriving Eq

instance Show (JValue) where
    show (JObject xs)  = surround "{" "}" $ intercalate "," (map showpair xs)
        where showpair (k,v) = show k ++ ":" ++ show v
    show (JArray ms)   = surround "[" "]" $ intercalate "," (map show ms)
    show (JNumber n)   = show n
    show (JString s)   = show s
    show (JBool True)  = "true"
    show (JBool False) = "false"
    show JNull         = "null"

surround :: [a] -> [a] -> [a] -> [a]
surround a b xs = a ++ xs ++ b

prettyJSON :: JValue -> String
prettyJSON = unlines . pretty
    where
        pretty :: JValue -> [String] -- each string sits on its own line. Makes it easier to indent
        pretty (JObject ps) = prettyCollection "{" "}" prettyPair ps
        pretty (JArray ms) = prettyCollection "[" "]" pretty ms
        pretty x = [show x]
        prettyCollection a b p xs@(_ : _ : _) = -- more than one member: show on multiple lines, with all but the braces indented one level
            surround [a] [b] $ prettySequence p xs >>= pure . ('\t' : )
        prettyCollection a b p xs = -- one or fewer members: show on a single line
            prettySequence p xs >>= pure . surround a b

        prettyPair :: (String, JValue) -> [String]
        prettyPair (name, json) = let (line : rest) = pretty json
                                  in (show name ++ " : " ++ line) : rest -- the first line of the value is on the same line as the name

        prettySequence :: (a -> [String]) -> [a] -> [String] -- commas the prettified elements, bar the last
        prettySequence p (x : y : ys) = (commaLast $ p x) ++ (prettySequence p $ y : ys)
        prettySequence p [y] = p y
        prettySequence _ [] = []

        commaLast [x] = [x ++ ","]
        commaLast (x : xs) = x : commaLast xs
        commaLast [] = []

parseJSON :: String -> String -> Either String JValue
parseJSON fname input = case parse parseJValue fname input of
    Left err -> Left ("parse error: " ++ show err)
    Right json -> Right json

parseJValue :: Stream s m Char => ParsecT s u m JValue
parseJValue = do
            spaces
            json <- (
                parseJObject
                <|> parseJArray
                <|> parseJNumber
                <|> parseJString
                <|> parseJBool
                <|> parseJNull)
            spaces
            return json
parseJObject :: Stream s m Char => ParsecT s u m JValue
parseJObject = do
    char '{'
    spaces
    ms <- members
    spaces
    char '}'
    return $ JObject ms
    where
        member = do
            name <- parseString
            spaces
            char ':'
            spaces
            value <- parseJValue
            return (name,value)
        members = do
            m <- member
            ms <- optionMaybe (do
                spaces
                char ','
                spaces
                ms <- members
                return ms)
            return $ case ms of
                Nothing -> [m]
                Just ms -> (m : ms)

parseJArray :: Stream s m Char => ParsecT s u m JValue
parseJArray = do
    char '['
    spaces
    ms <- members
    spaces
    char ']'
    return $ JArray ms
    where
        member = do
            value <- parseJValue
            return value
        members = do
                    m <- member
                    ms <- optionMaybe (do
                        spaces
                        char ','
                        spaces
                        ms <- members
                        return ms)
                    return $ case ms of
                        Nothing -> [m]
                        Just ms -> (m : ms)

parseJNumber :: Stream s m Char => ParsecT s u m JValue
parseJNumber = do
    val <- many1 digit
    dec <- option "0" (do
            char '.'
            many1 digit)
    return $ JNumber $ read (val ++ "." ++ dec)

parseString :: Stream s m Char => ParsecT s u m String
parseString = do
    char '"'
    val <- many $ noneOf "\""
    char '"'
    return $ val

parseJString :: Stream s m Char => ParsecT s u m JValue
parseJString = do
    val <- parseString
    return $ JString val

parseJBool :: Stream s m Char => ParsecT s u m JValue
parseJBool = do
    bool <- string "true" <|> string "false"
    return $ case bool of
        "true"  -> JBool True
        "false" -> JBool False

parseJNull :: Stream s m Char => ParsecT s u m JValue
parseJNull = do
    string "null"
    return JNull

main :: IO ()
main = do
    [ifile] <- getArgs
    text <- readFile ifile
    case parseJSON ifile text of
        Left err -> putStrLn $ err
        Right json -> putStr $ prettyJSON json
