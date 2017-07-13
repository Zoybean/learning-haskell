{-# LANGUAGE FlexibleContexts #-}
module ParseJSON (parseJSON) where

import Text.Parsec (parse, optionMaybe, option, many1, many, noneOf, spaces, string, char, digit, (<|>), ParsecT, Stream)
import JValue (JValue (..))

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
