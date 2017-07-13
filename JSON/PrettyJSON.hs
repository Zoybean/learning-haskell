module PrettyJSON (prettyJSON) where

import Data.List (intercalate)
import Pretty (Doc(..), tab, indent, char, string, (<>), (<+>), (<++>))
import JValue (JValue (..))

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

prettyJSON :: JValue -> Doc
prettyJSON = pretty
    where
        pretty :: JValue -> Doc
        pretty (JObject ps) = prettyCollection "{" "}" prettyPair ps
        pretty (JArray ms)  = prettyCollection "[" "]" pretty ms
        pretty x            = Doc [show x]

        prettyCollection :: String -> String -> (a -> Doc) -> [a] -> Doc
        prettyCollection a b p xs@(_ : _ : _) = -- more than one member: show on multiple lines, with all but the braces indented one level
            surroundDoc a b $ tab 1 $ prettySequence p xs
        prettyCollection a b p xs = -- one or fewer members: show on a single line
            surroundDoc a b $ prettySequence p xs

        prettyPair :: (String, JValue) -> Doc
        prettyPair (name, json) = string (show name ++ " : ") <+> pretty json -- the first line of the value is on the same line as the name

        prettySequence :: (a -> Doc) -> [a] -> Doc -- commas the prettified elements, bar the last
        prettySequence _ [] = Doc []
        prettySequence p [x] = p x
        prettySequence p (x : xs) = (commaLast $ p x) <++> (prettySequence p $ xs)

        commaLast :: Doc -> Doc
        commaLast = (<> char ',')

        surroundDoc :: String -> String -> Doc -> Doc -- wraps a line inline, wraps multiple lines with additional lines
        surroundDoc a b d@(Doc (_ : _ : _)) = string a <++> d <++> string b
        surroundDoc a b d                   = string a <>   d <>   string b

