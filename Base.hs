module Base (intToStr, strToInt, strToStr, ub58, ub10, ub2, sb10, Base) where

import Data.List (elemIndex)

intToChar :: Base -> Integer -> Char
intToChar (_,cs) n = cs !! fromIntegral n

-- encode integer as a string in the given base
intToStr :: Base -> Integer -> Maybe String
intToStr b@(m,cs) n
    | n < 0 -- negative
        = case m of
            Nothing -> Nothing -- unsigned: no way to encode negative integers
            Just m' -> (m' :) <$> intToStr b (negate n)
    | n < fromIntegral (length cs) -- single digit -- need this to avoid log 0
        = Just $ intToChar b n : ""
    | otherwise -- multiple digits
        = Just $ countdown i b n
        where
            countdown :: Integer -> Base -> Integer -> String
            countdown 0 b n = intToChar b n : "" -- last digit
            countdown i b n = intToChar b d : countdown (i - 1) b n'
                where
                    n' = n `mod` p
                    d  = n `div` p
                    p = k ^ i
            i = logf k n -- logf only fails if n <= 0. this case is caught in the above guards
            k = fromIntegral $ length cs

-- change a string's base
strToStr :: Base -> Base -> String -> Maybe String
strToStr b b' s = strToInt b s >>= intToStr b'

-- decode a string in the given base to an integer
strToInt :: Base -> String -> Maybe Integer
strToInt b@(m,_) (n:ds)
    | Just n == m = negate <$> strToInt b ds -- only valid at string start, if a negate character is provided
strToInt b@(_,cs) ds = foldl (\t d -> shiftAdd <$> t <*> digitToInt b d) (pure 0) ds
    where
        k :: Integer
        k = fromIntegral $ length cs
        shiftAdd :: Integer -> Integer -> Integer
        shiftAdd = (\t n -> k * t + n)
        digitToInt :: Base -> Char -> Maybe Integer
        digitToInt (_,cs) d = fromIntegral <$> elemIndex d cs

-- (Sign character, digits)
-- TODO: use a record type
type Base = (Maybe Char,[Char])

-- unsigned base 64
-- source: wikipedia: https://en.wikipedia.org/wiki/Base64#Design
ub64mime :: Base
ub64mime = (Nothing, ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/")

-- unsigned base 58
-- source: wikipedia: https://en.wikipedia.org/wiki/Base58
ub58bitc :: Base
ub58bitc = (Nothing, ['1'..'9'] ++ "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
ub58ripl :: Base
ub58ripl = (Nothing, "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz")
ub58flkr :: Base
ub58flkr = (Nothing, ['1'..'9'] ++ "abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ")

-- base 10
ub10 :: Base
ub10 = (Nothing, "0123456789")
sb10 :: Base
sb10 = (Just '-', "0123456789")

-- unsigned base 2
ub2 :: Base
ub2 = (Nothing, "01")

logf :: (Integral a) => a -> a -> a -- N-ary logarithm, rounded down
logf n x = floor $ log x' / log n'
    where
        [x',n'] = fromIntegral <$> [x,n]
