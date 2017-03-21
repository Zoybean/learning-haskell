module Base (intToStr, strToInt, strToStr, ub58, ub10, ub2, sb10) where

import Data.List (elemIndex)
type Base = (Maybe Char,[Char])

intToChar :: Base -> Integer -> Char
intToChar (_,cs) n = cs !! fromIntegral n

intToStr :: Base -> Integer -> Maybe String
intToStr b@(m,cs) n
    | n < 0 -- negative
        = case m of
            Nothing -> Nothing
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

strToStr :: Base -> Base -> String -> Maybe String
strToStr b b' s = strToInt b s >>= intToStr b'

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

ub58 :: Base
ub58 = (Nothing, "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

ub10 :: Base
ub10 = (Nothing, "0123456789")

sb10 :: Base
sb10 = (Just '-', "0123456789")

ub2 :: Base
ub2 = (Nothing, "01")

logf :: (Integral a) => a -> a -> a -- N-ary logarithm, rounded down
logf n x = floor $ log x' / log n'
    where
        [x',n'] = fromIntegral <$> [x,n]
