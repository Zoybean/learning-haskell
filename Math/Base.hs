module Base (encode, decode, recode, Base, BaseB (..), BaseU (..), Sign (..)) where

import Data.List (elemIndex)
import Control.Applicative ((<|>))

-- A generic numeric base
class Base b where
    -- encode integer as a string in the given base
    encode :: b -> Integer -> Maybe String
    -- decode a string in the given base to an integer
    decode :: b -> String -> Maybe Integer
-- change a string's base
recode :: (Base a, Base b) => a -> b -> String -> Maybe String
recode b b' s = decode b s >>= encode b'


-- Specification of an unbalanced integral positional encoding
data BaseU = BaseU { sign   :: Sign   -- whether and how to sign numbers
                   , digits :: [Char] -- digits increasing from zero
                   } deriving Show
data Sign = Unsigned -- no sign character
          | Signed Char -- sign character
          deriving (Eq, Show)
instance Base BaseU where
    encode b@(BaseU m cs) n -- TODO: can have prefix 0s
        | n < 0 -- negative
            = case m of
                Unsigned  -> Nothing -- no way to encode negative integers
                Signed m' -> (m' :) <$> encode b (negate n)
        | n < k -- single digit -- need this to avoid log 0
            = Just $ nextDigit 1 b n : ""
        | otherwise -- multiple digits
            = Just $ countdown i b n
        where
            countdown :: Integer -> BaseU -> Integer -> String
            countdown 0 b n = nextDigit 1 b n : "" -- last digit
            countdown i b n = nextDigit p b n : countdown (i - 1) b n'
                where
                    n' = n `mod` p
                    p = k ^ i
            i = logf k n -- logf only fails if n <= 0. this case is caught in the above guards
            k = fromIntegral $ length cs
            -- placevalue -> base -> remainder -> digit
            nextDigit :: Integer -> BaseU -> Integer -> Char
            nextDigit p b n = getDigit b $ getIndex p n
                where
                    getDigit (BaseU _ cs) n = cs !! n
                    getIndex p n = fromIntegral $ n `div` p
    decode b@(BaseU m _) (n:ds)
        | Signed n == m = negate <$> decode b ds -- only valid at string start, if a negate character is provided
    decode b@(BaseU _ cs) ds = foldl (\t d -> shiftAdd <$> t <*> digitToInt b d) (pure 0) ds
        where
            k :: Integer
            k = fromIntegral $ length cs
            shiftAdd :: Integer -> Integer -> Integer
            shiftAdd t n = k * t + n
            digitToInt :: BaseU -> Char -> Maybe Integer
            digitToInt (BaseU _ cs) d = fromIntegral <$> elemIndex d cs


-- Specification of a balanced integral positional encoding
data BaseB = BaseB { zero :: Char            -- character to represent zero
                   , pairs :: [(Char, Char)] -- (-x,x) character pairs, increasing in magnitude from (-1,1)
                   } deriving Show
instance Base BaseB where
    encode b@(BaseB z cs) n
        | n == 0 = Just [z] -- avoid log 0
        | otherwise = Just $ countdown i b n
        where
            n' = abs n
            i = skewLog k n' -- highest possible index needed
            k = fromIntegral (length cs) * 2 + 1
            countdown :: Integer -> BaseB -> Integer -> String
            countdown 0 b n = nextDigit 0 b n : "" -- last digit
            countdown i b n = nextDigit i b n : countdown (i - 1) b n'
                where
                    n' = n - getIndex i n * p
                    p = k ^ i
            nextDigit :: Integral a => a -> BaseB -> Integer -> Char
            nextDigit i b n = getDigit b $ getIndex i n
            getIndex :: (Integral a, Integral b) => a -> Integer -> b
            getIndex i n = fromIntegral $ signum n * magnitude
                where
                    minn = (k ^ i - 1) `div` 2 -- exclusive min
                    maxn = (k ^ (i + 1) - 1) `div` 2
                    range = fromIntegral $ maxn - minn
                    length = fromIntegral $ fromIntegral (abs n) - minn -- how far n is along range
                    scale = length / range -- how far n is along range, as a ratio
                    divisions = fromIntegral $ (k - 1) `div` 2 -- number of possible magnitudes
                    magnitude = ceiling $ scale * divisions -- how many times to add/subtract this place value from n to take it out of range
            getDigit :: BaseB -> Integer -> Char
            getDigit (BaseB z cs) n = case n `compare` 0 of
                LT -> fst $ cs !! n' -- negative; get the negative digit
                EQ -> z              -- just 0
                GT -> snd $ cs !! n' -- positive; get the positive digit
                where n' = fromIntegral $ abs n - 1 -- excudes 0 digit, so scale down index
            skewLog k n = logf k (n * 2) -- the range is weird with balanced systems
    decode b@(BaseB z cs) ds = foldl (\t d -> shiftAdd <$> t <*> digitToInt b d) (pure 0) ds
        where
            k = fromIntegral (length cs) * 2 + 1
            shiftAdd :: Integer -> Integer -> Integer
            shiftAdd t n = k * t + n
            digitToInt :: BaseB -> Char -> Maybe Integer
            digitToInt (BaseB z cs) d = z' <|> n' <|> p'
                where
                    (n,p) = unzip cs
                    getFrom cs = fromIntegral <$> elemIndex d cs
                    z' = if d == z then Just 0 else Nothing
                    n' = negate . (1 +) <$> getFrom n
                    p' = (1 +) <$> getFrom p


-- N-ary logarithm, rounded down
logf :: (Integral a) => a -> a -> a
logf n x = floor $ logBase n' x'
    where [n',x'] = fromIntegral <$> [n,x]
