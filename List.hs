module List where
import Control.Applicative
import Data.Char (digitToInt)
import Data.Universe.Helpers (diagonal)
import Data.List (nub)

-- implementation of parseInt
parseInt :: String -> Int
parseInt ('-':cs) = negate $ parseInt cs
parseInt cs = foldl (\t c -> 10 * t + digitToInt c) 0 cs

-- python range (but inclusive)
range :: Integral n => n -> n -> [n]
range a b = [a..b]

range' :: Integral n => n -> n -> [n]
range' a b
    | a < b = (a : (range' (a+1) b ))
    | a == b = [a]
    | otherwise = []

-- python enumerate
enumerate :: Integral n => [a] -> [(n,a)]
enumerate = zip [0..]

-- python slice (list[a:b])
slice :: Int -> Int -> [a] -> [a]
slice a b = take (b - a) . drop a

-- insert element at the first point where preticate is True
insertif :: (a -> a -> Bool) -> a -> [a] -> [a]
insertif p i (x:xs)
    | p i x = i : x : xs
    | otherwise = x : insertif p i xs
insertif _ _ [] = []

-- insert each element, at the first point where the preticate is True, in order
injectif :: (a -> a -> Bool) -> [a] -> [a] -> [a]
injectif p (i:is) (x:xs)
    | p i x = i : injectif p is (x:xs)
    | otherwise = x : injectif p (i:is) xs
injectif _ [] xs = xs
injectif _ _ [] = []

-- find the list of items that each follow x
next x (i:y:ys) -- take the first two items in the list
    | x == i = -- if the first item == x, 
        y : next x (y:ys) -- take the second, and continue to the rest of the list (minus the first element)
    |otherwise = -- not equal, 
        next x (y:ys) -- so skip that element
next _ [_] = [] -- if there's no second element, then stop
next _ _ = [] -- if the list is empty, stop

-- list intersection which works on infinite lists
isect :: Eq a => [a] -> [a] -> [a]
isect = isectBy (==)

isectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
isectBy eq xs = catMaybes . diagonal . map matches
    where matches y = [if eq x y then Just x else Nothing | x <- xs] -- ensures that non-yields are interleaved with yields

--mFilter :: (a -> Bool) -> [a] -> [Maybe a]
--mFilter f xs = [if f x then Just x else Nothing | x <- xs]

--mFilter :: (a -> Bool) -> [a] -> [Maybe a]
--mFilter f (x:xs)
--    | f x = Just x : mFilter f xs
--    | otherwise = Nothing : mFilter f xs

boolMaybe :: (a -> Bool) -> a -> Maybe a
boolMaybe f x
    | f x = Just x
    | otherwise = Nothing

mFilter :: (a -> Bool) -> [a] -> [Maybe a]
mFilter = map . boolMaybe

-- implementation of fromMaybe, and catMaybes
fromMaybe :: a -> Maybe a -> a
fromMaybe = (`maybe` id)

catMaybes :: (Foldable t, Alternative t) => t (Maybe a) -> t a
catMaybes = foldr
    (\x -> case x of
        Just x -> (pure x <|>)
        Nothing -> id
    ) empty

-- function to floop a list of maybes into a maybe of a list. yeah, not a great description
floop :: (Foldable t, Alternative t) => t (Maybe a) -> Maybe (t a)
floop xs =
    let
        f :: (Foldable t, Alternative t) => Maybe a -> Maybe (t a) -> Maybe (t a)
        f Nothing = const Nothing
        f (Just x) = fmap ((<|>) $ pure x)
        --f x ys = maybe Nothing (flip (fmap . (<|>) . pure) ys) x -- is equivalent
    in
        foldr f (Just empty) xs

-- simpler, more limited example
floop' :: [Maybe a] -> Maybe [a]
floop' [] = Just []
floop' (Nothing:_) = Nothing
floop' (Just x:xs) = fmap (x:) . floop $ xs
--floop' xs = foldr (fmap . (:)) (Just []) xs

--floop'' :: (Foldable t, Alternative t) => t (Maybe a) -> Maybe (t a)
--floop'' xs = foldr (\x ys -> maybe Nothing (flip (fmap . (<|>) . pure) ys) x) (Just empty) xs --needlessly equivalent

-- I finally get this one! -- Source: http://stackoverflow.com/a/41986867/6112457
sequence' :: (Alternative t, Foldable t, Applicative a) => t (a b) -> a (t b)
sequence' = foldr inject (pure empty)
  where inject = liftA2 prepend
        prepend = (<|>) . pure

-- apply each function with the given argument
applyAll :: [(a -> b)] -> a -> [b]
applyAll fs x = map ($x) fs

main = do
    mapM_ print [floop (map Just [1,2,3,4,5]) == Just [1,2,3,4,5]
                ,floop (Nothing : ( map Just [1,2,3,4,5])) == Nothing
                ,floop' (map Just [1,2,3,4,5]) == Just [1,2,3,4,5]
                ,floop' (Nothing : ( map Just [1,2,3,4,5])) == Nothing
                ,sequence' (map Just [1,2,3,4,5]) == Just [1,2,3,4,5]
                ,sequence' (Nothing : ( map Just [1,2,3,4,5])) == Nothing
                ]
