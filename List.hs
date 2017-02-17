module List where
import Control.Applicative
import Prelude hiding ((++), reverse, map, zip, sum, any, all, filter, length, maybe)
import Data.Char --strings 'n' stuff

-- implementation of parseInt
parseInt :: String -> Int
parseInt ('-':cs) = negate $ parseInt cs
parseInt cs = foldl (\t c -> 10 * t + digitToInt c) 0 cs

-- implementation of (++)
(++) :: [a] -> [a] -> [a]
(++) (x:xs) = (:) x . (++) xs
(++) [] = id

-- implementation of intercalate and intersperse
inter :: (a -> b -> b) -> b -> a -> [a] -> b
inter _ i _ [] = i
inter f i _ [x] = f x i -- coerce x into a member of type b
inter f i s (x:xs) = f x $ f s $ inter f i s xs

intercalate :: [a] -> [[a]] -> [a]
intercalate = inter (++) []

intersperse :: a -> [a] -> [a]
intersperse = inter (:) []

-- implementation of reverse
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = (++) (reverse xs) [x]

-- implementation of map
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x):(map f xs)

-- implementations of range
range :: Int -> Int -> [Int]
range a b = [a..b]

range' :: Int -> Int -> [Int]
range' a b
    | a < b = (a : (range' (a+1) b ))
    | a == b = [a]
    | otherwise = []

-- implementation of zip
zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b):(zip as bs)
zip [] _ = []
zip _ [] = []

-- implementation of python's enumerate
enumerate :: [a] -> [(Int,a)]
enumerate = zip [0..]

-- implementation of slice (list[a:b] in python)
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

-- implementation of next (find the list of items that each follow x)
next x (i:y:ys) -- take the first two items in the list
    | x == i = -- if the first item == x, 
        y : next x (y:ys) -- take the second, and continue to the rest of the list (minus the first element)
    |otherwise = -- not equal, 
        next x (y:ys) -- so skip that element
next _ [_] = [] -- if there's no second element, then stop
next _ _ = [] -- if the list is empty, stop
-- implementations of fold
foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' _ i [] = i
foldr' f i (x:xs) = f x $ foldr' f i xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ i [] = i
foldl' f i (x:xs) = foldl' f (f i x) xs

-- implementation of sum
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

-- implementations of any and all
all :: Foldable t => (a -> Bool) -> t a -> Bool 
all f = foldr ((&&).f) True 

any :: Foldable t => (a -> Bool) -> t a -> Bool 
any f = foldr ((||).f) False

-- implementation of filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
    | f x = x:(filter f xs)
    | otherwise = filter f xs

-- implementation of length
length :: Foldable t => t a -> Int
length = foldr ((+).(const 1)) 0
length' :: [a] -> Int
length' = sum . (map $ const 1)

-- implementation of maybe and fromMaybe
maybe :: a -> (b -> a) -> (Maybe b) -> a
maybe d _ Nothing = d
maybe _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe = (`maybe` id)

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

-- testing out monads and bind
mayadd :: Maybe Int -> Maybe Int -> Maybe Int
mayadd ma mb = ma >>= (\a -> mb >>= (\b -> Just (a + b)))

main = do
    mapM_ print ["hello" ++ " world" == "hello world"
                ,reverse "nope" == "epon"
                ,map(+2) [3] == [5]
                ,fmap (+2) (Just 3) == Just 5
                ,range 1 5 == [1,2,3,4,5]
                ,range' 1 5 == [1,2,3,4,5]
                ,range 5 1 == []
                ,range' 5 1 == []
                ,zip [1,2,3] [4,5,6,7] == [(1,4),(2,5),(3,6)]
                ,zip [1,2,3,7] [4,5,6] == [(1,4),(2,5),(3,6)]
                ,mayadd (Just 1) (Just 2) == Just 3
                ,mayadd Nothing (Just 2) == Nothing
                ,mayadd (Just 1) Nothing == Nothing
                ,foldl (-) 0 [1,2,3,4] == -10 -- (((1 - 2) - 3) - 4) - 0
                ,foldl' (-) 0 [1,2,3,4] == -10
                ,foldr (-) 0 [1,2,3,4] == -2 -- 1 - (2 - (3 - (4 - 0)))
                ,foldr' (-) 0 [1,2,3,4] == -2
                ,floop (map Just [1,2,3,4,5]) == Just [1,2,3,4,5]
                ,floop (Nothing : ( map Just [1,2,3,4,5])) == Nothing
                ,floop' (map Just [1,2,3,4,5]) == Just [1,2,3,4,5]
                ,floop' (Nothing : ( map Just [1,2,3,4,5])) == Nothing
                ,sequence' (map Just [1,2,3,4,5]) == Just [1,2,3,4,5]
                ,sequence' (Nothing : ( map Just [1,2,3,4,5])) == Nothing
                ]
