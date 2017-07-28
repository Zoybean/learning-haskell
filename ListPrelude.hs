import Prelude hiding ((++), all, any, elem, filter, length, map, maybe, reverse, sum, zip)

-- implementation of (++)
(++) :: [a] -> [a] -> [a]
(++) (x:xs) = (:) x . (++) xs
(++) [] = id

-- implementations of any and all
all :: Foldable t => (a -> Bool) -> t a -> Bool 
all f = foldr ((&&).f) True 

any :: Foldable t => (a -> Bool) -> t a -> Bool 
any f = foldr ((||).f) False

-- implementation of elem
elem :: (Eq a, Foldable t) => a -> t a -> Bool
elem x = any (== x)

-- implementation of filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
    | f x = x : filter f xs
    | otherwise = filter f xs

-- implementations of folds on lists
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ i [] = i
foldl' f i (x:xs) = foldl' f (f i x) xs

foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' _ i [] = i
foldr' f i (x:xs) = f x $ foldr' f i xs

-- implementation of length
length :: (Integral n, Foldable t) => t a -> n
length = foldr ((+).(const 1)) 0
length' :: (Integral n, Functor t, Foldable t) => t a -> n
length' = sum . (fmap $ const 1)

-- implementation of map
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x):(map f xs)

-- implementation of maybe
maybe :: a -> (b -> a) -> (Maybe b) -> a
maybe d _ Nothing = d
maybe _ f (Just x) = f x

-- implementation of reverse
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = (++) (reverse xs) [x]

-- implementation of sum
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

-- implementation of zip
zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b):(zip as bs)
zip [] _ = []
zip _ [] = []

