module List where
import Control.Applicative
import Prelude hiding ((++), reverse, map, zip, any, all, filter, length, maybe)
-- queue (kinda) efficiently implemented as 2 stacks
type Staq a = ([a],[a])
newStaq :: [a] -> Staq a
newStaq xs = (xs,[])
deq :: Eq a => Staq a -> Maybe a
deq (e,d)
    | d == []
    , e == [] = Nothing
    | d == []
    , e /= [] = Just $ head $ reverse e
    | otherwise = Just $ head d
rest :: Eq a => Staq a -> Maybe (Staq a)
rest (e,d)
    | d == []
    , e == [] = Nothing
    | d == []
    , e /= [] = Just $ ([], tail $ reverse e)
    | otherwise = Just $ (e, tail d)

enq :: Staq a -> a -> Staq a
enq (e,d) i = (i:e,d)

flush :: Staq a -> [a]
flush ([],[]) = []
flush (e,[]) = flush ([], reverse e)
flush (e,(d:ds)) = d:flush (e,ds)

-- implementation of (++)
(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x:(xs ++ ys)
[] ++ ys = ys

-- implementation of python's join
join :: String -> [String] -> String
join s [] = ""
join s (x:[]) = x
join s (x:xs) = x ++ s ++ (join s xs)

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
range a b
    | a < b = (a : (range (a+1) b ))
    | a == b = [a]
    | otherwise = []

range' :: Int -> Int -> Maybe [Int]
range' a b
    | a < b = fmap (a:) (range' (a+1) b)
    | a == b = Just [a]
    | otherwise = Nothing

-- implementation of zip
zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b):(zip as bs)
zip [] _ = []
zip _ [] = []

-- implementations of fold
foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' _ i [] = i
foldr' f i (x:xs) = f x $ foldr' f i xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ i [] = i
foldl' f i (x:xs) = foldl' f (f i x) xs

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
floop xs = let
                f :: (Foldable t, Alternative t) => Maybe a -> Maybe (t a) -> Maybe (t a)
                f Nothing _ = Nothing
                f (Just x) ys = fmap ((<|>) $ pure x) ys
                --f x ys = maybe Nothing (flip (fmap . (<|>) . pure) ys) x -- is equivalent
            in
                foldr f (Just empty) xs

-- simpler, more limited example
floop' :: [Maybe a] -> Maybe [a]
floop' [] = Just []
floop' (Nothing:_) = Nothing
floop' (Just x:xs) = fmap (x:) $ floop xs

--floop'' :: (Foldable t, Alternative t) => t (Maybe a) -> Maybe (t a)
--floop'' xs = foldr (\x ys -> maybe Nothing (flip (fmap . (<|>) . pure) ys) x) (Just empty) xs --needlessly equivalent

-- I still don't understand this one but it is better -- Source: http://stackoverflow.com/a/41986867/6112457
sequence' :: (Alternative t, Foldable t, Applicative a) => t (a b) -> a (t b)
sequence' = foldr inject (pure empty)
  where inject = liftA2 prepend
        prepend = (<|>) . pure

--floop' xs = foldr (fmap . (:)) (Just []) xs
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
                ,range' 1 5 == Just [1,2,3,4,5]
                ,range 1 5 == [1,2,3,4,5]
                ,range' 5 1 == Nothing
                ,range 5 1 == []
                ,zip [1,2,3] [4,5,6,7] == [(1,4),(2,5),(3,6)]
                ,zip [1,2,3,7] [4,5,6] == [(1,4),(2,5),(3,6)]
                ,mayadd (Just 1) (Just 2) == Just 3
                ,mayadd Nothing (Just 2) == Nothing
                ,mayadd (Just 1) Nothing == Nothing
                ,foldl (-) 0 [1,2,3,4] == -10 -- (((1 - 2) - 3) - 4) - 0
                ,foldl' (-) 0 [1,2,3,4] == -10
                ,foldr (-) 0 [1,2,3,4] == -2 -- 1 - (2 - (3 - (4 - 0)))
                ,foldr' (-) 0 [1,2,3,4] == -2
                ]
    
    let q = ([6,5,4],[1,2,3]) :: Staq Int
    print $ q
    print $ flush q
    let nth q n = iterate (>>= rest) (Just q) !! n
    let showStaq q = liftA2 (\d r -> join ":" [show d, show r]) (deq q) (rest q)
    mapM_ print $ map (\n -> (nth q n) >>= showStaq) [0,1,2,3,4,5]
    
    mapM_ print [floop (map Just [1,2,3,4,5]) == Just [1,2,3,4,5]
                ,floop (Nothing : ( map Just [1,2,3,4,5])) == Nothing
                ,floop' (map Just [1,2,3,4,5]) == Just [1,2,3,4,5]
                ,floop' (Nothing : ( map Just [1,2,3,4,5])) == Nothing
                ,sequence' (map Just [1,2,3,4,5]) == Just [1,2,3,4,5]
                ,sequence' (Nothing : ( map Just [1,2,3,4,5])) == Nothing
                ]
