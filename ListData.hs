-- implementation of intercalate and intersperse
inter :: (a -> b -> b) -> b -> a -> [a] -> b
inter _ i _ [] = i
inter f i _ [x] = f x i -- coerce x into a member of type b
inter f i s (x:xs) = f x $ f s $ inter f i s xs

intercalate :: [a] -> [[a]] -> [a]
intercalate = inter (++) []

intersperse :: a -> [a] -> [a]
intersperse = inter (:) []

-- basic list intersection. ineffective on infinite lists
intersect :: Eq a => [a] -> [a] -> [a]
intersect = intersectBy (==)

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy _ _ [] = []
intersectBy _ [] _ = []
intersectBy eq (x:xs) ys
    | any (eq x) ys = x : intersectBy eq xs ys
    | otherwise     = intersectBy eq xs ys
