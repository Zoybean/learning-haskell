module Knot where
data DList a = DEmpty | DNode (DList a) a (DList a) -- Doubly-linked list structure
instance Show a => Show (DList a) where
    show xs = "[" ++ go xs ++ "]"
        where
            go DEmpty = ""
            go (DNode DEmpty x DEmpty) = show x
            go (DNode _ x DEmpty) = ".." ++ show x
            go (DNode DEmpty x xs) = show x ++ "," ++ goOn xs
            go (DNode _ x xs) = ".." ++ show x ++ "," ++ goOn xs
            goOn (DNode _ x DEmpty) = show x
            goOn (DNode _ x xs) = show x ++ "," ++ goOn xs

knotList [] = []
knotList xs = first
    where
        first = go xs first
        go [x] first = x : first
        go (x:xs) first = x : (go xs first)

knot :: [a] -> DList a
knot xs = first
    where
        (first,last) = loop last xs first
        loop :: (DList a) -> [a] -> (DList a) -> (DList a, DList a)
        loop prev [x] first = (curr, last) -- here, only last is actually used, unless the full list is singular
            where
                next = first
                curr = DNode prev x next
                last = curr
        loop prev (x:xs) first = (curr,last) -- curr value is only used at the top level. maybe abstract it away?
            where
                curr = DNode prev x next
                (next,last) = loop curr xs first

link xs = go DEmpty xs
    where
        go :: (DList a) -> [a] -> DList a
        go prev [x] = curr
            where
                next = DEmpty
                curr = DNode prev x next
        go prev (x:xs) = curr
            where
                next = go curr xs
                curr = DNode prev x next

takeF :: Integer -> DList a -> DList a -- take n elements forward from the first element, inclusive
takeF 0 _ = DEmpty
takeF n (DNode prev x next)
    | n > 0 = DNode prev x (takeF (n - 1) next)

takeB :: Integer -> DList a -> DList a -- take n elements back from the first element, inclusive
takeB 0 _ = DEmpty
takeB n xs = takeF n $ iterate (1 - n) xs

iterate :: Integer -> DList a -> DList a
iterate n xs@(DNode prev _ next)
    | n < 0  = iterate (n + 1) prev
    | n == 0 = xs
    | n > 0  = iterate (n - 1) next

main :: IO ()
main = do
    let ls = knot [0..4]
    print $ takeF 8 ls
    print $ takeB 8 ls
    print $ take 8 $ knotList [0..4]
