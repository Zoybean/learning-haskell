module Staq where
import List (intercalate)
import Control.Applicative (liftA2)
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

main :: IO ()
main = do
    let q = ([6,5,4],[1,2,3]) :: Staq Int
    print $ q
    print $ flush q
    let nth q n = iterate (>>= rest) (Just q) !! n
    let showStaq q = liftA2 (\d r -> intercalate ":" [show d, show r]) (deq q) (rest q)
    mapM_ print $ map (\n -> (nth q n) >>= showStaq) [0,1,2,3,4,5]
    
