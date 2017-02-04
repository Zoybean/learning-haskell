module Math where

hcf :: Int -> Int -> Int
hcf 0 y = y
hcf x y
    | x < 0 = hcf (negate x) y
    | x <= y = hcf x (y - x)
    | otherwise = hcf y x

lb :: Int -> Maybe Int
lb 1 = Just 0
lb x
    | x > 1 = fmap (+1) $ lb (div x 2)
    | otherwise = Nothing

lbRnd :: Int -> Maybe Int
lbRnd 1 = Just 0
lbRnd x = let
            m :: Floating b => Int -> b
            m x = 2 ** ((fromIntegral x) + 1/2)
            mid :: Floating b => Int -> Maybe b
            mid x = fmap m (lb x)
          in
            if fmap ((fromIntegral x)<) (mid x) == Just True then lb x else lbMax x

lbMax :: Int -> Maybe Int
lbMax 1 = Just 0
lbMax x
    | mod x 2 == 0 = fmap (+1) $ lbMax (div x 2)
    | otherwise = fmap (+1) $ lb (x - 1)
