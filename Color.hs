module Color where
import List (intercalate)

data Format = Fore Shade Color | Back Shade Color | Effect Effect | None
data Color = Grey | Red | Green | Yellow | Blue | Magenta | Cyan | White
data Effect = Bold | Dim | Italic | Under | Blink | Flip | Hidden | Strike
data Shade = Light | Dark

formats :: [Format] -> String -> String
formats fs s = pre ++ s ++ post
    where
        pre = "\x1b[" ++ intercalate ";" (map (show . go) fs) ++ "m"
        post = "\x1b[0m"
        go :: Format -> Int
        go (None) = 0 -- erases all previous properties
        go (Effect e) = case e of
            Bold    -> 1 -- also sets 'Shade' property of foreground color to 'Light'.
            Dim    -> 2
            Italic  -> 3
            Under   -> 4
            -- no 5 afaik
            Blink   -> 6
            Flip    -> 7 -- swaps background and foreground colours. does not stack
            Hidden  -> 8
            Strike  -> 9
        go (Fore Dark c) = case c of
            Grey    -> 30
            Red     -> 31
            Green   -> 32
            Yellow  -> 33
            Blue    -> 34
            Magenta -> 35
            Cyan    -> 36
            White   -> 37
        go (Fore Light c) = 60 + go (Fore Dark c)
        go (Back s c) = 10 + go (Fore s c)
format :: Format -> String -> String
format x = formats [x]

main :: IO ()
main = do
    let
        pattern = [ [Back Light Cyan, Fore Dark Blue]
              , [Back Light Magenta, Fore Dark Red]
              , [Effect Flip]
              , [Back Light Magenta, Fore Dark Red]
              , [Back Light Cyan, Fore Dark Blue]
              ]
    putStrLn $ intercalate "\n" $ zipWith formats
        pattern
        [ "don't"
        , " be  "
        , "shit!"
        ]
    putStrLn ""
    putStrLn $ intercalate "\n" $ map
        ((>>= id) . zipWith formats pattern . map (:[])) -- temp operate on the characters as strings. there must be a better way
            [ "don't"
            , " be  "
            , "shit!"
            ]
