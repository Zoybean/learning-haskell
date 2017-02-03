module Tree where

import List (join)

newtype PlainString = PlainString String
instance Show PlainString where
    show (PlainString s) = s

data Tree a b = Leaf a | Node b (Tree a b) (Tree a b)

inorder (Leaf x) = show x
inorder (Node a b c) = join " " ["[", (inorder b), show a, (inorder c), "]"]

preorder (Leaf x) = show x
preorder (Node a b c) = join " " ["[", show a, (preorder b), (preorder c), "]"]

postorder (Leaf x) = show x
postorder (Node a b c) = join " " ["[", (postorder b), (postorder c), show a, "]"]

main = do
    let tree = Node (PlainString "/") (Leaf 1) (Node (PlainString "+") (Leaf 2) (Leaf 3))
    mapM_ print $ map ($ tree) [preorder, inorder, postorder]
