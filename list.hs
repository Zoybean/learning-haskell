-- implementation of (++)
cat (x:xs) ys = x:(cat xs ys)
cat [] ys = ys

-- implementation of reverse
rev [] = []
rev (x:xs) = cat (rev xs) [x]

main = do
	print $ cat "hello" " world"
	print $ rev "nope"

