-- implementation of (++)
cat (x:xs) ys = x:(cat xs ys)
cat [] ys = ys

main = do
	print $ cat "hello" " world"
