module Change where

coins	:: Num a => [a]
coins = [2,3,7]

change :: (Ord a, Num a) => a -> [[a]]
change x = change' [] x where
	change' acc n 
		| n > 0 	= concatMap (\c -> change' (c : acc) (n - c)) coins
		| n < 0 	= []
		| otherwise = [acc]
	

main = do
	print $ change 7