module Perms where

perms 	:: [a] -> [[a]]
perms [] = [[]]
perms xs = concatMap (\(y,ys) -> map (y:) $ perms ys) $ pairs xs

pairs 	:: [a] -> [(a,[a])]
pairs xs = map (\n -> (xs!!n, delete n xs)) [0..length xs - 1]

delete 	:: Int -> [a] -> [a]
delete _ [] = []
delete 0 (x:xs) = xs
delete n (x:xs) = x : delete (n-1) xs

{- Solution that requires Eq a
perms xs = concatMap (\x -> map (x:) $ perms (filter (/=x) xs)) xs
-}

{- Solution with do
perms xs = do
	x <- xs
	let xs' = filter (/=x) xs
	ys <- perms xs'
	return $ x : ys
-}

main = do
	print $ mySol
	print $ sol == mySol where
		sol = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
		mySol = perms [1,2,3]