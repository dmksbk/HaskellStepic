qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort l ++ [x] ++ qsort m where
	l = filter (<=x) xs
	m = filter (>x) xs