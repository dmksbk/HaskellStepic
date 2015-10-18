module Main where

fibonacci :: Integer -> Integer
fibonacci n = f (0, 1) n where
	f (a, b) n 
		| n == 0 = a
		| n == 1 = b
		| n >  1 = f (b, a + b) (n - 1)
		| n <  0 = f (b - a, a) (n + 1)