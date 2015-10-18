module Main where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 xs ys zs = (hx + hy + hz) : sum3 tx ty tz where
	(hx,tx) = ht xs
	(hy,ty) = ht ys
	(hz,tz) = ht zs
	ht []     = (fromInteger 0, [])
	ht (x:xs) = (x,xs)

main = print $ sum3 [1,2,3] [4,5] [6]