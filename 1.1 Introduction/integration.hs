module Main where

import Debug.Trace

{-
integration :: (Double -> Double) -> Double -> Double -> Double
integration f b a = trap 0 (beg, beg + h) where
	trap acc (x, y) = if y > end + h / 2 then acc else
		trap (acc + h * (f x + f y) / 2) (y, y + h)
	h = abs $ (b - a) / 1000
	beg = min a b
	end = max a b
-}

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * sum (a / 2 : f b  / 2 : [ f (a + i * h) | i <- [1..n - 1]]) where
	n = 1000
	h = (b - a) / n

main = print $ integration sin 0 pi