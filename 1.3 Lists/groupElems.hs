module Main where

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems l@(x:_) =  b : groupElems e where
	(b, e) = span (==x) l

{-
main = do
	print $ groupElems []
	print $ groupElems [1,2]
	print $ groupElems [1,2,2,4]
	print $ groupElems [1,2,3,2,4]
-}