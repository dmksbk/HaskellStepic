module RevRange where

import Data.List (unfoldr)

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g where
	g (f, t) = if t >= f then Just (t, (f, pred t)) else Nothing

main = do
	print $ revRange ('a','z')
	print $ "zyxwvutsrqponmlkjihgfedcba" == revRange ('a','z')