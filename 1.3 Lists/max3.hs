module Main where

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 m3 where m3 a b c = max a $ max b c 

main = print $ max3 [7,2,9] [3,6,8] [1,8,10]