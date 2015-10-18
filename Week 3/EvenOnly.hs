module EvenOnly where

evenOnly :: [a] -> [a]
evenOnly = foldr (\(i,x) a -> if even i then x:a else a) [] . zip [1..]