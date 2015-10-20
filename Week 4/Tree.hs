module Tree where

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node n1 n2) = 1 + max (height n1) (height n2)

size :: Tree a -> Int
size (Leaf _) = 1
size ((Node n1 n2)) = 1 + size n1 + size n2

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf l) = (1, l)
    go (Node n1 n2) = let {(c1, s1) = go n1; (c2, s2) = go n2}
    				  in (c1+c2, s1+s2)