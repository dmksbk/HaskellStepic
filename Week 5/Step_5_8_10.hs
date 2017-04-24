module Step_5_8_10 where

import Control.Monad
import Control.Monad.State

-- Некоторое время назад мы определили тип двоичных деревьев, содержащих значения в узлах:

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
    deriving (Eq, Show)

-- В этой задаче вам дано значение типа Tree (), иными словами, вам задана форма дерева. Требуется пронумеровать вершины дерева данной формы, обойдя их in-order (то есть, сначала обходим левое поддерево, затем текущую вершину, затем правое поддерево):

numberTree :: Tree a -> Tree Integer
numberTree tree = evalState (numStep tree) 1

numStep     :: Tree a -> State Integer (Tree Integer)
numStep (Leaf a) = do
    n <- get
    put $ n + 1
    return $ Leaf n

numStep (Fork l c r) = do
    l' <- numStep l
    n <- get
    put $ n + 1
    r' <- numStep r
    return $ Fork l' n r'


-- GHCi> numberTree (Leaf ())
-- Leaf 1
-- GHCi> numberTree (Fork (Leaf ()) () (Leaf ()))
-- Fork (Leaf 1) 2 (Leaf 3)

tests = 
    [ numberTree (Leaf ())                          == Leaf 1
    , numberTree (Fork (Leaf ()) () (Leaf ()))      == Fork (Leaf 1) 2 (Leaf 3)

    ]