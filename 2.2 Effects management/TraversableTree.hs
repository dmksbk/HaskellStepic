module TrversableTree where

-- Сделайте двоичное дерево представителем класса типов Traversable (а также всех других необходимых классов типов).
data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

instance Functor Tree where
  fmap _ Nil            = Nil
  fmap f (Branch l c r) = Branch (fmap f l) (f c) (fmap f r)

instance Foldable Tree where
  foldr _ ini Nil            = ini
  foldr f ini (Branch l c r) = foldr f (c `f` foldr f ini r) l

-- instance Applicative Tree where
--   pure x = Branch Nil x Nil
--

instance Traversable Tree where
  traverse _ Nil            = pure Nil
  traverse f (Branch l c r) = Branch <$> traverse f l <*> f c <*> traverse f r

-- GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 3 Nil)
-- Right (Branch (Branch Nil 1 Nil) 3 Nil)
-- GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 2 Nil)
-- Left 2
-- GHCi> sequenceA $ Branch (Branch Nil [1,2] Nil) [3] Nil
-- [Branch (Branch Nil 1 Nil) 3 Nil,Branch (Branch Nil 2 Nil) 3 Nil]
