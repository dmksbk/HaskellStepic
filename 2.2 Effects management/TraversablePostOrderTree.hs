module TraversablePostOrderTree where

import           Control.Applicative (liftA3)
import           Data.Traversable    (fmapDefault, foldMapDefault)

-- Сделайте двоичное дерево
data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Functor Tree where
  fmap = fmapDefault

flip132 :: (a -> b -> c -> d) -> a -> c -> b -> d
flip132 f a b c = f a c b

instance Traversable Tree where
  -- sequenceA (Tr a b c) = Tr <$> a <*> b <*> c}
  sequenceA Nil            = pure Nil
  sequenceA (Branch l c r) = Branch <*> liftA3 flip132 <*> l <*> r <*> c

-- представителем класса типов Traversable таким образом, чтобы обеспечить для foldMapDefault порядок обхода «postorder traversal»:
testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- GHCi> foldMapDefault (\x -> [x]) testTree =?= [1,3,2,5,4]

main :: IO ()
main = do
  print $ foldMapDefault (\x -> [x]) testTree
