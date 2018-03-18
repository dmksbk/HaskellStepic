module Triple where

-- import           MyTest

-- Сделайте тип
data Triple a = Tr a a a  deriving (Eq,Show)

-- представителем класса типов Foldable:
-- tests1 = [
--   lbl "foldr and foldl chack",
--   foldr (++) "!!" (Tr "ab" "cd" "efg") =?= "abcdefg!!",
--   foldl (++) "!!" (Tr "ab" "cd" "efg") =?= "!!abcdefg"]

instance Functor Triple where
    fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
    pure a = Tr a a a
    (Tr fa fb fc) <*> (Tr a b c) = Tr (fa a) (fb b) (fc c)

instance Foldable Triple where
  foldr f ini (Tr x y z) = x `f` (y `f` (z `f` ini))

instance Traversable Triple where
  traverse f (Tr x y z) = pure Tr <*> f x <*> f y <*> f z
  sequenceA (Tr a b c) = Tr <$> a <*> b <*> c

-- Сделайте тип  представителем класса типов Traversable:

  -- GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
  -- "!!abcdefg"
  -- GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
  -- Right (Tr 12 14 16)
  -- GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
  -- Left 8
  -- GHCi> sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
  -- Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)

-- main = sequence_ $
--   tests1
