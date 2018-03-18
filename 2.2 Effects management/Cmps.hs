{-# LANGUAGE TypeOperators #-}

module FoldableCmps where

-- import           Data.Monoid
-- import           MyTest

-- Сделайте тип (|.|) представителем класса типов Foldable при условии, что аргументы композиции являются представителями Foldable.

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap u = Cmps . fmap (fmap u) . getCmps

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  Cmps u <*> Cmps x = Cmps $ fmap (<*>) u <*> x

instance (Foldable f, Foldable g, Functor f, Functor g) => Foldable (f |.| g) where
  -- foldMap u x = foldr mappend mempty (fmap (foldMap u) (getCmps x))
  foldMap u = foldMap (foldMap u) . getCmps
-- tests1 =
--   [ 3 =?= maximum $ Cmps [Nothing, Just 2, Just 3]
--   , 7 =?= length $ Cmps [[1,2], [], [3,4,5,6,7]]
--   ]

-- Сделайте тип представителем класса типов Traversable при условии, что аргументы композиции являются представителями Traversable.
instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  traverse u (Cmps x) = Cmps <$> traverse (traverse u) x

-- GHCi> sequenceA (Cmps [Just (Right 2), Nothing])
-- Right (Cmps {getCmps = [Just 2,Nothing]})
-- GHCi> sequenceA (Cmps [Just (Left 2), Nothing])
-- Left 2

-- main = sequence_ tests1
main :: IO ()
main = putStrLn . maximum . Cmps $ [Nothing, Just "2", Just "3"]
