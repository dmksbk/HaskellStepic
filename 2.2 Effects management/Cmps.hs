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

-- main = sequence_ tests1
main :: IO ()
main = putStrLn . maximum . Cmps $ [Nothing, Just "2", Just "3"]
