{-# LANGUAGE ScopedTypeVariables #-}

module OddC where

import           Data.Monoid

-- Рассмотрим следующий тип данных
data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
-- Этот тип представляет собой контейнер-последовательность, который по построению может содержать только нечетное число элементов:

cnt1 :: OddC Integer
cnt1 = Un 42

cnt3 :: OddC Integer
cnt3 = Bi 1 2 cnt1
cnt5 :: OddC Integer
cnt5 = Bi 3 4 cnt3
-- GHCi> cnt5 == Bi 3 4 (Bi 1 2 (Un 42))

cntInf  :: OddC Char
cntInf = Bi 'A' 'B' cntInf
-- GHCi> cntInf == Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'Interrupted.

-- Сделайте этот тип данных представителем классов типов Functor, Foldable и Traversable:

instance Functor OddC where
  --class Functor (f :: * -> *) where
  --  fmap :: (a -> b) -> f a -> f b
  fmap f (Un x)        = Un (f x)
  fmap f (Bi x y rest) = Bi (f x) (f y) (fmap f rest)

instance Foldable OddC where
  --class Foldable (t :: * -> *) where
  --  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (Un x)        = f x
  foldMap f (Bi x y rest) = f x <> f y <> foldMap f rest

toList  :: OddC a -> [a]
toList = foldr (:) []

instance Applicative OddC where
  -- class Functor f => Applicative (f :: * -> *) where
  --   pure :: a -> f a
  --   (<*>) :: f (a -> b) -> f a -> f b
  pure = Un
  fx <*> vs = undefined

instance Traversable OddC where
  --class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  --  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Un x)        = Un <$> f x
  traverse f (Bi x y rest) = Bi <$> f x <*> f y <*> traverse f rest
  --class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  --  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA (Un x)        = pure x
  sequenceA (Bi x y rest) = Bi <$> x <*> y <*> sequenceA rest

-- GHCi> (+1) <$> cnt5                  == Bi 4 5 (Bi 2 3 (Un 43))
-- GHCi> toList cnt5                    == [3,4,1,2,42]
-- GHCi> sum cnt5                       == 52
-- GHCi> traverse (\x->[x+2,x-2]) cnt1  == [Un 44,Un 40]

instance Monad OddC where
  -- class Applicative m => Monad (m :: * -> *) where
  --   (>>=) :: m a -> (a -> m b) -> m b
  --   (>>) :: m a -> m b -> m b
  --   return :: a -> m a
  return = pure
  mv >>= k = undefined

-------------------------------------------------
-- Для типа данных OddC a (контейнер-последовательность, который по построению может содержать только нечетное число элементов) реализуйте функцию, конкатенирующую три таких контейнера в один:

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC = undefined

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')
--GHCi> concat3OC tst1 tst2 tst3 =?= Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))

-- Обратите внимание, что соображения четности запрещают конкатенацию двух контейнеров OddC. Реализуйте всё «честно», не сводя к стандартным спискам.

main :: IO ()
main = do
  print $ (+1) <$> cnt5
  print $ toList cnt5
  print $ sum cnt5
  print $ traverse (\x->[x+2,x-2]) cnt1
