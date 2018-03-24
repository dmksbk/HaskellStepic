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

fromList  :: [a] -> OddC a
fromList [] = error "fromList on empty list"
fromList [x] = Un x
fromList (x:y:rest) = Bi x y $ fromList rest

instance Applicative OddC where
  -- class Functor f => Applicative (f :: * -> *) where
  --   pure :: a -> f a
  --   (<*>) :: f (a -> b) -> f a -> f b
  pure = Un
  fs <*> vs = fromList $ toList fs <*> toList vs

instance Traversable OddC where
  --class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  --  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Un x)        = Un <$> f x
  traverse f (Bi x y rest) = Bi <$> f x <*> f y <*> traverse f rest
  --class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  --  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA (Un x)        = Un <$> x
  sequenceA (Bi x y rest) = Bi <$> x <*> y <*> sequenceA rest

-- GHCi> (+1) <$> cnt5                  == Bi 4 5 (Bi 2 3 (Un 43))
-- GHCi> toList cnt5                    == [3,4,1,2,42]
-- GHCi> sum cnt5                       == 52
-- GHCi> traverse (\x->[x+2,x-2]) cnt1  == [Un 44,Un 40]

-------------------------------------------------
-- Для типа данных OddC a (контейнер-последовательность, который по построению может содержать только нечетное число элементов) реализуйте функцию, конкатенирующую три таких контейнера в один:

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x1 x2 x3) y             z = Bi x1 x2 $ concat3OC x3 y z
concat3OC (Un x)        (Bi y1 y2 y3) z = Bi x  y1 $ concat3OC (Un y2) y3 z
concat3OC (Un x)        (Un y)        z = Bi x  y    z

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')
--GHCi> concat3OC tst1 tst2 tst3 =?= Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
-- Обратите внимание, что соображения четности запрещают конкатенацию двух контейнеров OddC. Реализуйте всё «честно», не сводя к стандартным спискам.

-------------------------------------------------
-- Для типа данных OddC a реализуйте функцию
concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x)         = x
concatOC (Bi x1 y1 z1)  = concat3OC x1 y1 $ concatOC z1

-- Она должна обеспечивать для типа OddC поведение, аналогичное поведению функции concat для списков:

-- GHCi> concatOC $ Un (Un 42) =?= Un 42
-- GHCi> concatOC $ Bi tst1 tst2 (Un tst3) =?= Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))

-------------------------------------------------
-- Сделайте тип данных OddC a представителем классов типов Functor, Applicative и Monad. Семантика должна быть подобной семантике представителей этих классов типов для списков: монада OddC должна иметь эффект вычисления с произвольным нечетным числом результатов:

tst4 = Bi 10 20 (Un 30)
tst5 = Bi 1 2 (Bi 3 4 (Un 5))
-- GHCi> do {x <- tst4; y <- tst5; return (x + y)} =?= Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))
-- GHCi> do {x <- tst5; y <- tst4; return (x + y)} =?= Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35)))))))

-- Реализуйте всё «честно», не сводя к стандартным спискам. Функцию fail можно не реализовывать, полагаясь на реализацию по умолчанию.

instance Monad OddC where
  -- class Applicative m => Monad (m :: * -> *) where
  --   (>>=) :: m a -> (a -> m b) -> m b
  --   (>>) :: m a -> m b -> m b
  --   return :: a -> m a
  return = pure
  (Un x)     >>= k  = k x
  (Bi x y z) >>= k  = concat3OC (k x) (k y) (z >>= k)

------------------------------------------------
main :: IO ()
main = do
  print $ "--- Applicative OddC ---"
  print $ (+1) <$> cnt5
  print $ "--- Foldable Oddc ---"
  print $ toList cnt5
  print $ sum cnt5
  print $ traverse (\x->[x+2,x-2]) cnt1
  print $ "--- concat3OC ---"
  print $ concat3OC tst1 tst2 tst3
  print $ "--- contactOC ---"
  print $ concatOC $ Un (Un 42)
  print $ concatOC $ Bi tst1 tst2 (Un tst3)
  print $ "--- Monad OddC"
  print $ do {x <- tst4; y <- tst5; return (x + y)}
  print $ do {x <- tst5; y <- tst4; return (x + y)}
