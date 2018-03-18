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
  fmap f (Un x)        = Un (f x)
  fmap f (Bi x y rest) = Bi (f x) (f y) (fmap f rest)

instance Foldable OddC where
  foldMap f (Un x)        = f x
  foldMap f (Bi x y rest) = f x <> f y <> foldMap f rest

toList  :: OddC a -> [a]
toList = foldr (:) []

instance Traversable OddC where
  traverse f (Un x)        = Un <$> f x
  traverse f (Bi x y rest) = Bi <$> f x <*> f y <*> traverse f rest

-- GHCi> (+1) <$> cnt5                  == Bi 4 5 (Bi 2 3 (Un 43))
-- GHCi> toList cnt5                    == [3,4,1,2,42]
-- GHCi> sum cnt5                       == 52
-- GHCi> traverse (\x->[x+2,x-2]) cnt1  == [Un 44,Un 40]

main :: IO ()
main = do
  print $ (+1) <$> cnt5
  print $ toList cnt5
  print $ sum cnt5
  print $ traverse (\x->[x+2,x-2]) cnt1
