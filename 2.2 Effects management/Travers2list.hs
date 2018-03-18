module Traverse2List where

-- Реализуйте функцию работающую с эффектами как traverse_, , но параллельно с накоплением эффектов «восстанавливающую» сворачиваемую структуру в виде списка:
-- traverse_ :: (Foldable t, Applicative f) => (a-> f b) -> t a -> f ()
-- foldMap f = foldr ((*>) . f) (pure())

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (u . f) (pure []) where
  u x y = pure (:) <*> x <*> y
-- traverse2list f = undefined

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

instance Foldable Tree where
  foldr _ ini Nil            = ini
  foldr f ini (Branch l c r) = foldr f (c `f` foldr f ini r) l

-- tests =
--   [ traverse2list (\x -> [x+10,x+20]) [1,2,3] =?=     [[11,12,13],[11,12,23],[11,22,13],[11,22,23],[21,12,13],[21,12,23],[21,22,13],[21,22,23]]
--   , traverse2list (\x -> [x+10,x+20]) $ Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil) =?= [[12,11,13],[12,11,23],[12,21,13],[12,21,23],[22,11,13],[22,11,23],[22,21,13],[22,21,23]]
--   ]

main :: IO ()
main = do
  print $ traverse2list (\x -> [x+10,x+20]) [1,2,3]
  print $ traverse2list (\x -> [x+10,x+20]) $ Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)
