{-# LANGUAGE TypeOperators #-}

import           MyTest

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)}
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure

-- Напишите универсальные функции unCmps3 & unCmps4, позволяющие избавляться от синтаксического шума для композиции нескольких функторов:
-- tests1 =
--   [ (pure 42 :: ([] |.| [] |.| []) Int) =?= (Cmps (Cmps [[42]]))
--   , unCmps3 (pure 42 :: ([] |.| [] |.| []) Int) =?= [[[42]]]
--   , unCmps3 (pure 42 :: ([] |.| Maybe |.| []) Int) =?= [Just [42]]
--   , unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int) =?= [[[[42]]]]
--   ]

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
-- unCmps3 = getCmps . fmap getCmps . fmap getCmps
unCmps3 = fmap getCmps . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap (fmap getCmps) . fmap getCmps . getCmps

-- main = sequence_ $
--   tests1
