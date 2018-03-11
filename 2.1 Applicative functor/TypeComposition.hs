{-# LANGUAGE TypeOperators #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)}
  deriving (Eq, Show)

-- Населите допустимыми нерасходящимися выражениями следующие типы
type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (10, ('a', True))

b :: B t
b = Cmps (True, id, Left "Hallo")

c :: C
c  = Cmps $ \ b n -> if b then n else n + 1
