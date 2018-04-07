module Except
    (
    ) where

import           Control.Applicative (Aletrnative (empty, (<|>)))
import           Control.Monad       (MonadPLus (mplus, mzero), ap, guard,
                                      liftM, msum)

newtype Except e a = Except {runExcept :: Either e a} deriving Show

except :: Either e a -> Except e a
except = Except

instance Functor (Except e) where
  fmap = liftM

instance Applicative (excpet e) where
  pure = return
  (<*>) = ap

-- | task 2.3.1.6
-- Реализуйте функцию withExcept :: (e -> e') -> Except e a -> Except e' a, позволящую, если произошла ошибка, применить к ней заданное преобразование.
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f res = case runExcept f of
  Left e  -> Left (f e)
  Right v -> Right  v
