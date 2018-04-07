module Except
    (
    ) where

import           Control.Applicative (Alternative (empty, (<|>)))
import           Control.Monad       (MonadPlus (mplus, mzero), ap, guard,
                                      liftM, msum)

newtype Except e a = Except {runExcept :: Either e a} deriving Show

except :: Either e a -> Except e a
except = Except

instance Functor (Except e) where
  fmap = liftM

instance Applicative (Except e) where
  pure = return
  (<*>) = ap

-- | task 2.3.1.6
-- Реализуйте функцию withExcept :: (e -> e') -> Except e a -> Except e' a, позволящую, если произошла ошибка, применить к ней заданное преобразование.
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f res = case runExcept res of
  Left e  -> Except . Left . f $ e
  Right v -> Except . Right $ v
