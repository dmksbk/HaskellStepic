module Except
    ( Except(..), except, withExcept, throwE, catchE
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

instance Monad (Except e) where
  return a = Except (Right a)
  m >>= k =
    case runExcept m of
      Left e  -> Except (Left e)
      Right x -> k x

-- | task 2.3.1.6
-- Реализуйте функцию withExcept :: (e -> e') -> Except e a -> Except e' a, позволящую, если произошла ошибка, применить к ней заданное преобразование.
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f res = case runExcept res of
  Left e  -> Except . Left . f $ e
  Right v -> Except . Right $ v
-- end task 2.3.1.6

throwE  :: e -> Except e a
throwE = except . Left

catchE  :: Except e a -> (e -> Except e' a) -> Except e' a
m `catchE` h =
  case runExcept m of
    Left e  -> h e
    Right r -> except (Right r)

-- Использование:
-- do {action1; action2; action3} `catchE` handler

-- Закон:
-- catchE h (throwE e) == h e
