module DivByError
    (
    ) where

import           Control.Applicative
import           Control.Monad       (guard, msum)
import           Except
import           MyTest

data DivByError = ErrZero String | ErrOther deriving (Eq, Show)

(/?)  :: Double -> Double -> Except DivByError Double
x /? 0 = throwE $ ErrZero (show x ++ "/0;")
x /? y = return $ x / y

example0 :: Double -> Double -> Except DivByError String
example0 x y = action `catchE` handler where
  action = do
    q <- x /? y
    return $ show q
  handler = return . show

tests0 =
  [ lbl "Example 0"
  , runExcept (example0 5 2) =?= Right "2.5"
  , runExcept (example0 5 0) =?= Right "ErrZero \"5.0/0;\""
  ]

instance Monoid DivByError where
  mempty = ErrOther
  ErrZero s1 `mappend` ErrZero s2 = ErrZero $ s1 ++ s2
  ErrZero s1 `mappend` ErrOther   = ErrZero s1
  ErrOther   `mappend` ErrZero s2 = ErrZero s2
  ErrOther   `mappend` ErrOther   = ErrOther

example2  :: Double -> Double -> Except DivByError String
example2 x y = action `catchE` handler where
  action = do
    q <- x /? y
    guard $ y >= 0
    return $ show q

  handler (ErrZero s) = return s
  handler ErrOther    = return "NONNEGATIVE GUARD"

tests2=
  [ lbl "Example 2"
  , runExcept (example2 5 2)    =?= Right "2.5"
  , runExcept (example2 5 0)    =?= Right "5.0/0;"
  , runExcept (example2 5 (-3)) =?= Right "NONNEGATIVE GUARD"
  ]

tests3 =
  [ lbl "MonadPlus and msum"
  , runExcept (msum [5 /? 0, 7 /? 2, 2 /? 0]) =?= Right 3.5
  , runExcept (msum [5 /? 0, 7 /? 0, 2 /? 0]) =?=
      Left (ErrZero "5.0/0;7.0/0;2.0/0;")
  ]

test = sequence_ $ tests0 ++ tests2 ++ tests3
