module DivByError
    (
    ) where

import           Except

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

tst1 = runExcept $ example0 5 2
tst2 = runExcept $ example0 5 0
