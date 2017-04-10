module Step_5_7_7 where

import Control.Monad.Writer
import Data.Monoid

-- Измените определение типа Shopping и доработайте функцию purchase из предыдущего задания таким образом, чтобы можно было реализовать функцию items, возвращающую список купленных товаров (в том же порядке, в котором они были перечислены при покупке):

--newtype Log = Log (Sum Int, [String])
--instance Monoid (Sum Int, [String]) where
--    mempty = (0, [])
--    mappend (s1, l1) (s2, l2) = (s1 + s2, s1 ++ s2)

type Shopping = Writer (Sum Integer, [String]) ()

purchase    :: String -> Integer -> Writer (Sum Integer, [String]) ()
purchase t p = writer ((), (Sum p, [t]))

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

-- GHCi> total shopping1 
-- 19708
-- GHCi> items shopping1
-- ["Jeans","Water","Lettuce"]

-- Реализуйте функцию items и исправьте функцию total, чтобы она работала как и прежде.
total   :: Shopping -> Integer
total = getSum . fst . execWriter

items   :: Shopping -> [String]
items = snd . execWriter

tests = [ total shopping1 == 19708
        , items shopping1 == ["Jeans","Water","Lettuce"]
        ]