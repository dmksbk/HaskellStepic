-- Сделайте типы данных Arr2 e1 e2 и Arr3 e1 e2 e3 представителями класса типов Functor:

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

-- Эти типы инкапсулируют вычисление с двумя и тремя независимыми окружениями соответственно:
--
-- GHCi> getArr2 (fmap length (Arr2 take)) 10 "abc"
-- 3
-- GHCi> getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]
-- [33,44]

instance Functor (Arr2 e1 e2) where
  fmap f = Arr2 . (fmap . fmap $ f) . getArr2

instance Functor (Arr3 e1 e2 e3) where
  fmap f = Arr3 . (fmap . fmap . fmap $ f) . getArr3

main = do
  print $ getArr2 (fmap length (Arr2 take)) 10 "abc"
  print $ getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]
