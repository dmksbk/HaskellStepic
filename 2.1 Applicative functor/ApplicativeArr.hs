-- Сделайте типы данных Arr2 e1 e2 и Arr3 e1 e2 e3 представителями класса типов Applicative

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }
-- с естественной семантикой двух и трех окружений:

-- GHCi> getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3
-- -1
-- GHCi> getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4
-- -15

instance Functor (Arr2 e1 e2) where
  fmap f = Arr2 . (fmap . fmap $ f) . getArr2

instance Functor (Arr3 e1 e2 e3) where
  fmap f = Arr3 . (fmap . fmap . fmap $ f) . getArr3

instance Applicative (Arr2 e1 e2) where
  pure = Arr2 . const . const
  Arr2 g <*> Arr2 h = Arr2 $ \ e1 e2 -> g e1 e2 (h e1 e2)

instance Applicative (Arr3 e1 e2 e3) where
  pure = Arr3 . const . const . const
  Arr3 g <*> Arr3 h = Arr3 $ \ e1 e2 e3 -> g e1 e2 e3 (h e1 e2 e3)

main = do
  print $ getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3
  print $ getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4
