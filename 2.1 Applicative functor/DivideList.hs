import           Test.QuickCheck

-- Функция

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)
-- сворачивает список посредством деления. Модифицируйте ее, реализовав divideList' :: (Show a, Fractional a) => [a] -> (String,a), такую что последовательность вычислений отражается в логе:
--
-- GHCi> divideList [3,4,5]   == 3.75
-- GHCi> divideList' [3,4,5]  == ("<-3.0/<-4.0/<-5.0/1.0",3.75)
-- Используйте аппликативный функтор пары, сохраняя близкую к исходной функции структуру реализации
--
divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> (s, x) <*> divideList' xs where
  s = "<-" ++ show x ++ "/"

-------- Tests --------
prop_divideList   :: (Fractional a, Eq a, Show a) => [a] -> Bool
prop_divideList xs = divideList xs == snd (divideList'  xs)

main = let l = [3..5] in do
  print $ divideList  l
  print $ divideList' l
  -- Do quck checks
  quickCheck (prop_divideList :: [Float] -> Bool)
