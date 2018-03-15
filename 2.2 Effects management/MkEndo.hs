import           Data.Monoid
import           MyTest

-- Реализуйте функцию, принимающую контейнер функций и последовательно сцепляющую элементы этого контейнера с помощью композиции, порождая в итоге эндоморфизм.
mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo 
-- mkEndo = Endo . foldr (.) id

e1 = mkEndo [(+5),(*3),(^2)]
e2 = mkEndo (42,(*3))
tests1 =
  [ appEndo e1 2 =?= 17
  , appEndo e2 2 =?= 6
  ]

main = sequence_ tests1
