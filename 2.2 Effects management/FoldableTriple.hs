import            MyTest

-- Сделайте тип
data Triple a = Tr a a a  deriving (Eq,Show)

-- представителем класса типов Foldable:
tests1 = [
  lbl "foldr and foldl chack",
  foldr (++) "!!" (Tr "ab" "cd" "efg") =?= "abcdefg!!",
  foldl (++) "!!" (Tr "ab" "cd" "efg") =?= "!!abcdefg"]

instance Foldable Triple where
  foldr f ini (Tr x y z) = x `f` (y `f` (z `f` ini))

main = sequence_ $
  tests1
