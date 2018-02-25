-- В модуле Data.List имеется семейство функций zipWith, zipWith3, zipWith4,..:
--
-- GHCi> let x1s = [1,2,3]
-- GHCi> let x2s = [4,5,6]
-- GHCi> let x3s = [7,8,9]
-- GHCi> let x4s = [10,11,12]
-- GHCi> zipWith (\a b -> 2*a+3*b) x1s x2s
-- [14,19,24]
-- GHCi> zipWith3 (\a b c -> 2*a+3*b+5*c) x1s x2s x3s
-- [49,59,69]
-- GHCi> zipWith4 (\a b c d -> 2*a+3*b+5*c-4*d) x1s x2s x3s x4s
-- [9,15,21]
-- Аппликативные функторы могут заменить всё это семейство
--
-- GHCi> getZipList $ (\a b -> 2*a+3*b) <$> ZipList x1s <*> ZipList x2s
-- [14,19,24]
-- GHCi> getZipList $ (\a b c -> 2*a+3*b+5*c) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s
-- [49,59,69]
-- GHCi> getZipList $ (\a b c d -> 2*a+3*b+5*c-4*d) <$> ZipList x1s <*> ZipList x2s <*>ZipList x3s <*> ZipList x4s
-- [9,15,21]
-- Реализуйте операторы (>*<) и (>$<), позволяющие спрятать упаковку ZipList и распаковку getZipList:
--
-- GHCi> (\a b -> 2*a+3*b) >$< x1s >*< x2s
-- [14,19,24]
-- GHCi> (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s
-- [49,59,69]
-- GHCi> (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s
-- [9,15,21]
import           Data.List
import           Test.QuickCheck

import           Control.Applicative (ZipList (ZipList), getZipList)

(>$<)    :: (a -> b) -> [a] -> [b]
(>$<) f = getZipList . fmap f . ZipList

(>*<)    :: [a -> b] -> [a] -> [b]
(>*<) fs xs = getZipList $ ZipList fs <*> ZipList xs

--- =========== Tests ============
prop_add2     :: [Int] -> Property
prop_add2 xs = label "Adding 2" $
  (+2) >$< xs == fmap (+2) xs

prop_zip2     :: [Int] -> [Int] -> Property
prop_zip2 xs ys = label "zip2 with 2 a + 3 b" $
  f2 >$< xs >*< ys == zipWith f2 xs ys where
    f2 = \ a b -> 2 * a + 3 * b

prop_zip3     :: [Int] -> [Int] -> [Int] -> Property
prop_zip3 xs ys zs = label "zip3 with 2 a + 3 b + 5 c" $
  f3 >$< xs >*< ys >*< zs == zipWith3 f3 xs ys zs where
    f3 = \ a b c -> 2 * a + 3 * b + 5 * c

prop_zip4     :: [Int] -> [Int] -> [Int] -> [Int] -> Property
prop_zip4 xs ys zs ws = label "zip4 with 2 a + 3 b + 5 c - 4 d" $
  f4 >$< xs >*< ys >*< zs >*< ws == zipWith4 f4 xs ys zs ws where
    f4 = \ a b c d -> 2 * a + 3 * b + 5 * c - 4 * d

-- main =
--   sequence_ $ map quickCheck [prop_add2, prop_zip2, prop_zip3, prop_zip4]

main =
  quickCheck prop_add2 >>
  quickCheck prop_zip2 >>
  quickCheck prop_zip3 >>
  quickCheck prop_zip4
