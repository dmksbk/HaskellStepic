import           Data.Char (digitToInt)
import           MyTest    (myTest)

-- Предположим, тип парсера определен следующим образом:

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
-- Сделайте этот парсер представителем класса типов Functor. Реализуйте также парсер anyChr :: Prs Char, удачно разбирающий и возвращающий любой первый символ любой непустой входной строки.

-- GHCi> runPrs anyChr "ABC"                   == Just ('A',"BC")
-- GHCi> runPrs anyChr ""                      == Nothing
-- GHCi> runPrs (digitToInt <$> anyChr) "BCD"  == Just (11,"CD")

instance Functor Prs where
  fmap f p = Prs fun where
    fun s = case runPrs p s of
      Nothing      -> Nothing
      Just (x, xs) -> Just (f x, xs)

anyChr :: Prs Char
anyChr = Prs f where
  f []     = Nothing
  f (x:xs) = Just (x, xs)

main = do
  myTest (runPrs anyChr "ABC") (Just ('A',"BC"))
  myTest (runPrs anyChr "") Nothing
  myTest (runPrs (digitToInt <$> anyChr) "BCD") (Just (11,"CD"))
