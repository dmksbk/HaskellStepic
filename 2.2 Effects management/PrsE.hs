import           Data.Char (digitToInt)
import           MyTest    (myTests, (=?=))

-- Рассмотрим более продвинутый парсер, позволяющий возвращать пользователю причину неудачи при синтаксическом разборе:
newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

-- Реализуйте функцию satisfyE :: (Char -> Bool) -> PrsE Char таким образом, чтобы функция
satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE fun where
  fun []                  = Left "unexpected end of input"
  fun (x:xs) | p x        = Right (x, xs)
             | otherwise  = Left $ "unexpected " ++ [x]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

-- обладала бы следующим поведением:
-- GHCi> runPrsE (charE 'A') "ABC" == Right ('A',"BC")
-- GHCi> runPrsE (charE 'A') "BCD" == Left "unexpected B"
-- GHCi> runPrsE (charE 'A') ""    == Left "unexpected end of input"

-- Сделайте парсер из предыдущей задачи функтором и аппликативным функтором:
anyE = satisfyE (const True)
-- GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE" == Right (('A','C'),"DE")
-- GHCi> runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE" == Left "unexpected B"
-- GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"    == Left "unexpected end of input"

instance Functor PrsE where
  fmap f p = PrsE fun where
    fun s = case runPrsE p s of
      Left  e       -> Left e
      Right (v, xs) -> Right (f v, xs)

instance Applicative PrsE where
  pure v = PrsE fun where
    fun s = Right (v, s)
  fp <*> vp = PrsE fun where
    fun s = case runPrsE fp s of
      Left e -> Left e
      Right (f, s') -> case runPrsE vp s' of
        Left e         -> Left e
        Right (v, s'') -> Right (f v, s'')

-- Сделайте парсер PrsE a из первого модуля курса представителем класса типов Monad:

--GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ABC" =?= Right (('A','B'),"C")
--GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ACD" =?= Left "unexpected C"
--GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "BCD" =?= Left "unexpected B"

instance Monad PrsE where
    m >>= k = PrsE $ \ s -> case runPrsE m s of
        Left  e         -> Left e
        Right (a, s')   -> runPrsE (k a) s'

prs1    :: PrsE (Char, Char)
prs1 = do
    a <- charE 'A'
    b <- charE 'B'
    return (a, b)

main = myTests
  [ (runPrsE (charE 'A') "ABC",                            Right ('A',"BC"))
  , (runPrsE (charE 'A') "BCD",                            Left "unexpected B")
  , (runPrsE (charE 'A') "",                               Left "unexpected end of input")
  ] >> myTests
  [ (runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE", Right (('A','C'),"DE"))
  , (runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE", Left "unexpected B")
  , (runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB",    Left "unexpected end of input")
  --] >> myTests
  --[ (runPrsE prs1 "ABC") =?= (Right (('A','B'),"C"))
  --, runPrsE prs1 "ACD" =?= Left "unexpected C"
  --, runPrsE prs1 "BCD" =?= Left "unexpected B"
  ]
