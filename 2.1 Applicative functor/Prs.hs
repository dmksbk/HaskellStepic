{-# LANGUAGE MonadComprehensions #-}
import           Control.Applicative
import           Data.Char           (digitToInt, isDigit)
import           Data.Maybe          (isJust)
import           MyTest

-- Предположим, тип парсера определен следующим образом:
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

-- Сделайте этот парсер представителем класса типов Functor.
tests1 =
  [ lbl "instance Functor Prs"
  , runPrs anyChr "ABC"                  =?= Just ('A',"BC")
  , runPrs anyChr ""                     =?= Nothing
  , runPrs (digitToInt <$> anyChr) "BCD" =?= Just (11,"CD")
  ]

instance Functor Prs where
  fmap f p = Prs fun where
    fun s = case runPrs p s of
      Nothing      -> Nothing
      Just (x, xs) -> Just (f x, xs)

-- Реализуйте также парсер anyChr :: Prs Char, удачно разбирающий и возвращающий любой первый символ любой непустой входной строки.
anyChr :: Prs Char
anyChr = Prs f where
  f []     = Nothing
  f (x:xs) = Just (x, xs)

------------------------------------------
-- Сделайте парсер из предыдущей задачи аппликативным функтором с естественной для парсера семантикой:
tests2 =
  [ lbl "instance Applicative Prs"
  , runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE" =?= Just (('A','B','C'),"DE")
  , runPrs (anyChr *> anyChr) "ABCDE"                      =?= Just ('B',"CDE")
  ]

-- Представитель для класса типов Functor уже реализован.
instance Applicative Prs where
  pure v  = Prs fun where
    fun xs = Just (v, xs)

  -- Naive solution
  -- fp <*> vp = Prs fun where
  --     fun s = case runPrs fp s of
  --       Nothing -> Nothing
  --       Just (f, s') -> case runPrs vp s' of
  --         Nothing -> Nothing
  --         Just (v, s'') -> Just (f v, s'')

  -- Monad do notation
  -- fp <*> vp = Prs fun where
  --   fun s = do
  --           (f, s')    <- runPrs fp s
  --           (v, s'')   <- runPrs vp s'
  --           return (f v, s'')

  -- List comprehension for Maybe monad
  fp <*> vp = Prs fun where
    fun s = [ (f v, s'')
            | (f, s')    <- runPrs fp s
            , (v, s'')   <- runPrs vp s'
            ]
------------------------------------------
-- Сделайте парсер представителем класса типов Alternative с естественной для парсера семантикой:
tests3 =
  [ lbl "instance Alternative Prs"
  , runPrs (char 'A' <|> char 'B') "ABC" =?= Just ('A',"BC")
  , runPrs (char 'A' <|> char 'B') "BCD" =?= Just ('B',"CD")
  , runPrs (char 'A' <|> char 'B') "CDE" =?= Nothing
  ]

-- Представители для классов типов Functor и Applicative уже реализованы. Функцию char :: Char -> Prs Char включать в решение не нужно, но полезно реализовать для локального тестирования.

char  :: Char -> Prs Char
char c = Prs fun where
  fun [] = Nothing
  fun (x:xs) | x == c    = Just (x, xs)
             | otherwise = Nothing

instance Alternative Prs where
  empty = Prs $ \ _ -> Nothing
  x <|> y = Prs fun where
    fun s = let xs = runPrs x s in if isJust xs then xs else runPrs y s

------------------------------------------
-- Реализуйте парсер-комбинатор many1 :: Prs a -> Prs [a], который отличается от many только тем, что он терпит неудачу в случае, когда парсер-аргумент неудачен на начале входной строки.

tests4 =
  [ lbl "many1 for Prs"
  , runPrs (many1 $ char 'A') "AAABCDE" =?= Just ("AAA","BCDE")
  , runPrs (many1 $ char 'A') "BCDE"    =?= Nothing
  ]

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> (many1 p <|> pure [])

------------------------------------------

-- Реализуйте парсер nat :: Prs Int для натуральных чисел, так чтобы парсер
mult  :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

satisfy :: (Char -> Bool) -> Prs Char
satisfy p = Prs fun where
  fun []                  = Nothing
  fun (x:xs) | p x        = Just (x, xs)
             | otherwise  = Nothing

digit :: Prs Int
digit = digitToInt <$> satisfy isDigit

digits :: Prs [Int]
digits = many1 digit

-- nat     :: Prs Int
-- nat = f <$> digit <*> (nat <|> pure 0) where
--   f x y = 10 * x + y

nat     :: Prs Int
nat = foldl (\ x y -> 10*x+y) 0 <$> digits

-- обладал таким поведением
tests5 =
  [ lbl "nat for Prs"
  , runPrs nat "0C"       =?= Just (0, "C")
  , runPrs nat "7b"       =?= Just (7, "b")
  , runPrs nat "28K"      =?= Just (28, "K")
  , runPrs nat "123"      =?= Just (123, "")
  , runPrs nat "45AB"     =?= Just (45, "AB")
  , lbl "mul for Prs"
  , runPrs mult "14*3"    =?= Just (42,"")
  , runPrs mult "64*32"   =?= Just (2048,"")
  , runPrs mult "77*0"    =?= Just (0,"")
  , runPrs mult "2*77AAA" =?= Just (154,"AAA")
  ]

------------------------------------------
main = sequence_ $
  tests1 ++ tests2 ++ tests3 ++ tests4 ++ tests5
