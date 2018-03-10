{-# LANGUAGE MonadComprehensions #-}
import           Control.Applicative
import           Data.Char           (digitToInt)
import           Data.Maybe          (isJust)
import           MyTest

-- Предположим, тип парсера определен следующим образом:
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

-- Сделайте этот парсер представителем класса типов Functor.
tests1 =
  [ runPrs anyChr "ABC"                  =?= Just ('A',"BC")
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
  [ runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE" =?= Just (('A','B','C'),"DE")
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
  [ runPrs (char 'A' <|> char 'B') "ABC" =?= Just ('A',"BC")
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

main = sequence_ $ tests1 ++ tests2 ++ tests3
