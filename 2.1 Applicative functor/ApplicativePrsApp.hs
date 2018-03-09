{-# LANGUAGE MonadComprehensions #-}
import           Data.Char (digitToInt)
import           MyTest    (myTest)

-- Сделайте парсер
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
-- из предыдущей задачи аппликативным функтором с естественной для парсера семантикой:

-- GHCi> runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE" == Just (('A','B','C'),"DE")
-- GHCi> runPrs (anyChr *> anyChr) "ABCDE" == Just ('B',"CDE")

-- Представитель для класса типов Functor уже реализован. From ApplicativePrs.hs
instance Functor Prs where
  fmap f p = Prs fun where
    fun s = case runPrs p s of
      Nothing      -> Nothing
      Just (x, xs) -> Just (f x, xs)

anyChr :: Prs Char
anyChr = Prs f where
  f []     = Nothing
  f (x:xs) = Just (x, xs)

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

main = do
  myTest (runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE") (Just (('A','B','C'),"DE"))
  myTest (runPrs (anyChr *> anyChr) "ABCDE") (Just ('B',"CDE"))
