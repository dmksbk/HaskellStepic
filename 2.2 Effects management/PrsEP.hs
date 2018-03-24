{-# LANGUAGE RankNTypes #-}

module PrsEP where

---- My Test -------------------
rt  :: String
rt = "\x1b[31m"
gt  :: String
gt = "\x1b[32m"
nt  :: String
nt = "\x1b[0m"
bt  :: String
bt = "\x1b[34m"

myTest  :: forall a. (Show a, Eq a) => a -> a -> IO ()
myTest x y =
  if x == y then
    putStrLn $ gt ++ "Correct: " ++ nt ++ show x
  else
    putStrLn $ rt ++ "  Wrong: " ++ nt ++ show x ++ " <> " ++ show y

myTests :: forall a. (Show a, Eq a) => [(a, a)] -> IO ()
myTests []     = return ()
myTests (t:ts) = myTest (fst t) (snd t) >> myTests ts

lbl :: String -> IO ()
lbl s = putStrLn $ bt ++ "--- " ++ s ++ " ---" ++ nt

infix 5 =?=
(=?=) :: forall a . (Show a, Eq a) => a -> a -> IO ()
x =?= y = myTest x y
--------------------------------

-- Реализуем улучшенную версию парсера PrsE

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

-- Этот парсер получил дополнительный целочисленный параметр в аргументе и в возвращаемом значении. С помощью этого параметра мы сможем отслеживать и передвигать текущую позицию в разбираемой строке и сообщать о ней пользователю в случае ошибки:

charEP  :: Char -> PrsEP Char
charEP c = satisfyEP (== c)

tsts1   :: [IO ()]
tsts1 =
  [ lbl "runPrsEP"
  , runPrsEP (charEP 'A') 0 "ABC"   =?= (1,Right ('A',"BC"))
  , runPrsEP (charEP 'A') 41 "BCD"  =?= (42,Left "pos 42: unexpected B")
  , runPrsEP (charEP 'A') 41 ""     =?= (42,Left "pos 42: unexpected end of input")
  ]

-- Вспомогательная функция parseEP дает возможность вызывать парсер более удобным образом по сравнению с runPrsEP, скрывая технические детали:
tsts2   :: [IO ()]
tsts2 =
  [ lbl "parseEP"
  , parseEP (charEP 'A') "ABC"      =?= Right ('A',"BC")
  , parseEP (charEP 'A') "BCD"      =?= Left "pos 1: unexpected B"
  , parseEP (charEP 'A') ""         =?= Left "pos 1: unexpected end of input"
  ]

-- Реализуйте функцию satisfyEP :: (Char -> Bool) -> PrsEP Char, обеспечивающую описанное выше поведение.
satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP fun where
  fun pos []                  = (pos + 1, Left $ "pos " ++ show (pos+1) ++ ": unexpected end of input")
  fun pos (x:xs) | pr x       = (pos + 1, Right (x, xs))
                 | otherwise  = (pos + 1, Left $ "pos " ++ show (pos+1) ++ ": unexpected " ++ [x])

--------------------------------------------------------------------------------
-- Сделайте парсер PrsEP a представителем классов типов Functor и Applicative, обеспечив следующее поведение:

instance Functor PrsEP where
  fmap f par = PrsEP fun where
    fun pos s = case runPrsEP par pos s of
      (n, Left  e)       -> (n, Left e)
      (n, Right (v, xs)) -> (n, Right (f v, xs))

instance Applicative PrsEP where
  pure v = PrsEP fun where
    fun pos s = (pos, Right (v, s))
  fp <*> vp = PrsEP fun where
    fun pos s = case runPrsEP fp pos s of
      (n, Left e)         -> (n, Left e)
      (n, Right (f, s'))  -> runPrsEP (f <$> vp) n s'

anyEP   :: PrsEP Char
anyEP = satisfyEP (const True)

testP   :: PrsEP (Char, Char)
testP = (,) <$> anyEP <* charEP 'B' <*> anyEP

tsts3   :: [IO ()]
tsts3 =
  [ lbl "Tests 3"
  , runPrsEP (pure 42) 0 "ABCDEFG"  =?= (0,Right (42,"ABCDEFG"))
  , runPrsEP testP 0 "ABCDE"        =?= (3,Right (('A','C'),"DE"))
  , parseEP testP "BCDE"            =?= Left "pos 2: unexpected C"
  , parseEP testP ""                =?= Left "pos 1: unexpected end of input"
  , parseEP testP "B"               =?= Left "pos 2: unexpected end of input"
  ]

-------------------------------------------------------------------------------

-- Сделайте парсер представителем класса типов Alternative, обеспечив следующее поведение для пары неудачных альтернатив: сообщение об ошибке возвращается из той альтернативы, которой удалось распарсить входную строку глубже.

tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c

tsts4 =
  [ runPrsEP empty 0 "ABCDEFG"                      =?= (0,Left "pos 0: empty alternative")
  , parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE" =?= Left "pos 3: unexpected E"
  , parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE" =?= Left "pos 3: unexpected E"
  , parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF" =?= Left "pos 2: unexpected E"
  ]


-------------------------------------------------------------------------------

main           :: IO ()
main = sequence_ $
  tsts1 ++ tsts2 ++ tsts3
