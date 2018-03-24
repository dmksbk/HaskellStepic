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
  [ runPrsEP (charEP 'A') 0 "ABC"   =?= (1,Right ('A',"BC"))
  , runPrsEP (charEP 'A') 41 "BCD"  =?= (42,Left "pos 42: unexpected B")
  , runPrsEP (charEP 'A') 41 ""     =?= (42,Left "pos 42: unexpected end of input")
  ]

-- Вспомогательная функция parseEP дает возможность вызывать парсер более удобным образом по сравнению с runPrsEP, скрывая технические детали:
tsts2   :: [IO ()]
tsts2 =
  [ parseEP (charEP 'A') "ABC"      =?= Right ('A',"BC")
  , parseEP (charEP 'A') "BCD"      =?= Left "pos 1: unexpected B"
  , parseEP (charEP 'A') ""         =?= Left "pos 1: unexpected end of input"
  ]

-- Реализуйте функцию satisfyEP :: (Char -> Bool) -> PrsEP Char, обеспечивающую описанное выше поведение.
satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP fun where
  fun pos []                  = (pos + 1, Left $ "pos " ++ show (pos+1) ++ ": unexpected end of input")
  fun pos (x:xs) | pr x       = (pos + 1, Right (x, xs))
                 | otherwise  = (pos + 1, Left $ "pos " ++ show (pos+1) ++ ": unexpected " ++ [x])

main           :: IO ()
main = sequence_ $
  tsts1 ++ tsts2
