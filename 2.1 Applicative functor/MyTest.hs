{-# LANGUAGE RankNTypes #-}

module MyTest (myTest, myTests, (=?=)) where

rt = "\x1b[31m"
gt = "\x1b[32m"
nt = "\x1b[0m"

myTest  :: forall a. (Show a, Eq a) => a -> a -> IO ()
myTest x y =
  if x == y then
    putStrLn $ gt ++ "Correct: " ++ nt ++ show x
  else
    putStrLn $ rt ++ "Wrong:   " ++ nt ++ show x ++ " <> " ++ show y

myTests :: forall a. (Show a, Eq a) => [(a, a)] -> IO ()
myTests []     = return ()
myTests (t:ts) = myTest (fst t) (snd t) >> myTests ts

infix 5 =?=
x =?= y = myTest x y
