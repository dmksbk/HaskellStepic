{-# LANGUAGE RankNTypes #-}

module MyTest (myTest, myTests, (=?=), lbl) where

rt = "\x1b[31m"
gt = "\x1b[32m"
nt = "\x1b[0m"
bt = "\x1b[34m"

myTest  :: forall a. (Show a, Eq a) => a -> a -> IO ()
myTest x y =
  if x == y then
    putStrLn $ gt ++ "Correct: " ++ nt ++ show x
  else
    putStrLn $ rt ++ "  Wrong: " ++ nt ++ show x ++ " <> " ++ show y

myTests :: forall a. (Show a, Eq a) => [(a, a)] -> IO ()
myTests []     = return ()
myTests (t:ts) = uncurry myTest t >> myTests ts

lbl :: String -> IO ()
lbl s = putStrLn $ bt ++ "--- " ++ s ++ " ---" ++ nt

infix 4 =?=
x =?= y = myTest x y

tst1 = sequence_
  [ lbl "Dummy test"
  , 1 + 1 =?= 2
  ]
