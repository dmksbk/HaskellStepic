module MyTest (myTest) where

rt = "\x1b[31m"
gt = "\x1b[32m"
nt = "\x1b[0m"

myTest  :: (Show a, Eq a) => a -> a -> IO ()
myTest x y =
  if x == y then
    putStrLn $ gt ++ "Correct: " ++ nt ++ show x
  else
    putStrLn $ rt ++ "Wrong:   " ++ nt ++ show x ++ " /= " ++ show y
