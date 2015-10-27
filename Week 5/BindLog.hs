module BindLog where

import Logger (Log(Log), toLogger, execLoggers, add1Log, mult2Log)

-- Реализуйте фукцию bindLog, которая работает подобно оператору >>= для контекста Log.
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log m1 v1) f = let (Log m2 v2) = f v1 in Log (m1 ++ m2) v2 

-- | Test data
l1  = Log ["nothing done yet"] 0 `bindLog` add1Log
l1' = Log ["nothing done yet","added one"] 1
l2  = Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
l2' = Log ["nothing done yet","added one","multiplied by 2"] 8 

-- | Tests
tests =
    [ l1    == l1'
    , l2    == l2' ]

-- | main function
main = do
    print tests
    putStrLn $ if and tests then "Good job!" else "Something goes wrong :-("