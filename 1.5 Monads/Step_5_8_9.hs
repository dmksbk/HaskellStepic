module Step_5_8_9 where

import Control.Monad
import Control.Monad.State

-- Если бы мы хотели вычислить nn-е число Фибоначчи на императивном языке программирования, мы бы делали это с помощью двух переменных и цикла, обновляющего эти переменные:

{-
def fib(n):
  a, b = 0, 1
  for i in [1 .. n]:
    a, b = b, a + b
  return a
С точки зрения Хаскеля, такую конструкцию удобно представлять себе как вычисление с состоянием. Состояние в данном случае — это два целочисленных значения.

Императивный алгоритм действует очень просто: он совершает nn шагов, каждый из которых некоторым образом изменяет текущее состояние. Первым делом, реализуйте функцию fibStep, изменяющую состояние таким же образом, как и один шаг цикла в императивном алгоритме:

GHCi> execState fibStep (0,1)
(1,1)
GHCi> execState fibStep (1,1)
(1,2)
GHCi> execState fibStep (1,2)
(2,3)
После этого останется лишь применить этот шаг nn раз к правильному стартовому состоянию и выдать ответ. Реализуйте вспомогательную функцию execStateN, которая принимает число шагов nn, вычисление с состоянием и начальное состояние, запускает вычисление nn раз и выдает получившееся состояние (игнорируя сами результаты вычислений). Применяя эту функцию к fibStep, мы сможем вычислять числа Фибоначчи:
-}

fibStep    :: State (Integer, Integer) ()
fibStep = do
    (a, b) <- get
    put (b, a + b)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM n m)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)

tests = 
    [ execState fibStep (0, 1)      == (1, 1) 
    , execState fibStep (1, 1)      == (1, 2)
    , execState fibStep (1, 2)      == (2, 3)
    , execStateN 0 fibStep (1, 1)   == (1, 1)
    , execStateN 1 fibStep (1, 1)   == (1, 2)
    , execStateN 2 fibStep (1, 1)   == (2, 3)
    , execStateN 3 fibStep (1, 1)   == (3, 5)
    , fib 1                         == 1
    , fib 2                         == 1
    , fib 3                         == 2
    , fib 4                         == 3
    , fib 5                         == 5
    , fib 6                         == 8
    ]

allTests = and tests