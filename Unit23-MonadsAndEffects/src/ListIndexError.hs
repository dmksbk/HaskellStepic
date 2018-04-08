module ListIndexError
    (
    ) where

import           Except
import           MyTest

-- | task 2.3.1.7
-- В модуле Control.Monad.Trans.Except библиотеки transformers имеется реализация монады Except с интерфейсом, идентичным представленному в видео-степах, но с более общими типами. Мы изучим эти типы в следующих модулях, однако использовать монаду Except из библиотеки transformers мы можем уже сейчас.

-- Введём тип данных для представления ошибки обращения к списку по недопустимому индексу:
data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)

-- Реализуйте оператор (!!!) :: [a] -> Int -> Except ListIndexError a доступа к элементам массива по индексу, отличающийся от стандартного (!!) поведением в исключительных ситуациях. В этих ситуациях он должен выбрасывать подходящее исключение типа ListIndexError.
infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
-- lst !!! n | n < 0     = throwE  ErrNegativeIndex
--           | otherwise = withExcept (\_ -> ErrIndexTooLarge n) (lst !!! n)

(x:xs)  !!! 0 = return x
[]      !!! n = throwE $ ErrIndexTooLarge n
(x:xs)  !!! n = if n < 0
    then throwE ErrNegativeIndex
    else withExcept (const $ ErrIndexTooLarge n)  (xs !!! (n - 1))

(!!!!) xs n = runExcept $ xs !!! n

tst1 =
  [ runExcept ([1..100] !!! 5)  =?= Right 6
  , [1,2,3] !!!! 0              =?= Right 1
  , [1..] !!!! 5                =?= Right 6 -- infinity list, could hang
  , [1,2,3] !!!! 42             =?= Left (ErrIndexTooLarge 42)
  , [1,2,3] !!!! (-3)           =?= Left ErrNegativeIndex
  ]

tests = sequence_ tst1
