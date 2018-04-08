{-# LANGUAGE FlexibleContexts #-}

module ReadError
    (
    ) where

import           Control.Monad (zipWithM)
import           Except
import           MyTest

-- | task 2.3.1.8
-- Реализуйте функцию tryRead, получающую на вход строку и пытающуюся всю эту строку превратить в значение заданного типа. Функция должна возвращать ошибку в одном из двух случаев: если вход был пуст или если прочитать значение не удалось.

-- Информация об ошибке хранится в специальном типе данных:

data ReadError = EmptyInput | NoParse String
  deriving (Eq, Show)

tryRead :: Read a => String -> Except ReadError a
tryRead [] = throwE EmptyInput
tryRead s  = if null r
  then throwE $ NoParse s
  else return . fst . head $ r
    where
      r = filter (null . snd) (reads s)


test1 =
  [ lbl "tryRead"
  , runExcept (tryRead "5" :: Except ReadError Int)                 =?= Right 5
  , runExcept (tryRead "5" :: Except ReadError Double)              =?= Right 5.0
  , runExcept (tryRead "5zzz" :: Except ReadError Int)              =?= Left (NoParse "5zzz")
  , runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ())) =?= Right (True,())
  , runExcept (tryRead "" :: Except ReadError (Bool, ()))           =?= Left EmptyInput
  , runExcept (tryRead "wrong" :: Except ReadError (Bool, ()))      =?= Left (NoParse "wrong")
  ]

-- end task 2.3.1.8

-- | task 2.3.1.9

-- Используя tryRead из прошлого задания, реализуйте функцию trySum, которая получает список чисел, записанных в виде строк, и суммирует их. В случае неудачи, функция должна возвращать информацию об ошибке вместе с номером элемента списка (нумерация с единицы), вызвавшим ошибку.

-- Для хранения информации об ошибке и номере проблемного элемента используем новый тип данных:

data SumError = SumError Int ReadError
  deriving (Eq, Show)

trySum :: [String] -> Except SumError Integer
trySum xs = sum <$> zipWithM exc xs [1..] where
  exc r n = withExcept (SumError n) (tryRead r)

test2 =
  [ lbl "trySum"
  , runExcept (trySum ["10", "20", "30"])  =?= Right 60
  , runExcept (trySum ["10", "20", ""])    =?= Left (SumError 3 EmptyInput)
  , runExcept (trySum ["10", "two", "30"]) =?= Left (SumError 2 (NoParse "two"))
  ]

-- Подсказка: функция withExcept в этом задании может быть чрезвычайно полезна. Постарайтесь максимально эффективно применить знания, полученные на прошлой неделе.

tests = sequence_ $ test1 ++ test2
