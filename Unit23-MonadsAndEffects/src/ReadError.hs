module ReadError
    (
    ) where

import           Except
import           MyTest

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
  [ runExcept (tryRead "5" :: Except ReadError Int)                 =?= Right 5
  , runExcept (tryRead "5" :: Except ReadError Double)              =?= Right 5.0
  , runExcept (tryRead "5zzz" :: Except ReadError Int)              =?= Left (NoParse "5zzz")
  , runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ())) =?= Right (True,())
  , runExcept (tryRead "" :: Except ReadError (Bool, ()))           =?= Left EmptyInput
  , runExcept (tryRead "wrong" :: Except ReadError (Bool, ()))      =?= Left (NoParse "wrong")
  ]

tests = sequence_ test1
