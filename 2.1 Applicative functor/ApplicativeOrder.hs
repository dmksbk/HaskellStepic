{-# LANGUAGE RankNTypes #-}
import           Control.Applicative (ZipList (..), liftA2, (<**>))
import           Data.Char

-- Двойственный оператор аппликации (<**>) из модуля Control.Applicative изменяет направление вычислений, не меняя порядок эффектов:

infixl 4 <**>
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))
-- Определим оператор (<*?>) с той же сигнатурой, что и у (<**>), но другой реализацией:

infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)
-- Для каких стандартных представителей класса типов Applicative можно привести цепочку аппликативных вычислений, дающую разный результат в зависимости от того, какой из этих операторов использовался?

-- В следующих шести примерах вашей задачей будет привести такие контрпримеры для стандартных типов данных, для которых они существуют. Следует заменить аппликативное выражение в предложении in на выражение того же типа, однако дающее разные результаты при вызовах с (<??>) = (<**>) и (<??>) = (<*?>). Проверки имеют вид exprXXX (<**>) == exprXXX (<*?>) для различных имеющихся XXX. Если вы считаете, что контрпримера не существует, то менять ничего не надо.

-- Программирование — Напишите программу. Тестируется через stdin → stdout
-- Time Limit: 5 seconds
-- Memory Limit: 256 MB

exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op =
  let (<??>) = op
      infixl 4 <??>
  in Just 5 <??> Just (+2) -- NO
  -- in Just 5 <??> Just (+2) -- place for counterexample

exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
exprList op =
  let (<??>) = op
      infixl 4 <??>
  in [1,2] <??> [(+3),(+4),(+5)] -- YES
  -- in [1,2] <??> [(+3),(+4)] -- place for counterexample

exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op =
  let (<??>) = op
      infixl 4 <??>
  in ZipList [1,2] <??> ZipList [(+3),(+4)]  -- NO
  -- in ZipList [1,2] <??> ZipList [(+3),(+4)]  -- place for counterexample

exprEither :: (forall a b . Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
exprEither op =
  let (<??>) = op
      infixl 4 <??>
  in Left "AA" <??> Left "bb"  -- YES
  -- in Left "AA" <??> Right (+1)  -- place for counterexample

exprPair :: (forall a b . (String,a) -> (String,a -> b) -> (String,b)) -> (String,Int)
exprPair op =
  let (<??>) = op
      infixl 4 <??>
  in ("AA", 3) <??> ("bb",(+1))  -- YES
  -- in ("AA", 3) <??> ("",(+1))  -- place for counterexample

exprEnv :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
exprEnv op =
  let (<??>) = op
      infixl 4 <??>
  in (ord . head) <??> ((+) . length)  -- ?
  -- in length <??> (\_ -> (+5))  -- place for counterexample
