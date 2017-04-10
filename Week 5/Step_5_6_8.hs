module Step_5_6_8 where

-- Реализуйте функцию local' из прошлого задания. Считайте, что монада Reader определена так, как на видео:

data Reader r a = Reader { runReader :: (r -> a) }

asks = Reader

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

-- local'  :: (e -> e') -> Reader e' a -> Reader e a

db = [("John", "123"), ("Mike", "qwerty"), ("Sil", "fjs6s")]

firstName   :: Reader [(String, String)] String
firstName = asks $ fst . head

