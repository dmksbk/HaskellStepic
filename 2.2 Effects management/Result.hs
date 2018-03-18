module Result where

-- Сделайте тип данных представителем класса типов Traversable (и всех других необходимых классов типов).

data Result a = Ok a | Error String deriving (Eq,Show)

-- GHCi> traverse (\x->[x+2,x-2]) (Ok 5)
-- [Ok 7,Ok 3]
-- GHCi> traverse (\x->[x+2,x-2]) (Error "!!!")
-- [Error "!!!"]

instance Functor Result where
  fmap _ (Error e) = Error e
  fmap f (Ok a)    = Ok (f a)

instance Foldable Result where
  foldMap _ (Error _) = mempty
  foldMap f (Ok a)    = f a

instance Traversable Result where
  traverse _ (Error e) = pure (Error e)
  traverse f (Ok a)    = pure Ok <*> f a
