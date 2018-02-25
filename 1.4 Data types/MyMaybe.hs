module MyMaybe where

{- Реализуйте instance Monoid для Maybe' a так, чтобы mempty не был равен Maybe' Nothing. Нельзя накладывать никаких дополнительных ограничений на тип a, кроме указанных в условии. -}

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' $ Just mempty
    mappend (Maybe' Nothing)  (Maybe' Nothing)     = Maybe' Nothing
    mappend (Maybe' Nothing)  (Maybe' (Just y))    = Maybe' $ Nothing
    mappend (Maybe' (Just x)) (Maybe' Nothing)     = Maybe' $ Nothing
    mappend (Maybe' (Just x)) (Maybe' (Just y))    = Maybe' . Just $ x `mappend` y 

e1 = mempty :: Maybe' String
e2 = Maybe' Nothing
e3 = Maybe' $ Just "Hello"
e4 = Maybe' $ Just " world"
e5 = Maybe' $ Just "Hello world"

tests =
    [ e1                    == Maybe' (Just "")
    , e1 `mappend` e2       == e1
    , e2 `mappend` e1       == e1
    , e1 `mappend` e3       == e3
    , e4 `mappend` e1       == e4
    , e3 `mappend` e4       == e5
    ]

main = print tests