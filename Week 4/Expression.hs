module Expression where

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: (e3 :+: e4)) = e1' :*: e3' :+: e1' :*: e4' :+: e2' :*: e3' :+: e2' :*: e4' where
    e1' = expand e1
    e2' = expand e2
    e3' = expand e3
    e4' = expand e4
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e

expr1 = (Val 1 :+: Val 2 :+: Val 3) :*: Val 4
expr2 = (Val 1 :+: Val 2) :*: (Val 3 :+: Val 4)