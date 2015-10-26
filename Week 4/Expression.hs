module Expression where

{- Исправьте определение функции expand так, чтобы она, используя дистрибутивность (а также, возможно, ассоциативность и коммутативность), возвращала значение, эквивалентное данному и являющееся суммой произведений числовых значений. -}

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

{- instance Show Expr where
    show (Val n) = show n
    show (e1 :+: e2) = show e1 ++ " + " ++ show e2
    show (e1 :*: e2) = show e1 ++ " * " ++ show e2
-}

expand :: Expr -> Expr
{- 
expand ((e1 :+: e2) :*: (e3 :+: e4)) = e1' :*: e3' :+: e1' :*: e4' :+: e2' :*: e3' :+: e2' :*: e4' where
    e1' = expand e1
    e2' = expand e2
    e3' = expand e3
    e4' = expand e4
-}

expand ((e1 :+: e2) :*: e) = expand (expand e1 :*: expand e) :+: expand (expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) = expand (expand e :*: expand e1) :+: expand (expand e :*: expand e2)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e

expr1  = (Val 1 :+: Val 2 :+: Val 3) :*: Val 4
expr1' = Val 1 :*: Val 4 :+: Val 2 :*: Val 4 :+: Val 3 :*: Val 4

expr2  = Val 4 :*: (Val 1 :+: Val 2 :+: Val 3)
expr2' = Val 4 :*: Val 1 :+: Val 4 :*: Val 2 :+: Val 4 :*: Val 3

expr3  = Val 1 :*: Val 2 :*: (Val 3 :+: Val 4)
expr3' = Val 1 :*: Val 2 :*: Val 3 :+: Val 1 :*: Val 2 :*: Val 4

expr4  = (Val 1 :+: Val 2) :*: (Val 3 :+: Val 4)
expr4' = Val 1 :*: Val 3 :+: Val 1 :*: Val 4 :+: Val 2 :*: Val 3 :+:Val 2 :*: Val 4

expr5  = Val 1 :*: (Val 2 :*: (Val 3 :*: (Val 4 :+: Val 5)))
expr5' = Val 1 :+: Val 2 :+: Val 3 :+: Val 4 :*: Val 1 :+: Val 2 :+: Val 3 :+: Val 5

tests =
    [ expand expr1        == expr1'
    , expand expr2        == expr2'
    , expand expr3        == expr3'
    , expand expr4        == expr4'
    , expand expr5		  == expr5' ]