module Expression where

{- Исправьте определение функции expand так, чтобы она, используя дистрибутивность (а также, возможно, ассоциативность и коммутативность), возвращала значение, эквивалентное данному и являющееся суммой произведений числовых значений. -}

import Control.Monad (when)

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

isSumProd      ::   Expr -> Bool
isSumProd (Val _)   = True
isSumProd (l :+: r) = isSumProd l && isProd r
isSumProd (l :*: r) = isProd l && isProd r

isProd         :: Expr -> Bool
isProd (Val _)         = True
isProd (l :+: r)     = False
isProd (l :*: r)     = isProd l && isProd r

expand'        :: Expr -> Expr
expand' (e1 :*: (e2 :*: e3))= (expand' e1 :*: expand' e2) :*: expand' e3
expand' (e1 :+: (e2 :+: e3))= (expand' e1 :+: expand' e2) :+: expand' e3
expand' ((e1 :+: e2) :*: e) = expand' (expand' e1 :*: expand' e) :+: expand' (expand' e2 :*: expand' e)
expand' (e :*: (e1 :+: e2)) = expand' (expand' e :*: expand' e1) :+: expand' (expand' e :*: expand' e2)
expand' (e1 :+: e2) = expand' e1 :+: expand' e2
expand' (e1 :*: e2) = expand' e1 :*: expand' e2
expand' e = e

expand         :: Expr -> Expr
expand e = if isSumProd e then e else expand (expand' e)

expr1  = (Val 1 :+: Val 2 :+: Val 3) :*: Val 4
expr1' = Val 1 :*: Val 4 :+: Val 2 :*: Val 4 :+: Val 3 :*: Val 4

expr2  = Val 4 :*: (Val 1 :+: Val 2 :+: Val 3)
expr2' = Val 4 :*: Val 1 :+: Val 4 :*: Val 2 :+: Val 4 :*: Val 3

expr3  = Val 1 :*: Val 2 :*: (Val 3 :+: Val 4)
expr3' = Val 1 :*: Val 2 :*: Val 3 :+: Val 1 :*: Val 2 :*: Val 4

expr4  = (Val 1 :+: Val 2) :*: (Val 3 :+: Val 4)
expr4' = Val 1 :*: Val 3 :+: Val 1 :*: Val 4 :+: Val 2 :*: Val 3 :+:Val 2 :*: Val 4

expr5  = Val 1 :*: (Val 2 :*: (Val 3 :*: (Val 4 :+: Val 5)))
expr5' = Val 1 :*: Val 2 :*: Val 3 :*: Val 4 :+: Val 1 :*: Val 2 :*: Val 3 :*: Val 5

tests =
    [ expand expr1        == expr1'
    , expand expr2        == expr2'
    , expand expr3        == expr3'
    , expand expr4        == expr4'
    , expand expr5          == expr5' ]

main = do
    print tests
    putStrLn $ if and tests then "Good job!" else "Something goes wrong :-("