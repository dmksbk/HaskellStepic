module Step_5_4_6 where

import Control.Monad

-- Пусть имеется тип данных, который описывает конфигурацию шахматной доски:
type Board = Int

-- Кроме того, пусть задана функция, которая получает на вход некоторую конфигурацию доски и возвращает все возможные конфигурации, которые могут получиться, если какая-либо фигура сделает один ход.
nextPositions :: Board -> [Board]
nextPositions b = [b..b+3]

repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM n f a
    | n < 0     = fail "n should be non negative"
    | n == 0    = return a
    | otherwise = f a >>= repeatM (n-1) f

-- Напишите функцию, которая принимает конфигурацию доски, число ходов n, предикат и возвращает все возможные конфигурации досок, которые могут получиться, если фигуры сделают n ходов и которые удовлетворяют заданному предикату. При n < 0 функция возвращает пустой список.
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN brd n pred = 
    mfilter pred $ repeatM n nextPositions brd
