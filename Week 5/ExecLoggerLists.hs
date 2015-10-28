module ExecLoggerLists where

-- Реализованные ранее returnLog и bindLog позволяют определить тип Log представителем класса Monad:

import BindLog (Log(Log), bindLog, returnLog, add1Log, mult2Log)

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

-- Используя return и >>=, определите функцию execLoggersList, которая принимает некоторый элемент, список функций с логированием и возвращает результат последовательного применения всех функций в списке к переданному элементу вместе со списком сообщений, которые возвращались данными функциями:
execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList i l = foldl (>>=) (return i) l

l1  = execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
l1' = Log ["added one","multiplied by 2","multiplied by 100"] 800

tests = [l1 == l1']

main = do
    print tests
    putStrLn $ if and tests then "Good job!" else "Something goes wrong :-("