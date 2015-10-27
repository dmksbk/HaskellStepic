module Logger where

-- Введём следующий тип:
data Log a = Log [String] a deriving (Eq, Show)

-- Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger, которая превращает обычную функцию, в функцию с логированием:
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f m = \ x -> Log [m] $ f x

-- | Далее, определите функцию execLoggers, Которая принимает некоторый элемент и две функции с логированием. execLoggers возвращает результат последовательного применения функций к элементу и список сообщений, которые были выданы при применении каждой из функций:
execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f1 f2 = case f1 x of
	Log m1 y -> case f2 y of
		Log m2 z -> Log (m1 ++ m2) z

-- | Test data
add1Log = toLogger (+(1::Int)) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

-- | Tests
tests =
	[ add1Log 3 						== Log ["added one"] 4 
	, mult2Log 3 						== Log ["multiplied by 2"] 6
	, execLoggers 3 add1Log mult2Log 	== Log ["added one","multiplied by 2"] 8 ]

-- | main function
main = do
    print tests
    putStrLn $ if and tests then "Good job!" else "Something goes wrong :-("