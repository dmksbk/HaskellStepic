module Token where

import Data.Char (isDigit)

-- Рассмотрим язык арифметические выражения, которые состоят из чисел, скобок, операций сложения и вычитания. Конструкции данного языка можно представить следующим типом данных:

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

-- Реализуйте лексер арифметических выражений. Для начала реализуйте следующую функцию:
-- Она проверяет, является ли переданная строка числом (используйте функцию isDigit из модуля Data.Char), знаком "+" или "-", открывающейся или закрывающейся скобкой. Если является, то она возвращает нужное значение обёрнутое в Just, в противном случае - Nothing:
asToken :: String -> Maybe Token
asToken t   | t == "+"       = Just Plus
            | t == "-"       = Just Minus
            | t == "("       = Just LeftBrace
            | t == ")"       = Just RightBrace
            | all isDigit  t = Just . Number . read $ t
            | otherwise      = Nothing


-- Далее, реализуйте функцию tokenize: Функция принимает на вход строку и если каждое слово является корректным токеном, то она возвращает список этих токенов, завёрнутый в Just. В противном случае возвращается Nothing. Функция должна разбивать входную строку на отдельные слова по пробелам (используйте библиотечную функцию words). Далее, полученный список строк должен быть свёрнут с использованием функции asToken и свойств монады Maybe. Обратите внимание, что скобки отделяются пробелами от остальных выражений!
tokenize :: String -> Maybe [Token]
tokenize str = sequence . map asToken . words $ str


-- | Tests
tests =
    [ asToken "123"             == Just (Number 123)
    , asToken "abc"             == Nothing
    , tokenize "1 + 2"          == Just [Number 1,Plus,Number 2]
    , tokenize "1 + ( 7 - 2 )"  == Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]
    , tokenize "1 + abc"        == Nothing ]

testsCount = countCorrect ++ " out of " ++ countAll ++ " tests are correct" where
    countCorrect = show . length $ filter ( == True) tests
    countAll = show . length $ tests

-- | main function
main = do
    print tests
    putStrLn $ if and tests then "Good job!" else "Something goes wrong: " ++ testsCount
