-- -- Реализуйте парсер getList, который разбирает строки из чисел, разделенных точкой с запятой, и возвращает список строк, представляющих собой эти числа:
--
-- GHCi> parseTest getList "1;234;56"
-- ["1","234","56"]
-- GHCi> parseTest getList "1;234;56;"
-- parse error at (line 1, column 10):
-- unexpected end of input
-- expecting digit
-- GHCi> parseTest getList "1;;234;56"
-- parse error at (line 1, column 3):
-- unexpected ";"
-- expecting digit
-- Совет: изучите парсер-комбинаторы, доступные в модуле Text.Parsec, и постарайтесь найти наиболее компактное решение.

import Text.Parsec

getList :: Parsec String u [String]
getList = (many1 digit) `sepBy1` (char ';')

main = do
  parseTest getList "1;234;56"
  parseTest getList "1;234;56;"
  parseTest getList "1;;234;56"
