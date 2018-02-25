module Step_5_7_6 where

import Control.Monad.Writer
import Data.Monoid

-- Давайте разработаем программное обеспечение для кассовых аппаратов одного исландского магазина. Заказчик собирается описывать товары, купленные покупателем, с помощью типа Shopping следующим образом:

type Shopping = Writer (Sum Integer) ()

purchase    :: String -> Integer -> Shopping
purchase _ p = writer ((), Sum p)

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

-- Последовательность приобретенных товаров записывается с помощью do-нотации. Для этого используется функция purchase, которую вам предстоит реализовать. Эта функция принимает наименование товара, а также его стоимость в исландских кронах (исландскую крону не принято делить на меньшие единицы, потому используется целочисленный тип Integer). Кроме того, вы должны реализовать функцию total:

--GHCi> total shopping1 
--19708

total   :: Shopping -> Integer
total = getSum . execWriter

tests = [
    total shopping1 == 19708 ]