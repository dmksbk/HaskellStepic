{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Temperature where

-- Расширьте интерфейс для работы с температурами из предыдущего видео Кельвинами и реализуйте функцию, обеспечивающую следующее поведение

newtype Temperature a = Temperature Double
  deriving (Num, Show, Eq, Fractional)

data Celsius
data Fahrenheit
data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature c) = Temperature (c - 273.15)


-- GHCi> k2c 0                          =?= Temperature (-273.15)
-- GHCi> k2c 0 == Temperature (-273.15) =?= True
-- GHCi> k2c 273.15                     =?= Temperature 0.0

main :: IO ()
main = do
  print $ k2c 0
  print $ k2c 0 == Temperature (-273.15)
  print $ k2c 273.15
