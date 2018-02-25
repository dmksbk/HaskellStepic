module Main where

data P = P Int

instance Show P where
	show (P v) = show v ++ "."

a = P 127
b = P 224
c = P 120
d = 12

ip = show a ++ show b ++ show c ++ show d

main = putStrLn ip