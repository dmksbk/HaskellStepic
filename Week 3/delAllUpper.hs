module Main where

import Data.Char (isUpper)

delAllUpper :: String -> String
delAllUpper = unwords . filter notUpperCase . words where
	notUpperCase = not . all isUpper

main = print $ delAllUpper "Abc IS not ABC"