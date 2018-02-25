module Odd where

data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
    succ (Odd n) = Odd $ n + 2
    pred (Odd n) = Odd $ n - 2
    enumFrom n = n : enumFrom (succ n)

    enumFromThen n@(Odd n') k@(Odd k') =
    	n : enumFromThen k (Odd $ 2 * k' - n')

    enumFromTo n@(Odd n') m@(Odd m')
    	| n' > m' 	= []
    	| otherwise = n : enumFromTo (succ n) m

    enumFromThenTo n@(Odd n') k@(Odd k') m@(Odd m')
    	| k' > n' && n' > m' 	= []
    	| k' < n' && n' < m'	= []
    	| otherwise = n : enumFromThenTo k (Odd $ 2 * k' - n') m

    toEnum n = Odd  $ 2 * (toInteger n) + 1
    fromEnum (Odd n) = fromInteger $ div (n - 1) 2


main = do
	print $ succ $ Odd (-100000000000003)
	print $ Odd (-100000000000001) == succ (Odd (-100000000000003))