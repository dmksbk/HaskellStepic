module Bits where

data Bit = Zero | One deriving (Eq, Show)
data Sign = Minus | Plus deriving (Eq, Show)
data Z = Z Sign [Bit] deriving (Show)

iter	:: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
iter test f g x = if test x then f x : iter test f g (g x) else []

instance Enum Z where
	-- fromEnum :: Enum a => a -> Int
	fromEnum (Z s  b)
		| s == Plus =    m
		| s == Minus = (-m) where
			m = sum . zipWith (*) b' $ map (2^) [0..]
			b' = map bit2int b
			bit2int Zero = 0
			bit2int One  = 1

	-- toEnum	:: Enum a => Int -> a
	toEnum n = Z s m' where
		s = if n >= 0 then Plus else Minus
		m = iter (>0) (`mod` 2) (`div` 2) (abs n)
		m' = map int2bit m
		int2bit 0 = Zero
		int2bit 1 = One

add :: Z -> Z -> Z
add x y = toEnum $ fromEnum x + fromEnum y

mul :: Z -> Z -> Z
mul x y = toEnum $ fromEnum x * fromEnum y
