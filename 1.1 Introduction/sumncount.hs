sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (toInteger.sum $ digits, toInteger.length $ digits) where
	digits = loop.abs $ x
	loop x
		| x < 10 = [x]
		| otherwise = (x `mod` 10) : (loop $ x `div` 10)


main = print $ sum'n'count (-39)