module Task277 where

-- foo undefined ~> undefined
foo 	:: t -> t
foo a = a

-- bar id undefined -> undefined
bar		::b -> t -> t
bar = const foo


-- Good: baz undefined undefined -> True
baz :: t -> b -> Bool
baz x = const True

-- Infinite cycle
quux :: t
quux = let x = x in x

-- Good: Just integer
corge :: Integer
corge = 10

-- grault undefined 0 -> undefined
grault :: (Eq a, Num a) => t -> a -> t
grault x 0 = x
grault x y = x

-- Bad: garply undefined ~> undefiend
garply :: Integer -> Char
garply = grault 'q'

-- waldo undefined ~> indefined
waldo :: t -> t
waldo = foo



