module Map where

data  Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord x y) = Coord x' y' where
	x' = toCenter x
	y' = toCenter y
	toCenter x = w * fromIntegral x + 0.5 * w

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x y) = Coord x' y' where
	x' = floor $ x / w
	y' = floor $ y / w