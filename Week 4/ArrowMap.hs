module ArrowMap where

{- Реализуйте instance MapLike для типа ArrowMap, определенного ниже. -}

import Prelude hiding (lookup)
import qualified Data.List

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap $ const Nothing
    lookup k (ArrowMap f) = f k
    insert k v  (ArrowMap f) = ArrowMap (\ x -> if x == k then Just v else f x)
    delete k (ArrowMap f) = ArrowMap (\ x -> if x == k then Nothing else f x)
    fromList kvs = ArrowMap (\ x -> Data.List.lookup x kvs)

m1 = ArrowMap (\x -> if x == 4 then Just 'a' else Nothing)
m2 = insert 4 'z' m1
m3 = delete 4 m1

m10 = fromList [(1, 'A'), (2, 'B'), (3, 'C')] :: ArrowMap Int Char

tests = 
    [ lookup 4 m1        == Just 'a'
    , lookup 4 m2        == Just 'z'
    , lookup 4 m3        == Nothing
    , lookup 5 m3        == Nothing

    , lookup 2 m10          == Just 'B'
    ]

main = print tests
