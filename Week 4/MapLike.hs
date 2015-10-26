module MapLike where

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)
    
instance MapLike ListMap where
    empty = ListMap []
    lookup _ (ListMap []) = Nothing
    lookup k' (ListMap ((k,v) : ms)) = if k' == k then Just v else lookup k ms
    insert k' v' (ListMap []) = ListMap [(k', v')]
    insert k' v' (ListMap ((k,v) : ms)) = if k' == k then ListMap ((k',v') : ms) else ListMap ((k, v) : insert k' v' ms)
    delete _ [] = []
    delete k' ((k,v):ms) = if k' == k then ms else (k,v) : delete k' ms