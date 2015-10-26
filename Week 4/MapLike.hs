module MapLike where

{- Ниже приведено определение класса MapLike типов, похожих на тип Map. Определите instance MapLike для типа ListMap, определенного ниже как список пар ключ-значение. Для каждого ключа должно храниться не больше одного значения. Функция insert заменяет старое значение новым, если ключ уже содержался в структуре. -}

import Prelude hiding (lookup)
import qualified Data.List as L
import Debug.Trace

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
    
    lookup _  (ListMap []) = Nothing
    lookup k' (ListMap ((k, v) : ms)) = if k' == k then Just v else lookup k' $ ListMap ms
    
    insert k v (ListMap kvs) = ListMap $ insert' k v kvs where
    	insert' k' v' [] = [(k', v')]
    	insert' k' v' ((k, v) : kvs) = if k' == k then (k', v') : kvs else (k, v) : insert' k' v' kvs
    
    delete k (ListMap kvs) = ListMap $ delete' k kvs where
    	delete' _  [] = []
    	delete' k' ((k, v) : kvs) = if k' == k then kvs else (k, v) : delete' k' kvs

testMap1 = ListMap [(1, 'A'), (2, 'B'), (3, 'C')]
testMap2 = ListMap [(1, 'A'), (2, 'Z'), (3, 'C')]

type Tests = [Bool]
tests		:: Tests
tests =
	[ lookup 2 testMap1 		== Just 'B'
	, insert 2 'Z' testMap1 	== testMap2
	]

main = print tests

