module Data.Graph.Libgraph.UnionFind
( UF
, fromList
, find
, union
) where
import Data.UnionFind.IntMap(PointSupply,newPointSupply,fresh,repr,descriptor)
import qualified Data.UnionFind.IntMap as UF

type UF = PointSupply a

fromList :: [a] -> UF a
fromList xs = foldl singleton newPointSupply xs

singleton :: UF a -> a -> UF a
singleton uf x = fst (fresh uf x)

find :: UF a -> a -> a
find uf = (descriptor uf) . (repr uf)

union :: UF a 
