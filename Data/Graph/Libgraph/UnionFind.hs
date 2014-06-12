module Data.Graph.Libgraph.UnionFind
( UF
, fromList
, find
, union
) where

import Data.UnionFind.IntMap( Point,PointSupply,newPointSupply
                            , fresh,repr,descriptor)
import qualified Data.UnionFind.IntMap as UF
import Data.IntMap.Lazy(IntMap,(!))
import qualified Data.IntMap.Lazy as IM

data UF = UF {ps :: PointSupply Int, im :: IntMap (Point Int)}

fromList :: [Int] -> UF
fromList xs = foldl singleton (UF newPointSupply IM.empty) xs

singleton :: UF -> Int -> UF
singleton uf x = UF ps' $ IM.insert x p (im uf)
  where (ps',p) = fresh (ps uf) x

point :: UF -> Int -> Point Int
point uf i = (im uf) ! i

find :: UF -> Int -> Int
find uf = (descriptor $ ps uf) . (repr $ ps uf) . (point uf)

union :: UF -> Int -> Int -> UF
union uf x y = undefined
