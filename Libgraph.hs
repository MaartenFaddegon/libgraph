module Libgraph
( Arc(..), Graph(..)
)
where
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------
-- External representation of graphs

data Arc vertex   = Arc   { source :: vertex
                          , target :: vertex
                          }
data Graph vertex = Graph { root :: vertex
                          , vertices :: [vertex]
                          , arcs :: [Arc vertex]
                          }

--------------------------------------------------------------------------------
-- Successors and predecessors

succs :: Eq vertex => Graph vertex -> vertex -> [vertex]
succs g v = map target $ filter ((== v) . source) (arcs g)

preds :: Eq vertex => Graph vertex -> vertex -> [vertex]
preds g v = map source $ filter ((== v) . target) (arcs g)

--------------------------------------------------------------------------------
-- Depth first search

preorder :: Eq vertex => Graph vertex -> [vertex]
preorder g = dfs g [root g] []

dfs :: Eq vertex => Graph vertex -> [vertex] -> [vertex] -> [vertex]
dfs g [] vs        = vs
dfs g (v:stack) vs = if v `elem` vs
                     then dfs g stack vs
                     else dfs g (succs g v ++ stack) (v:vs)

--------------------------------------------------------------------------------
-- Dominance

type Domsets vertex = [(vertex,[vertex])]

dominators :: Eq vertex => vertex -> Domsets vertex -> [vertex]
dominators = lookup'

domsets :: Eq vertex => Graph vertex -> Domsets vertex
domsets g = dom g domset0
  where domset0 = map (\v -> (v, if v == r then [r] else vs)) vs
        vs      = vertices g
        r       = root g

dom :: Eq vertex => Graph vertex -> Domsets vertex -> Domsets vertex
dom g ds = if ds' == ds then ds else dom g ds'
  where ds'       = map (\(v,_) -> (v, nextSet v)) ds
        nextSet v = foldl (\s p -> (dominators p ds) `intersect` s) [] (preds g v)

--------------------------------------------------------------------------------
-- Some helper functions

lookup' :: Eq a => a -> [(a, b)] -> b
lookup' x ys = fromJust (lookup x ys)
