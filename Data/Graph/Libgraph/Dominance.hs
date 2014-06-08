module Data.Graph.Libgraph.Dominance
( Domsets
, domsets
, dominators
) where
import Data.Graph.Libgraph.Core
import Data.List

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


