module Data.Graph.Libgraph.Dominance
( Domsets
, getDomsets
, getDominators
) where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Dot
import Data.List

data Domsets vertex = Domsets { graph :: Graph vertex, sets :: [(vertex,[vertex])] }

-- | Vertices dominating the vertex given as argument.
getDominators :: Eq vertex => vertex -> Domsets vertex -> [vertex]
getDominators v = (lookup' v) . sets

-- | Compute dominator sets.
-- N.B. currently a naive algorithm is implemented with time-complexity O(vertex^2).
getDomsets :: Eq vertex => Graph vertex -> Domsets vertex
getDomsets g = dom domset0
  where domset0 = Domsets g $ map (\v -> (v, if v == r then [r] else vs)) vs
        vs      = vertices g
        r       = root g

dom :: Eq vertex => Domsets vertex -> Domsets vertex
dom ds = if sets ds' == sets ds then ds else dom ds'
  where ds'          = ds { sets = map update (sets ds) }
        update (v,_) = if v == r then (v,[v]) else (v, sets' v)
        sets' v      = [v] `union` isets v
        isets v      = foldl (\s p -> (getDominators p ds) `intersect` s) vs (preds g v)
        vs           = vertices g
        r            = root g
        g            = graph ds

instance (Eq vertex,Show vertex) => Show (Domsets vertex) where
  show d = showWith (graph d) showVertex showArc
    where showVertex v = show v ++ show (getDominators v d)
          showArc _    = ""
