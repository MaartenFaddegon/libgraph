module Data.Graph.Libgraph.Dagify where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Cycles
import Data.Graph.Libgraph.DepthFirst
import Data.List(nub)

dagify :: Ord vertex => ([vertex]->vertex) -> Graph vertex -> Graph vertex
dagify merge = (collapse merge) . remove

remove :: Ord v => Graph v -> Graph v
remove g = filterArc (\a@(Arc _ tgt) -> not $ isBackEdge a && isRedHead tgt) g
  where isRedHead  h = h `elem` getRedHeaders (getCycleNest g)
        isBackEdge a = getEdgetype (getDfs g) a == BackEdge

collapse :: Ord v => ([v]->v) -> Graph v -> Graph v
collapse merge g = foldl merge' g ics
  where (CycleTree _ ts) = getCycles (getCycleNest g)
        ics              = filter (\c -> case c of Irreducible _ -> True; _ -> False) ts
        merge' g (Irreducible cts)
          = let ws = (map (\(CycleTree v []) -> v) cts)
                v  = (merge ws)
            in rewire g ws v

rewire :: Eq vertex => Graph vertex -> [vertex] -> vertex -> Graph vertex
rewire (Graph r vs as) ws c 
  = Graph r 
          (c : filter (not . (`elem` ws)) vs)
          (nub $ map fromTo $ filter (not . internalArc) as)
  where internalArc (Arc src tgt) = src `elem` ws && tgt `elem` ws
        fromTo (Arc src tgt)
          | tgt `elem` ws = Arc src c
          | src `elem` ws = Arc c   tgt
          | otherwise     = Arc src tgt
        
