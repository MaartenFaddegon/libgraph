module Data.Graph.Libgraph.Dagify where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Cycles
import Data.Graph.Libgraph.DepthFirst
import Data.List(nub)

dagify :: Ord vertex => ([vertex]->vertex) -> Graph vertex -> Graph vertex
dagify merge = (collapse merge) . remove

remove :: Ord v => Graph v -> Graph v
remove g = filterArc (\a -> not $ isBackEdge a && hasRedHead a) g
  where isBackEdge a = getEdgetype (getDfs g) a == BackEdge
        hasRedHead (Arc _ h) = h `elem` getRedHeaders (getCycleNest g)

collapse :: Ord v => ([v]->v) -> Graph v -> Graph v
collapse merge g = foldl collapseCycle g ics
  where (CycleTree _ ts) = getCycles (getCycleNest g)
        ics              = filter (\c -> case c of Irreducible _ -> True; _ -> False) ts
        collapseCycle g (Irreducible cts)
          = let ws = (verticesInCycle cts)
                v  = (merge ws)
            in rewire g ws v
        verticesInCycle = map (\(CycleTree v []) -> v)

rewire :: Eq vertex => Graph vertex -> [vertex] -> vertex -> Graph vertex
rewire (Graph r vs as) ws c 
  = Graph r 
          (c : filter (not . (`elem` ws)) vs)
          (nub $ map fromTo $ filter (not . isInternalArc) as)
  where isInternalArc (Arc src tgt) = src `elem` ws && tgt `elem` ws
        fromTo (Arc src tgt)
          | tgt `elem` ws = Arc src c
          | src `elem` ws = Arc c   tgt
          | otherwise     = Arc src tgt
