module Data.Graph.Libgraph.Dagify where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Cycles
import Data.Graph.Libgraph.DepthFirst
import Data.List(nub)

dagify :: (Ord v, Eq a, Show v)  => ([v]->v) -> Graph v a -> Graph v a
dagify merge = (collapse merge) . remove

remove :: (Ord v, Show v) => Graph v a -> Graph v a
remove g = filterArc (\a -> not $ isBackEdge a && hasRedHead a) g
  where isBackEdge a = getEdgetype (getDfs g) a == BackEdge
        hasRedHead (Arc _ h _) = h `elem` getRedHeaders (getCycleNest g)

collapse :: (Ord v,Eq a) => ([v]->v) -> Graph v a -> Graph v a
collapse merge g = foldl collapseCycle g ics
  where (CycleTree _ ts) = getCycles (getCycleNest g)
        ics              = filter (\c -> case c of 
                                Irreducible _ -> True
                                _             -> False) ts
        collapseCycle g (Irreducible cts)
          = let ws = (verticesInCycle cts)
                v  = (merge ws)
            in rewire g ws v
        verticesInCycle = map (\(CycleTree v []) -> v)

rewire :: (Eq v, Eq a) => Graph v a -> [v] -> v -> Graph v a
rewire (Graph r vs as) ws c 
  = Graph r 
          (c : filter (not . (`elem` ws)) vs)
          (nub $ map fromTo $ filter (not . isInternalArc) as)
  where isInternalArc (Arc src tgt _) = src `elem` ws && tgt `elem` ws
        -- MF TODO: Should we keep arc-type 't' when rewiring?
        fromTo (Arc src tgt t)
          | tgt `elem` ws = Arc src c t 
          | src `elem` ws = Arc c   tgt t
          | otherwise     = Arc src tgt t
