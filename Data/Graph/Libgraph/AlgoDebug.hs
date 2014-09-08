module Data.Graph.Libgraph.AlgoDebug where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Dagify(collapse,remove)

findFaulty_dag :: (Ord v, Eq a, Show v) => (v -> Bool) -> Graph v a -> [v]
findFaulty_dag isWrong g = filter isFaulty (vertices g)
  where isFaulty v = isWrong v && (null $ (filter isWrong) (succs g v))

findFaulty :: (Ord v, Eq a, Show v) => (v -> Bool) -> ([v]->v) -> Graph v a -> [v]
findFaulty isWrong merge = (findFaulty_dag isWrong) . (collapse merge) . remove
