module Data.Graph.Libgraph.AlgoDebug where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Dagify(collapse,remove)
import Prelude hiding (Right)

data Judgement = Right | Wrong | Unassessed | Assisted String deriving (Eq,Show,Ord)

findFaulty_dag :: (Ord v, Eq a, Show v) => (v -> Judgement) -> Graph v a -> [v]
findFaulty_dag judge g = filter isFaulty (vertices g)
  where isFaulty v =  (judge v == Wrong)
                   && (null $ filter ((/=Right) . judge) (succs g v))

findFaulty :: (Ord v, Eq a, Show v) 
           => (v -> Judgement) -> ([v]->v) -> Graph v a -> [v]
findFaulty isWrong merge = (findFaulty_dag isWrong) . (collapse merge) . remove
