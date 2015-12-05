module Data.Graph.Libgraph.AlgoDebug where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Dagify(collapse,remove)
import Prelude hiding (Right)
import Data.List(find)
import Data.Maybe(isJust)

data Judgement = Right | Wrong | Unassessed | Assisted String deriving (Eq,Show,Ord)

findFaulty_dag :: (Ord v, Eq a, Show v) => (v -> Judgement) -> Graph v a -> [v]
findFaulty_dag judge g = filter isFaulty (vertices g)
  where isFaulty v =  (judge v == Wrong)
                   && (null $ filter ((/=Right) . judge) (succs g v))

findFaulty :: (Ord v, Eq a, Show v) 
           => (v -> Judgement) -> ([v]->v) -> Graph v a -> [v]
findFaulty isWrong merge = (findFaulty_dag isWrong) . (collapse merge) . remove

next_step :: Eq v => Graph v a -> (v -> Judgement) -> v
next_step tree j = case go r of 
  Just v  -> v
  Nothing -> r -- no defect?
  where
  r = root tree
  go v | v == r       = findJust (map go (succs tree v))
       | j v == Right = Nothing                -- v is right; don't examine (grand)children
       | j v == Wrong = case findJust (map go (succs tree v)) of
                          Nothing  -> (Just v) -- v is a faulty node
                          (Just w) -> (Just w) -- found next node w in (grand)children of v
       | otherwise    = Just v                 -- v is an unassed node
  findJust mvs = case (find (isJust) mvs) of Just justv -> justv; Nothing -> Nothing

{- A more efficient next_step implementation; that starts from the
   current computation statement. The problem with this implementation
   is that it can get "stuck" in trees with nodes that already have some
   judged nodes (e.g. due to free exploration or using property-assisted
   algorithmic debugging.)

next_step :: Eq v => Graph v a -> (v -> Judgement) -> v -> v
next_step tree j v 
  | j v == Wrong || v == root tree =
    case filter (\w -> j w == Unassessed) (succs tree v) of
                      []    -> v
                      (w:_) -> w

  | j v == Right =
    case preds tree v of
                      []    -> v
                      (w:_) -> next_step tree j w

  | otherwise = v -- j v is Unassessed or Assisted
-}
