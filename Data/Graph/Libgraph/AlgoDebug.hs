{-# LANGUAGE DeriveGeneric #-}
module Data.Graph.Libgraph.AlgoDebug where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Dagify(collapse,remove)
import Prelude hiding (Right)
import Data.List(find)
import Data.Maybe(isJust)
import GHC.Generics

data AssistedMessage = InconclusiveProperty String | PassingProperty String deriving (Eq,Show,Ord,Generic)

data Judgement = Right | Wrong | Unassessed | Assisted [AssistedMessage] deriving (Eq,Show,Ord,Generic)

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

{- Divide and Query: Try to find a node that divides the tree in two. -}
next_daq :: Ord v => Graph v a -> (v -> Judgement) -> v
next_daq tree j = snd (foldr1 f ws)
  where c  = succCache tree
        ws = weight j c
               -- start from deepest wrong node; if no wrong nodes start from the root node
               (case start j c (root tree) of (Just r) -> r; Nothing -> root tree)
        w  = (maximum (map fst ws)) `div` 2 -- the "ideal" weight
        f x y | abs (w - (fst x)) < abs (w - (fst y)) = x
              | otherwise                             = y

weight :: (v -> Judgement) -> (v -> [v]) -> v -> [(Int,v)]
weight j c v | j v == Right = [(0,v)]        -- discard subtrees that are right
             | otherwise    = (w,v) : foldr (++) [] vs
  where vs = map (weight j c) (c v)          -- weighted children
        w  = 1 + (sum $ map (fst . head) vs) -- weight of v


-- find starting point (i.e. the "deepest" node that is wrong)
start :: (v -> Judgement) -> (v -> [v]) -> v -> Maybe v
start j c v = case filter isJust (map (start j c) (c v)) of
  []    -> if j v == Wrong then Just v else Nothing
  (w:_) -> w
