module Libgraph
( Arc(..), Graph(..)
)
where
import Control.Monad.State
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

data Dfs vertex = Dfs { num :: [(vertex,Int)], lastVisit :: [(vertex,Int)] }

data DfsState vertex = DfsState { graph      :: Graph vertex
                                , stack      :: [vertex]
                                , seen       :: [vertex]
                                , time       :: Int
                                , num'       :: [(vertex,Int)]
                                , lastVisit' :: [(vertex,Int)]
                                }

dfs :: Eq vertex => Graph vertex -> Dfs vertex
dfs g = Dfs (num' finalState) (lastVisit' finalState)
  where state0 = DfsState { graph      = g
                          , stack      = [root g]
                          , seen       = []
                          , time       = 0
                          , num'       = []
                          , lastVisit' = []
                          }
        finalState = execState (visit $ root g) state0

visit :: Eq vertex => vertex -> State (DfsState vertex) ()
visit v = do see v
             pushSuccs v
             s <- gets stack
             mw <- pop
             case mw of Just w  -> visit w
                        Nothing -> return ()

pushSuccs :: Eq vertex => vertex -> State (DfsState vertex) ()
pushSuccs v = do g  <- gets graph
                 vs <- gets seen
                 modify $ \s -> s { stack = (succs g v \\ vs) ++ (stack s) }

pop :: Eq vertex => State (DfsState vertex) (Maybe vertex)
pop = do s <- gets stack
         case s of []     -> return Nothing
                   (x:xs) -> do modify $ \s -> s { stack = xs }
                                return $ Just x

see :: vertex -> State (DfsState vertex) ()
see v = modify $ \s -> s { seen = v           : seen s
                         , num' = (v, time s) : num' s
                         , time = time s + 1
                         }

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
-- Cycles



--------------------------------------------------------------------------------
-- Some helper functions

lookup' :: Eq a => a -> [(a, b)] -> b
lookup' x ys = fromJust (lookup x ys)

fstElem :: Eq a => a -> [(a, b)] -> Bool
fstElem x = isJust . (lookup x)
 

-- work :: [a] -> (state -> a -> (state, [a]) -> state
-- work [] _ state     = state
-- work (x:xs) f state = let (state',xs') = f state x
--                       in work (xs' ++ xs) f state
