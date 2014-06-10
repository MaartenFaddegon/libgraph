module Data.Graph.Libgraph.Core where
import Data.Maybe
import Data.List
import Debug.Trace(traceStack)

--------------------------------------------------------------------------------
-- External representation of graphs

data Arc vertex   = Arc   { source :: vertex
                          , target :: vertex
                          } deriving Eq
data Graph vertex = Graph { root :: vertex
                          , vertices :: [vertex]
                          , arcs :: [Arc vertex]
                          }

(-->) :: vertex -> vertex -> Arc vertex
(-->) = Arc

--------------------------------------------------------------------------------
-- Successors and predecessors

succs :: Eq vertex => Graph vertex -> vertex -> [vertex]
succs g v = map target $ filter ((== v) . source) (arcs g)

preds :: Eq vertex => Graph vertex -> vertex -> [vertex]
preds g v = map source $ filter ((== v) . target) (arcs g)

isSucc :: Eq vertex => Graph vertex -> vertex -> vertex -> Bool
isSucc g w v = w `elem` succs g v

isPred :: Eq vertex => Graph vertex -> vertex -> vertex -> Bool
isPred g w v = w `elem` preds g v

--------------------------------------------------------------------------------
-- Some other helper functions

lookup' :: Eq a => a -> [(a, b)] -> b
lookup' x ys = case lookup x ys of
  Nothing -> traceStack "Lookup failed" undefined
  Just y  -> y

fstElem :: Eq a => a -> [(a, b)] -> Bool
fstElem x = isJust . (lookup x)

update :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
update (x,y) xys = map (\(x',y') -> if x == x' then (x,y) else (x',y')) xys
