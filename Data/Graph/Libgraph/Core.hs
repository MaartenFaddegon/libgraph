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

-- | Create an arc between two vertices.
(-->) :: vertex -> vertex -> Arc vertex
(-->) = Arc

--------------------------------------------------------------------------------
-- Successors and predecessors

-- | Direct successors of a vertex.
succs :: Eq vertex => Graph vertex -> vertex -> [vertex]
succs g v = map target $ filter ((== v) . source) (arcs g)

-- | Direct predecessors of a vertex.
preds :: Eq vertex => Graph vertex -> vertex -> [vertex]
preds g v = map source $ filter ((== v) . target) (arcs g)

-- | Is first vertex a successor of second?
isSucc :: Eq vertex => Graph vertex -> vertex -> vertex -> Bool
isSucc g w v = w `elem` succs g v

-- | Is first vertex a predecessor of second?
isPred :: Eq vertex => Graph vertex -> vertex -> vertex -> Bool
isPred g w v = w `elem` preds g v

--------------------------------------------------------------------------------
-- Graph conversion

mapGraph :: (a -> b) -> Graph a -> Graph b
mapGraph f (Graph r vs as) = Graph (f r) (map f vs) (mapArcs f as)

mapArcs :: (a -> b) -> [Arc a] -> [Arc b]
mapArcs = map . mapArc

mapArc :: (a -> b) -> Arc a -> Arc b
mapArc f (Arc src tgt) = Arc (f src) (f tgt)

filterArc :: (Arc vertex->Bool) -> Graph vertex -> Graph vertex
filterArc p (Graph r vs as) = Graph r vs (filter p as)

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

sndList :: [(a,b)] -> [b]
sndList = snd . unzip

fstList :: [(a,b)] -> [a]
fstList = fst . unzip
