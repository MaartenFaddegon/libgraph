-- Copyright (c) Maarten Faddegon, 2015-2016
{-# LANGUAGE DeriveGeneric #-}

module Data.Graph.Libgraph.Core where
import Data.Maybe
import Data.List
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import GHC.Generics

--------------------------------------------------------------------------------
-- External representation of graphs

data Arc vertex arc 
  = Arc { source :: vertex, target :: vertex, arc :: arc}
    deriving (Eq, Show, Generic)

data SimpleArc vertex
  = SimpleArc { source' :: vertex, target' :: vertex }
    deriving Eq

data Graph vertex arc
  = Graph { root     :: vertex
          , vertices :: [vertex]
          , arcs     :: [Arc vertex arc]
          } deriving Generic

data SimpleGraph vertex
  = SimpleGraph { root'     :: vertex
                , vertices' :: [vertex]
                , arcs'     :: [SimpleArc vertex]
                }

-- | Create an arc between two vertices.
(-->) :: vertex -> vertex -> SimpleArc vertex
(-->) = SimpleArc

-- | Remove types from arcs.
simpleGraph :: Graph vertex arc -> SimpleGraph vertex
simpleGraph g = SimpleGraph (root g) (vertices g) (map simpleArc $ arcs g)
  where simpleArc (Arc v w _) = SimpleArc v w

unitGraph :: Graph vertex arc -> Graph vertex ()
unitGraph g = g { arcs = map unitArc (arcs g) }
  where unitArc a = a { arc = () }

--------------------------------------------------------------------------------
-- Successors and predecessors

-- | Direct successors of a vertex.
succs :: Eq vertex => Graph vertex arc -> vertex -> [vertex]
succs g v = map target $ filter ((== v) . source) (arcs g)


succCache :: Ord vertex => Graph vertex arc -> (vertex -> [vertex])
succCache g = \v -> case Map.lookup v m of 
                        Nothing -> []
                        (Just ws) -> ws

  where m = foldl' (\m' (Arc v w _) -> insertCon v w m') Map.empty (arcs g)

predCache :: Ord vertex => Graph vertex arc -> (vertex -> [vertex])
predCache g = \v -> case Map.lookup v m of 
                        Nothing -> []
                        (Just ws) -> ws

  where m = foldl' (\m' (Arc v w _) -> insertCon w v m') Map.empty (arcs g)
  

insertCon :: Ord k => k -> a -> Map k [a] -> Map k [a]
insertCon i x = Map.insertWith (\[x] xs->x:xs) i [x]

-- | Direct predecessors of a vertex.
preds :: Eq vertex => Graph vertex arc -> vertex -> [vertex]
preds g v = map source $ filter ((== v) . target) (arcs g)

-- | Is first vertex a successor of second?
isSucc :: Eq vertex => Graph vertex arc -> vertex -> vertex -> Bool
isSucc g w v = w `elem` succs g v

-- | Is first vertex a predecessor of second?
isPred :: Eq vertex => Graph vertex arc -> vertex -> vertex -> Bool
isPred g w v = w `elem` preds g v

--------------------------------------------------------------------------------
-- Graph conversion

mapGraph :: (a -> b) -> Graph a c -> Graph b c
mapGraph f (Graph r vs as) = Graph (f r) (map f vs) (mapArcsV f as)

mapArcs :: (a -> b) -> Graph c a -> Graph c b
mapArcs f (Graph r vs as) = Graph r vs (map (mapArc f) as)

mapArcsV :: (a -> b) -> [Arc a c] -> [Arc b c]
mapArcsV = map . mapArcV

mapArcV :: (a -> b) -> Arc a c -> Arc b c
mapArcV f (Arc src tgt t) = Arc (f src) (f tgt) t

mapArc :: (a->b) -> Arc v a -> Arc v b
mapArc f (Arc v w t) = Arc v w (f t)

filterArc :: (Arc vertex arc->Bool) -> Graph vertex arc -> Graph vertex arc
filterArc p (Graph r vs as) = Graph r vs (filter p as)

--------------------------------------------------------------------------------
-- Some other helper functions

nothingIsFalse :: Maybe Bool -> Bool
nothingIsFalse (Just b) = b
nothingIsFalse Nothing  = False

lookup' :: Eq a => a -> [(a, b)] -> String -> b
lookup' x ys msg = case lookup x ys of
  Nothing -> error msg
  Just y  -> y

fstElem :: Eq a => a -> [(a, b)] -> Bool
fstElem x = isJust . (lookup x)

update :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
update (x,y) xys = map (\(x',y') -> if x == x' then (x,y) else (x',y')) xys

sndList :: [(a,b)] -> [b]
sndList = snd . unzip

fstList :: [(a,b)] -> [a]
fstList = fst . unzip

---

treeDepth :: Ord v => Graph v a -> Int
treeDepth g = treeDepth' [] (root g)
  where suc = succCache g

        treeDepth' seen v
          | v `elem` seen = 1
          | otherwise     = case suc v of
             [] -> 0
             cs -> 1 + (maximum . map (treeDepth' (v:seen)) $ cs)

