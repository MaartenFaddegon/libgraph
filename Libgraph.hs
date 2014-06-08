module Libgraph
( Arc(..), Graph(..)
)
where

-- External representation of graphs

data Arc vertex   = Arc   { source :: vertex
                          , target :: vertex
                          }
data Graph vertex = Graph { root :: vertex
                          , vertices :: [vertex]
                          , arcs :: [Arc vertex]
                          }

--------------------------------------------------------------------------------
-- Some helper functions

children :: Eq vertex => Graph vertex -> vertex -> [vertex]
children g v = map target $ filter ((== v) . source) (arcs g)

--------------------------------------------------------------------------------
-- Depth first search

preorder :: Eq vertex => Graph vertex -> [vertex]
preorder g = dfs g [root g] []

dfs :: Eq vertex => Graph vertex -> [vertex] -> [vertex] -> [vertex]
dfs g [] vs        = vs
dfs g (v:stack) vs = if v `elem` vs
                     then dfs g stack vs
                     else dfs g (children g v ++ stack) (v:vs)

--------------------------------------------------------------------------------
-- 

