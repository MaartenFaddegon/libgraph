module Data.Graph.Libgraph.DepthFirst
( Dfs(num,lastVisit)
, dfs
, isAncestor
, edgetype
, preorder
, postorder
) where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Dot
import Control.Monad.State
import Data.Maybe
import Data.List

data Dfs vertex = Dfs { num       :: [(vertex,Int)]
                      , lastVisit :: [(vertex,Int)] 
                      , graph     :: Graph vertex
                      }

data EdgeType = TreeEdge | BackEdge | FwdEdge | CrossEdge

isAncestor :: Eq vertex => Dfs vertex -> vertex -> vertex -> Bool
isAncestor d v w = (n_w <= n_v && n_v <= l_w)
  where n_v    = lookup' v (num d)
        n_w    = lookup' w (num d)
        l_w    = lookup' w (lastVisit d)

edgetype :: Eq vertex => Dfs vertex -> Arc vertex -> EdgeType
edgetype d (Arc v w)
  | w `isAnc` v && w `isSuc` v = TreeEdge
  | w `isAnc` v                = FwdEdge
  | v `isAnc` w                = BackEdge
  | otherwise                  = CrossEdge
  where isAnc = isAncestor d
        isSuc = isSucc (graph d)

preorder :: Dfs vertex -> [vertex]
preorder d = map fst (num d)

postorder :: Dfs vertex -> [vertex]
postorder d = map fst (lastVisit d)

data Succs vertex = Succs vertex [vertex]

data DfsState vertex = DfsState { graph'     :: Graph vertex
                                , stack      :: [Succs vertex]
                                , seen       :: [vertex]
                                , time       :: Int
                                , num'       :: [(vertex,Int)]
                                , lastVisit' :: [(vertex,Int)]
                                }

dfs :: Eq vertex => Graph vertex -> Dfs vertex
dfs g = Dfs (num' finalState) (lastVisit' finalState) g
  where state0 = DfsState { graph'     = g
                          , stack      = []
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
pushSuccs v = do g  <- gets graph'
                 vs <- gets seen
                 modify $ \s -> s { stack = Succs v (succs g v \\ vs) : (stack s) }

pop :: Eq vertex => State (DfsState vertex) (Maybe vertex)
pop = do s <- gets stack
         case s of []                  -> return Nothing
                   (Succs v []:ss)     -> do modify $ \s -> s { stack = ss }
                                             visitedAllChildren v
                                             pop
                   (Succs v (c:cs):ss) -> do modify $ \s -> s { stack = Succs v cs : ss }
                                             return $ Just c

visitedAllChildren :: Eq vertex => vertex -> State (DfsState vertex) ()
visitedAllChildren v = modify $ \s -> s { lastVisit' = (v, time s) : lastVisit' s }

see :: vertex -> State (DfsState vertex) ()
see v = modify $ \s -> s { seen = v : seen s
                         , num' = (v, time s + 1) : num' s
                         , time = time s + 1
                         }

instance (Eq vertex,Show vertex) => Show (Dfs vertex) where
  show d = showWith (graph d) showVertex showArc
    where showVertex v = show (lookup' v (num d), lookup' v (lastVisit d))
          showArc    = show . (edgetype d)

instance Show EdgeType where
  show TreeEdge  = "tree edge"
  show BackEdge  = "back edge"
  show FwdEdge   = "forward edge"
  show CrossEdge = "cross edge"
