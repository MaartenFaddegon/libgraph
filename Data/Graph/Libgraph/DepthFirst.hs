module Data.Graph.Libgraph.DepthFirst
( Dfs(num,lastVisit)
, getDfs
, getEdgetype
, getPreorder
, getPostorder
, isAncestor
) where
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.Dot
import Control.Monad.State
import Data.List

data Dfs vertex = Dfs { num       :: [(vertex,Int)]
                      , lastVisit :: [(vertex,Int)] 
                      , spanning  :: [Arc vertex]
                      , graph     :: Graph vertex
                      }

data EdgeType = TreeEdge | BackEdge | FwdEdge | CrossEdge

isAncestor :: Eq vertex => Dfs vertex -> vertex -> vertex -> Bool
isAncestor d v w = (n_w <= n_v && n_v <= l_w)
  where n_v    = lookup' v (num d)
        n_w    = lookup' w (num d)
        l_w    = lookup' w (lastVisit d)

getEdgetype :: Eq vertex => Dfs vertex -> Arc vertex -> EdgeType
getEdgetype d a@(Arc v w)
  | a `elem` (spanning d) = TreeEdge
  | w `isAnc` v           = FwdEdge
  | v `isAnc` w           = BackEdge
  | otherwise             = CrossEdge
  where isAnc = isAncestor d

getPreorder :: Dfs vertex -> [vertex]
getPreorder d = map fst (num d)

getPostorder :: Dfs vertex -> [vertex]
getPostorder d = map fst (lastVisit d)

data Succs vertex = Succs vertex [vertex]

data DfsState vertex = DfsState { graph'     :: Graph vertex
                                , spanning'  :: [Arc vertex]
                                , stack      :: [Succs vertex]
                                , seen       :: [vertex]
                                , time       :: Int
                                , num'       :: [(vertex,Int)]
                                , lastVisit' :: [(vertex,Int)]
                                }

getDfs :: Eq vertex => Graph vertex -> Dfs vertex
getDfs g = Dfs (num' finalState) (lastVisit' finalState) (spanning' finalState) g
  where state0 = DfsState { graph'     = g
                          , spanning'  = []
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
             mvw <- pop
             case mvw of Just (v,w) -> do addToSpanning v w
                                          visit w
                         Nothing    -> return ()

addToSpanning :: vertex -> vertex -> State (DfsState vertex) ()
addToSpanning v w 
  = modify $ \s -> s { spanning' = v --> w : (spanning' s) }

pushSuccs :: Eq vertex => vertex -> State (DfsState vertex) ()
pushSuccs v = do g  <- gets graph'
                 vs <- gets seen
                 modify $ \s -> s { stack = Succs v (succs g v) : (stack s) }

pop :: Eq vertex => State (DfsState vertex) (Maybe (vertex,vertex))
pop = do s <- gets stack
         case s of []                  -> return Nothing
                   (Succs v []:ss)     -> do modify $ \s -> s { stack = ss }
                                             visitedAllChildren v
                                             pop
                   (Succs v (c:cs):ss) 
                     -> do visited <- gets seen
                           modify $ \s -> s { stack = Succs v cs : ss }
                           if c `elem` visited 
                             then pop 
                             else do return $ Just (v,c)

visitedAllChildren :: Eq vertex => vertex -> State (DfsState vertex) ()
visitedAllChildren v = modify $ \s -> s { lastVisit' = (v, time s) : lastVisit' s }

see :: vertex -> State (DfsState vertex) ()
see v = modify $ \s -> s { seen = v : seen s
                         , num' = (v, time s + 1) : num' s
                         , time = time s + 1
                         }

instance (Eq vertex,Show vertex) => Show (Dfs vertex) where
  show d = showWith (graph d) showVertex showArc
    where showVertex v = show v ++ show (lookup' v (num d), lookup' v (lastVisit d))
          showArc    = show . (getEdgetype d)

instance Show EdgeType where
  show TreeEdge  = "tree edge"
  show BackEdge  = "back edge"
  show FwdEdge   = "forward edge"
  show CrossEdge = "cross edge"
