module Data.Graph.Libgraph.DepthFirst
( Dfs
, EdgeType(..)
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

data Dfs vertex arc
  = Dfs { num       :: [(vertex,Int)]
        , lastVisit :: [(vertex,Int)] 
        , spanning  :: [SimpleArc vertex]
        , graph     :: Graph vertex arc
        }

data EdgeType = TreeEdge | BackEdge | FwdEdge | CrossEdge
  deriving Eq

-- | Is first vertex a (recursive) parent of second vertex?
--   May fail when one of the vertices is unreachable from the root.
isAncestor :: (Eq vertex, Show vertex)
           => Dfs vertex arc -> vertex -> vertex -> Maybe Bool
isAncestor d w v = do
  n_v <- lookup v (num d)
  n_w <- lookup w (num d)
  l_w <- lookup w (lastVisit d)
  return (n_w <= n_v && n_v <= l_w)

-- | The 'EdgeType' of an 'Arc'.
getEdgetype :: (Eq vertex, Show vertex) => Dfs vertex arc -> Arc vertex arc -> EdgeType
getEdgetype d (Arc v w _)
  | (v-->w) `elem` (spanning d) = TreeEdge
  | w `isAnc` v                 = BackEdge
  | v `isAnc` w                 = FwdEdge
  | otherwise                   = CrossEdge
  where isAnc x y = nothingIsFalse (isAncestor d x y)

-- | Get list of vertices in the order they were visited by the depth-first search.
getPreorder :: Dfs vertex arc -> [vertex]
getPreorder d = map fst (reverse . num $ d)

-- | Get list of vertices in the order they were last visited by the depth-first search.
getPostorder :: Dfs vertex arc -> [vertex]
getPostorder d = map fst (reverse . lastVisit $ d)

data Succs vertex = Succs vertex [vertex]

data DfsState vertex arc
  = DfsState { graph'     :: Graph vertex arc
             , spanning'  :: [SimpleArc vertex]
             , stack      :: [Succs vertex]
             , seen       :: [vertex]
             , time       :: Int
             , num'       :: [(vertex,Int)]
             , lastVisit' :: [(vertex,Int)]
             }

-- | Walk graph in depth-first order and number the vertices.
getDfs :: Eq vertex => Graph vertex arc -> Dfs vertex arc
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

visit :: Eq vertex => vertex -> State (DfsState vertex arc) ()
visit v = do see v
             pushSuccs v
             s <- gets stack
             mvw <- pop
             case mvw of Just (v,w) -> do addToSpanning v w
                                          visit w
                         Nothing    -> return ()

addToSpanning :: vertex -> vertex -> State (DfsState vertex arc) ()
addToSpanning v w 
  = modify $ \s -> s { spanning' = v --> w : (spanning' s) }

pushSuccs :: Eq vertex => vertex -> State (DfsState vertex arc) ()
pushSuccs v = do g  <- gets graph'
                 vs <- gets seen
                 modify $ \s -> s { stack = Succs v (succs g v) : (stack s) }

pop :: Eq vertex => State (DfsState vertex arc) (Maybe (vertex,vertex))
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

visitedAllChildren :: Eq vertex => vertex -> State (DfsState vertex arc) ()
visitedAllChildren v = modify $ \s -> s { lastVisit' = (v, time s) : lastVisit' s }

see :: vertex -> State (DfsState vertex arc) ()
see v = modify $ \s -> s { seen = v : seen s
                         , num' = (v, time s + 1) : num' s
                         , time = time s + 1
                         }

instance (Eq vertex,Show vertex) => Show (Dfs vertex arc) where
  show d = showWith (graph d) showVertex showArc
    where showVertex v = (show v ++ show (lkup v (num d), lkup v (lastVisit d)),"")
          showArc      = show . (getEdgetype d)
          lkup v ds    = lookup' v ds "Libgraph.show: lookup dfs number failed"

instance Show EdgeType where
  show TreeEdge  = "tree edge"
  show BackEdge  = "back edge"
  show FwdEdge   = "forward edge"
  show CrossEdge = "cross edge"
