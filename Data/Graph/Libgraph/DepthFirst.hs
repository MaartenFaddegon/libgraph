module Data.Graph.Libgraph.DepthFirst
( Dfs
, dfs
) where
import Data.Graph.Libgraph.Core
import Control.Monad.State
import Data.Maybe
import Data.List

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

