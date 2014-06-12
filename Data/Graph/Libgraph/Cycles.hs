module Data.Graph.Libgraph.Cycles
( getCycleNest
, CycleNest
) where
import Data.List
import Control.Monad
import Control.Monad.State
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.DepthFirst
import qualified  Data.Graph.Libgraph.UnionFind as UF

type S vertex = State (CycleNest vertex) ()

data CycleType        = NonHeader | Self | Reducible | Irreducible
data CycleNest vertex = CycleNest
  { graph        :: Graph vertex
  , dfs          :: Dfs vertex
  , preorder     :: [vertex]
  , backPreds    :: [(vertex, [vertex])]
  , nonBackPreds :: [(vertex, [vertex])]
  , cycleType    :: [(vertex, CycleType)]
  , header       :: [(vertex, vertex)]
  , body         :: [vertex]              -- P in Havlak's algorithm
  , worklist     :: [vertex]
  , uf           :: UF vertex
  }

-- Step a and b of Havlak
state0 :: Eq vertex => Graph vertex -> CycleNest vertex
state0 g = state0'
  where ps      = map (\w -> partition (isAncestor (dfs state0') w) (preds g w)) (preorder state0')
        state0' = CycleNest
          { graph        = g
          , dfs          = getDfs g
          , preorder     = getPreorder (dfs state0')
          , backPreds    = zip (preorder state0') $ map fst ps
          , nonBackPreds = zip (preorder state0') $ map snd ps
          , cycleType    = zip (preorder state0') $ cycle [NonHeader]
          , header       = zip (preorder state0') $ cycle [root g]
          , body         = []
          , worklist     = []
          , uf           = UF.newPointSupply
          }

getCycleNest :: Eq vertex => Graph vertex -> CycleNest vertex
getCycleNest g = execState (analyse . reverse . preorder $ s0) s0
  where s0 = state0 g

-- Step c of Havlak
analyse :: Eq vertex => [vertex] -> S vertex
analyse ws = mapM_ analyseBackPreds ws
  where analyse' w = do modify $ \s -> s { body = [] }
                        analyseBackPreds w
                        modify $ \s -> s { worklist = body s }

labelReducible :: Eq vertex => vertex -> S vertex
labelReducible w = do p <- gets $ body
                      case p of [] -> modifyCycleType (w,Reducible)
                                _  -> return ()


-- Step d of Havlak
analyseBackPreds :: Eq vertex => vertex -> S vertex
analyseBackPreds w = do bps <- gets backPreds
                        mapM_ f (lookup' w bps)
  where f v = if v /= w then return () -- MF TODO: add FIND(v) to body
                        else modifyCycleType (w,Self)

modifyCycleType :: Eq vertex => (vertex,CycleType) -> S vertex
modifyCycleType vtyp = modify $ \s -> s { cycleType = update vtyp (cycleType s) }
