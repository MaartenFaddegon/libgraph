module Data.Graph.Libgraph.Cycles
( getCycleNest
, CycleNest
) where
import Data.List
import Control.Monad
import Control.Monad.State
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.DepthFirst
import Data.Graph.Libgraph.UnionFind(UF)
import qualified  Data.Graph.Libgraph.UnionFind as UF
import Data.Array

type S vertex a = State (CycleNest vertex) a

data CycleType        = NonHeader | Self | Reducible | Irreducible
data CycleNest vertex = CycleNest
  { graph        :: Graph Int
  , getVertex    :: Int -> vertex
  , n            :: Int
  , dfs          :: Dfs Int
  , backPreds    :: [[Int]]
  , nonBackPreds :: Array Int [Int]
  , cycleType    :: Array Int CycleType
  , header       :: Array Int Int
  , body         :: [Int]              -- P in Havlak's algorithm
  , worklist     :: [Int]
  , uf           :: UF
  }

-- Part a and b of Havlak's algorithm
state0 :: Ord vertex => Graph vertex -> CycleNest vertex
state0 g = s0
  where ps   = map (\w -> partition (isAncestor (dfs s0) w) (preds (graph s0) w)) [1..n s0]
        dfsg = dfsGraph g
        s0   = CycleNest
          { graph        = fst dfsg
          , getVertex    = snd dfsg
          , n            = length (vertices g)
          , dfs          = getDfs (graph s0)
          , backPreds    = map fst ps
          , nonBackPreds = listArray (1,n s0) $ map snd ps
          , cycleType    = listArray (1,n s0) $ cycle [NonHeader]
          , header       = listArray (1,n s0) $ cycle [root . graph $ s0]
          , body         = []
          , worklist     = []
          , uf           = UF.fromList [1..n s0]
          }

getCycleNest :: Ord vertex => Graph vertex -> CycleNest vertex
getCycleNest g = execState (analyse [n s0..1]) s0
  where s0 = state0 g

-- Part c of Havlak's algorithm
analyse :: Eq vertex => [Int] -> S vertex ()
analyse ws = mapM_ analyse' ws
  where analyse' w = do modify $ \s -> s { body = [] }
                        analyseBackPreds w
                        modify $ \s -> s { worklist = body s }
                        labelReducible w
                        work w
                        merge w


labelReducible :: Eq vertex => Int -> S vertex ()
labelReducible w = do p <- gets $ body
                      case p of [] -> modifyCycleType (w,Reducible)
                                _  -> return ()
work :: Int -> S vertex ()
work w = do
  wl <- gets worklist
  case wl of
    []      -> return ()
    (x:wl') -> do modify $ \s -> s { worklist = wl' }
                  chase w x
                  work w

merge :: Int -> S vertex ()
merge w = do
  p <- gets body
  mapM_ (merge' w) p

merge' w x = do
  modify $ \s -> s { header = (header s) // [(x,w)] }
  uf_union x w

-- Part d of Havlak's algorithm
analyseBackPreds :: Eq vertex => Int -> S vertex ()
analyseBackPreds w = do bps <- gets backPreds
                        mapM_ f (bps !! w)
  where f v = if v /= w then do x <- uf_find v
                                addToBody x
                        else modifyCycleType (w,Self)



-- Part e of Havlak's algorithm

chase :: Int -> Int -> S vertex ()
chase w x = do
  nbps <- gets nonBackPreds
  mapM_ (chase' w) (nbps ! x)

chase' :: Int -> Int -> S vertex ()
chase' w y = do
  y' <- uf_find y
  d  <- gets dfs
  p  <- gets body
  if not $ isAncestor d w y' then do
    modifyCycleType (w,Irreducible)
    y' `addToNonBackPredsOf` w
  else if not (y' `elem` p) && not (y' /= w) then do
    addToBody y'
    addWork y'
  else
    return ()

-- Some helper functions

dfsGraph :: Ord vertex => Graph vertex -> (Graph Int, Int -> vertex)
dfsGraph g = (mapGraph v2i g, i2v)
  where preorder = getPreorder (getDfs g)
        i2v i = lookup' i (zip [1..] preorder)
        v2i v = lookup' v (zip preorder [1..])

modifyCycleType :: (Int,CycleType) -> S vertex ()
modifyCycleType vtyp = modify $ \s -> s { cycleType = (cycleType s) // [vtyp]}

addToNonBackPredsOf :: Int -> Int -> S vertex ()
addToNonBackPredsOf y w =
  modify $ \s -> s { nonBackPreds = (nonBackPreds s) // [(w,y : (nonBackPreds s) ! w)] }

addToBody :: Int -> S vertex ()
addToBody v = modify $ \s -> s { body = v : body s }

addWork :: Int -> S vertex ()
addWork v = modify $ \s -> s { worklist = v : worklist s }

uf_find :: Int -> S vertex Int
uf_find v = do uf' <- gets uf
               let r = UF.find uf' v
               -- MF TODO update uf?
               return r

uf_union :: Int -> Int -> S vertex ()
uf_union v w = modify $ \s -> s { uf = UF.union (uf s) v w }

