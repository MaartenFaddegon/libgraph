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

data VertexType = NonHead | SelfHead | RedHead | IrredHead
  deriving Show

data CycleNest vertex = CycleNest
  { graph        :: Graph Int
  , getVertex    :: Int -> vertex
  , n            :: Int
  , dfs          :: Dfs Int
  , backPreds    :: [[Int]]
  , nonBackPreds :: Array Int [Int]
  , vertexType   :: Array Int VertexType
  , header       :: Array Int Int
  , body         :: [Int]              -- P in Havlak's algorithm
  , worklist     :: [Int]
  , uf           :: UF
  }

instance Show (CycleNest vertex) where
  show cycleNest
    =  "diGraph G {\n"
    ++ "rankdir=BT\n"
    ++ foldl (\s -> (s++) . showVType)  "" (assocs . vertexType $ cycleNest)
    ++ foldl (\s -> (s++) . showHeader) "" (assocs . header     $ cycleNest)
    ++ "}\n"

showVType :: (Int,VertexType) -> String
showVType (i,vtyp) = "v" ++ show i ++ " [label=\"" ++ show vtyp ++ "\"]\n"

showHeader :: (Int,Int) -> String
showHeader (i,j) = v i ++ " -> " ++ v j ++ "\n"
  where v x = "v" ++ show x

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
          , vertexType   = listArray (1,n s0) $ cycle [NonHead]
          , header       = listArray (1,n s0) $ cycle [root . graph $ s0]
          , body         = []
          , worklist     = []
          , uf           = UF.fromList [1..n s0]
          }

getCycleNest :: Ord vertex => Graph vertex -> CycleNest vertex
getCycleNest g = execState (analyse . reverse $ [1..n s0]) s0
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
                      case p of [] -> modifyVertexType (w,RedHead)
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
                        mapM_ f (bps !!! w)
  where f v = if v /= w then do x <- uf_find v
                                addToBody x
                        else modifyVertexType (w,SelfHead)

(!!!) :: [a] -> Int -> a
xs !!! i = xs !! (i-1)



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
    modifyVertexType (w,IrredHead)
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

modifyVertexType :: (Int,VertexType) -> S vertex ()
modifyVertexType vtyp = modify $ \s -> s { vertexType = (vertexType s) // [vtyp]}

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

