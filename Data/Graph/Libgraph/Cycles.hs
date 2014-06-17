module Data.Graph.Libgraph.Cycles
( CycleTree(..)
, getCycles
, CycleNest
, getCycleNest
, getRedHeaders
) where
import Data.List
import Control.Monad
import Control.Monad.State
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.DepthFirst
import Data.Graph.Libgraph.UnionFind(UF)
import Data.Graph.Libgraph.Dot
import qualified  Data.Graph.Libgraph.UnionFind as UF
import Data.Array

data CycleTree vertex = CycleTree vertex [CycleTree vertex]
                      | Reducible vertex [CycleTree vertex]
                      | Irreducible      [CycleTree vertex]
                      deriving Show

getCycles :: Ord vertex => CycleNest vertex -> CycleTree vertex
getCycles nest = cycleTree nest (children nest) 1

cycleTree :: CycleNest vertex -> Array Int [Int] -> Int -> CycleTree vertex
cycleTree nest cs x
  = case (vertexType nest) ! x of
    NonHead   -> CycleTree v cts
    SelfHead  -> Reducible v []
    RedHead   -> Reducible v cts
    IrredHead -> Irreducible ((CycleTree v []) : simplify cts)
    
    where cts  = map ct (cs ! x)
          ct c = cycleTree nest cs c
          v    = getVertex nest x

-- Flattens irreducible loops.
simplify :: [CycleTree vertex] -> [CycleTree vertex]
simplify vs = foldl simplify' [] vs
  where simplify' acc v = case v of
          (Irreducible ws) -> ws ++ acc
          _                -> v : acc

children :: CycleNest vertex -> Array Int [Int]
children nest
  = foldl add cs0 ps
  where cs0 = listArray (1,n nest) (cycle [[]])
        ps  = assocs (header nest)
        add cs (p,c) = if p == c then cs else cs // [(c,p : cs ! c)]


-- | Entry vertices of reducible cycles.
getRedHeaders :: CycleNest vertex -> [vertex]
getRedHeaders nest
  = map i2v (filter (isRedHead . snd) vtyps)
  where vtyps = assocs . vertexType $ nest 
        i2v   = (getVertex nest) . fst

isRedHead RedHead  = True
isRedHead SelfHead = True
isRedHead _        = False

-- Implementation of Havlaks algorithm.

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


-- Part a and b of Havlak's algorithm
state0 :: Ord vertex => Graph vertex -> CycleNest vertex
state0 g = s0
  where ps   = map (\w -> partition (isAncestor (dfs s0) w) (preds (graph s0) w)) [1..n s0]
        bps  = map fst ps
        nbps = map snd ps
        dfsg = dfsGraph g
        s0   = CycleNest
          { graph        = fst dfsg
          , getVertex    = snd dfsg
          , n            = length (vertices g)
          , dfs          = getDfs (graph s0)
          , backPreds    = bps
          , nonBackPreds = listArray (1,n s0) nbps
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
                      case p of [] -> return ()
                                _  -> modifyVertexType (w,RedHead)
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
  else if not (y' `elem` p) && y' /= w then do
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
modifyVertexType vtyp = do
  modify $ \s -> s { vertexType = (vertexType s) // [vtyp]}

addToNonBackPredsOf :: Int -> Int -> S vertex ()
addToNonBackPredsOf y w =
  modify $ \s -> s { nonBackPreds = (nonBackPreds s) // [(w,y : (nonBackPreds s) ! w)] }

addToBody :: Int -> S vertex ()
addToBody v = modify $ \s -> s { body = v : body s }

addWork :: Int -> S vertex ()
addWork v = modify $ \s -> s { worklist = v : worklist s }

uf_find :: Int -> S vertex Int
uf_find v = do 
  uf' <- gets uf
  let r =  UF.find uf' v
  -- MF TODO update uf?
  return r

uf_union :: Int -> Int -> S vertex ()
uf_union v w = modify $ \s -> s { uf = UF.union (uf s) v w }

-- Show

instance Show vertex => Show (CycleNest vertex) where
  show cycleNest
    =  "diGraph G {\n"
    ++ "rankdir=BT\n"
    ++ foldl (\s -> (s++) . showVType i2v)  "" (assocs . vertexType $ cycleNest)
    ++ foldl (\s -> (s++) . showHeader)     "" (assocs . header     $ cycleNest)
    ++ "}\n"
      where i2v = getVertex cycleNest

showVType :: Show vertex => (Int -> vertex) -> (Int,VertexType) -> String
showVType i2v (i,vtyp) 
  = "v" ++ show i ++ " [label=\"" ++ s (i2v i) ++ " (" ++ show i ++ " | " ++ show vtyp ++ ")\"]\n"
  where s = escape . show

showHeader :: (Int,Int) -> String
showHeader (i,j) = v i ++ " -> " ++ v j ++ "\n"
  where v x = "v" ++ show x
