module Data.Graph.Libgraph.Dot
( showWith
, escape
, display
) where
import Data.Graph.Libgraph.Core
import System.Process(runCommand)

-- | Convert Graph to String with functions to show vertices and arcs.
showWith :: Eq vertex => Graph vertex arc -> (vertex->(String,String)) 
                          -> (Arc vertex arc->String) -> String
showWith g vLabel aLabel
  = "diGraph G {\n"
  -- ++ vName r ++ "[shape=none label=\".\"]\n"
  ++ foldl (\s v -> show1 v ++ s) "" vs
  ++ foldl (\s a -> (showArc vs aLabel a) ++ s) "" (arcs g)
  ++ "}\n"
  where vs = zip (vertices g) [0..]
        r  = lookup' (root g) vs "LibGraph.showWith: lookup root in vs failed"
        isRoot (v,_) = v == root g

        show1 v
          -- | isRoot v  = vName r ++ "[shape=none label=\".\"]\n"
          | otherwise = (showVertex vLabel v)

showVertex :: (vertex->(String,String)) -> (vertex,Int) -> String
showVertex vLabel (v,i) =
  let (lbl,extra) = vLabel v
  in  vName i ++ " [label=" ++ lbl ++ "" ++ extra ++ "]\n"
  -- in  vName i ++ " [label=\"" ++ escape lbl ++ "\"" ++ extra ++ "]\n"

showArc :: Eq vertex => [(vertex,Int)] -> (Arc vertex arc->String) -> (Arc vertex arc) -> String
showArc vs aLabel a
  = vName i ++ " -> " ++ vName j ++ " [label=\"" ++ (escape . aLabel) a ++ "\"]\n"
  where i = lookup' (source a) vs $ "LibGraph.showArc: lookup source failed" 
        j = lookup' (target a) vs $ "LibGraph.showArc: lookup target failed"
        
vName :: Int -> String
vName i = "v" ++ show i

escape :: String -> String
escape [] = []
escape ss = escape' False ss

escape' _ []                          = []
escape' _ ('"'  : ss)                 = '\\' : '"'  : escape' False ss
escape' _ ('\\' : ss)                 = '\\' : '\\' : escape' False ss
escape' w (s    : ss)
 | w       && (s == ' ' || s == '\n') =               escape' True  ss
 | (not w) && (s == ' ' || s == '\n') =         ' ' : escape' True  ss
 | otherwise                          =         s   : escape' False ss


-- | Invoke Graphviz and Imagemagick to display graph on screen.
display :: (Graph vertex arc -> String) -> Graph vertex arc -> IO ()
display sh g = do 
  writeFile "/tmp/test.dot" (sh g)
  runCommand $ "cat /tmp/test.dot | dot -Tpng | display -"
  return ()
