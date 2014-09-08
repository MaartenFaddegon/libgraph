module Data.Graph.Libgraph.Dot
( showWith
, escape
, display
) where
import Data.Graph.Libgraph.Core
import System.Process(runCommand)

-- | Convert Graph to String with functions to show vertices and arcs.
showWith :: Eq vertex => Graph vertex arc -> (vertex->String) -> (Arc vertex arc->String) -> String
showWith g vLabel aLabel
  = "diGraph G {\n"
  ++ "root [style=invis label=\"\"]\n"
  ++ foldl (\s v -> (showVertex vLabel v) ++ s) "" vs
  ++ "root -> " ++ vName r ++ "\n"
  ++ foldl (\s a -> (showArc vs aLabel a) ++ s) "" (arcs g)
  ++ "}\n"
  where vs = zip (vertices g) [0..]
        r  = lookup' (root g) vs "LibGraph.showWith: lookup root in vs failed"

showVertex :: (vertex->String) -> (vertex,Int) -> String
showVertex vLabel (v,i) 
  = vName i ++ " [label=\"" ++ (escape . vLabel) v ++ "\"]\n"

showArc :: Eq vertex => [(vertex,Int)] -> (Arc vertex arc->String) -> (Arc vertex arc) -> String
showArc vs aLabel a
  = vName i ++ " -> " ++ vName j ++ " [label=\"" ++ (escape . aLabel) a ++ "\"]\n"
  where i = lookup' (source a) vs $ "LibGraph.showArc: lookup source failed" 
        j = lookup' (target a) vs $ "LibGraph.showArc: lookup target failed"
        
vName :: Int -> String
vName i = "v" ++ show i

escape :: String -> String
escape []          = []
escape ('"' : ss)  = '\\' : '"'   : escape ss
escape ('\\' : ss)  = '\\' : '\\' : escape ss
escape (s   : ss)  = s : escape ss

-- | Invoke Graphviz and Imagemagick to display graph on screen.
display :: (Graph vertex arc -> String) -> Graph vertex arc -> IO ()
display sh g = do 
  writeFile "/tmp/test.dot" (sh g)
  runCommand $ "cat /tmp/test.dot | dot -Tpng | display -"
  return ()
