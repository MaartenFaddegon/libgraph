module Data.Graph.Libgraph.Dot
( showWith
, escape
, display
) where
import Data.Graph.Libgraph.Core
import System.Process(runCommand)

-- | Convert Graph to String with functions to show vertices and arcs.
showWith :: Eq vertex => Graph vertex -> (vertex->String) -> (Arc vertex->String) -> String
showWith g vLabel aLabel
  = "diGraph G {\n"
  ++ "root [style=invis label=\"\"]\n"
  ++ foldl (\s v -> (showVertex vLabel v) ++ s) "" vs
  ++ "root -> " ++ vName (lookup' (root g) vs) ++ "\n"
  ++ foldl (\s a -> (showArc vs aLabel a) ++ s) "" (arcs g)
  ++ "}\n"
  where vs = zip (vertices g) [0..]

showVertex :: (vertex->String) -> (vertex,Int) -> String
showVertex vLabel (v,i) 
  = vName i ++ " [label=\"" ++ (escape . vLabel) v ++ "\"]\n"

showArc :: Eq vertex => [(vertex,Int)] -> (Arc vertex->String) -> (Arc vertex) -> String
showArc vs aLabel a
  = vName i ++ " -> " ++ vName j ++ " [label=\"" ++ (escape . aLabel) a ++ "\"]\n"
  where i = lookup' (source a) vs
        j = lookup' (target a) vs
        
vName :: Int -> String
vName i = "v" ++ show i

escape :: String -> String
escape []          = []
escape ('"' : ss)  = '\\' : '"' : escape ss
escape (s   : ss)  = s : escape ss

-- | Invoke Graphviz and Imagemagick to display graph on screen.
display :: (Graph vertex -> String) -> Graph vertex -> IO ()
display sh g = do 
  writeFile "/tmp/test.dot" (sh g)
  runCommand $ "cat /tmp/test.dot | dot -Tpng | display -"
  return ()
