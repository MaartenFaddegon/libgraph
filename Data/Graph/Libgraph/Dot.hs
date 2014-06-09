module Data.Graph.Libgraph.Dot
( showWith
) where
import Data.Graph.Libgraph.Core

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

escape []          = []
escape ('"' : ss)  = '\\' : '"' : escape ss
escape (s   : ss)  = s : escape ss
