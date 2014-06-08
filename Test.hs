import System.Process(system)
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.DepthFirst

graph1 = Graph a [a,b,c,d] [a-->b,b-->d,a-->c,c-->d]
  where a = "a"; b = "b"; c = "c"; d = "d"

graph2 = Graph a [a,b,c,d] [a-->b,b-->d,a-->c,c-->d,a-->d]
  where a = "a"; b = "b"; c = "c"; d = "d"

dfsTest g = do writeFile "/tmp/test.dot" (show . dfs $ g)
               system $ "cat /tmp/test.dot | dot -Tpng | display -"

main = dfsTest graph2
