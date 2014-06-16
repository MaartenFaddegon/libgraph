import System.Process(runCommand)
import Data.Graph.Libgraph.Core
import Data.Graph.Libgraph.DepthFirst
import Data.Graph.Libgraph.Dominance
import Data.Graph.Libgraph.Cycles

type G = Graph String

a = "a"; b = "b"; c = "c"; d = "d"; e = "e"; f = "f"

graph1 = Graph a [a,b,c,d] [a-->b,b-->d,a-->c,c-->d]
graph2 = Graph a [a,b,c,d] [a-->b,b-->d,a-->c,c-->d,a-->d]
graph3 = Graph a [a,b,c,d] [a-->b,b-->d,a-->c,c-->d,d-->a]
graph4 = Graph a [a,b,c]   [a-->b,b-->c]

-- An reducible loop nested in an irreducible loop.
graph5 = Graph a [a,b,c,d] [a-->b,a-->c
                           ,b-->c,c-->b
                           ,c-->d,d-->c]

-- As graph5 but with the backedge to the reducible header removed.
graph6 = Graph a [a,b,c,d] [a-->b,a-->c
                           ,b-->c,c-->b
                           ,c-->d]

-- A graph with two nested irreducible loops.
graph7 = Graph a [a,b,c,d,e] [a-->b,b-->c,c-->d,d-->e
                             ,d-->c,e-->b
                             ,b-->d,a-->e]



test :: (G -> String) -> G -> IO ()
test sh g = do 
  writeFile "/tmp/test.dot" (sh g)
  runCommand $ "cat /tmp/test.dot | dot -Tpng | display -"
  return ()

dfsTest :: G -> IO ()
dfsTest = test (show . getDfs)

domTest :: G -> IO ()
domTest = test (show . getDomsets)

cycleTest :: G -> IO()
cycleTest = test (show . getCycleNest)
