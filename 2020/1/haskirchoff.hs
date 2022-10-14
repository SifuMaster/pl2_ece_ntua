import qualified Data.Vector as V
import qualified Data.Map as Map

rInt :: String -> Int
rInt = read
 




solution :: (Num a, Ord a) => a -> (Int, Int)
solution nodes | nodes == 2 = (1,1)
               | nodes >  2 = V.foldl findMin (0, 0) (V.map maxEdge edge_cost)
                    where findMin (node1, cost1) (node2, cost2) | node1 == 0 = (node2, cost2)
                                                                | node2 == 0 = (node1, cost1)
                                                                | cost1 <= cost2 = (node1, cost1)
                                                                | cost1 >  cost2 = (node2, cost2) 
                          maxEdge edgeCosts = Map.foldl compare (0, 0) edgeCosts
                                where compare (node1, cost1) (node2, cost2) = if cost1 >= cost2 then (node1, cost1)
                                                                                                else (node2, cost2)

cost i j = Map.foldrWithKey filterSum (i, node_cost V.! j) (edge_cost V.! j)                                     
               where filterSum :: Int-> (Int, Int) -> (Int, Int) -> (Int, Int)
                     filterSum to cost (from, sum) = if to == from then (from, sum)
                                                                   else (from, sum + snd cost)   

node_cost :: V.Vector Int
node_cost = V.fromList [0, 10, 10, 10, 20, 20]
edge_cost :: V.Vector (Map.Map Int (Int, Int))
edge_cost = V.fromList [Map.fromList [(0, (False, (0, 0))], 
                        Map.fromList [(0, (0, 0))], 
                        Map.fromList [(0, (0, 0))], 
                        Map.fromList [(2, cost 3 2), (1, cost 3 1), (4, cost 3 4)],
                        Map.fromList [(3, cost 4 3), (5, cost 4 5)], 
                        Map.fromList [(0, (0, 0))]] 