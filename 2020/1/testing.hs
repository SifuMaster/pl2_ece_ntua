{-# OPTIONS_GHC -static -rtsopts -O2 -optc-O2 -XBangPatterns -fprof-auto -fexcess-precision -optc-march=native #-}
import qualified Data.Vector as V
import qualified Data.Map as Map


rInt :: String -> Int 
rInt = read

inpPreparation :: String -> (V.Vector Int, V.Vector (Map.Map Int (Int, Int) ) )
inpPreparation inp = prepare $ words inp 

prepare :: [String] -> (V.Vector Int, V.Vector (Map.Map Int (Int, Int) ) )
prepare (n:inp) = help inp node node_cost_init edge_cost_init
                     where node = 1
                           node_cost_init = V.empty  
                           edge_cost_init = V.replicate (rInt n + 1) Map.empty






help :: [String] -> Int -> V.Vector Int -> V.Vector (Map.Map Int (Int, Int)) -> (V.Vector Int, V.Vector (Map.Map Int (Int, Int) ) )
help [] node node_cost edge_cost = (node_cost, edge_cost)
help (nodeCost:parent:rest) node node_cost edge_cost  = help rest (node + 1) (addNode node_cost (rInt nodeCost)) (addEdges edge_cost node (rInt parent))

import qualified Data.Vector as V
import qualified Data.Map as Map

addNode :: V.Vector Int -> Int -> V.Vector Int
addNode = V.snoc

addEdges :: V.Vector (Map.Map Int (Int, Int)) -> Int -> Int -> V.Vector (Map.Map Int (Int, Int))
addEdges edge_cost from to = addEdge (addEdge edge_cost from to) to from

addEdge :: V.Vector (Map.Map Int (Int, Int)) -> Int -> Int -> V.Vector (Map.Map Int (Int, Int))
addEdge edge_cost from to = edge_cost V.// [(from, checkEdges (edge_cost V.! from) from to)]

checkEdges :: Map.Map Int (Int, Int) -> Int -> Int -> Map.Map Int (Int, Int)
checkEdges entry from to
      | Map.size entry == 0 = if to /= 0 then Map.singleton 0 (from, to) else Map.empty 
      | Map.size entry == 1 = if to /= 0 then Map.insert to (from, to) (Map.singleton to' (from', to')) else entry
      | otherwise = if to /= 0 then Map.insert to (from, to) entry else entry
            where pair = entry Map.! 0
                  from' = fst pair
                  to'   = snd pair





main = do
       all <- getContents
       let (node_cost, edge_cost) = inpPreparation all
       print (node_cost V.! 0)
       


-- cost :: Int -> Int -> (Int, Int)
-- cost i j = Map.foldrWithKey filterSum (i, node_cost V.! j) (edge_cost V.! j)                                     
--             where filterSum :: Int-> (Int, Int) -> (Int, Int) -> (Int, Int)
--                   filterSum to cost (from, sum) = if to == from then (from, sum)
--                                                       else (from, sum + snd cost)   
