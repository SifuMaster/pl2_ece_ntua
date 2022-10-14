{-# OPTIONS_GHC -static -rtsopts -O2 -optc-O2 -XBangPatterns -fprof-auto -fexcess-precision -optc-march=native #-}

import qualified Data.Array as A
import qualified Data.Sequence as S

rInt :: String -> Int
rInt = read

                                                                                                           
solution :: Int -> String -> Int                                                                                    -- index 0 has length of array
solution nodes rest = solve $ readInput (words rest) 1 (A.array (1, nodes) [(i, (i, S.empty) ) |i <- [1..nodes]]) (A.array (0, nodes) [(0, nodes)])


readInput :: [String] -> Int -> A.Array Int (Int, S.Seq Int)-> A.Array Int Int -> (A.Array Int (Int, S.Seq Int), A.Array Int Int)   
readInput [] _ edges nodesCost = (edges, nodesCost)
readInput (nCost:nParent:rest) n edges nodesCost = readInput rest (n+1) edges' nodesCost'
                                                 where from = rInt nParent
                                                       to = n
                                                       edges' = if from /= 0 then edges A.// [(from, addToEdge), (to, addFromEdge)]
                                                                             else edges
                                                       addToEdge =  (fst fromElem, snd fromElem S.|> to)
                                                       fromElem = edges A.! from 
                                                       addFromEdge = (fst toElem, snd toElem S.|> from)
                                                       toElem = edges A.! to
                                                       nodesCost' = nodesCost A.// [(n, rInt nCost)]


getMaxBound :: Int -> Int
getMaxBound _ = maxBound 
solve (edges, nodesCost) = snd (foldl findBest (getMaxBound 0, 0) edges)
        where findBest (minCost, node) (n, ns) = let maxEdge = findMaxEdge n ns    
                                         in if maxEdge < minCost then (maxEdge, n) else (minCost, node)
              findMaxEdge n ns = foldl (edgeCost n) 0 ns 
              edgeCost from maxCost to = let edgeCost = cost from to 
                                         in if edgeCost > maxCost then edgeCost else maxCost 
              cost from to = (nodesCost A.! to) + foldl (sumCosts from to) 0 (snd (edges A.! to))
              sumCosts from to sum to' = if to' /= from then (costs A.! (to, to')) + sum
                                                 else sum 

              costs = A.listArray bounds 
                        [cost i j | (i, j) <- A.range bounds]
              bounds = ((0, 0), (nodes, nodes))
              nodes = nodesCost A.! 0

main = do
       n <- getLine 
       let nodes = rInt n
       rest <- getContents 
       let best = solution nodes rest
       print best