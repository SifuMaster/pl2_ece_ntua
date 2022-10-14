{-# OPTIONS_GHC -static -rtsopts -O2 -optc-O2 -XBangPatterns -fprof-auto -fexcess-precision -optc-march=native #-}

import Control.Exception (evaluate)
import Control.Monad (forM_)
import System.TimeIt (timeIt)

data Tree a = Node a [Tree a]
  deriving (Eq, Show, Read)


-- Add your solutions here!

leftish :: Int -> Tree Int
leftish 0 = Node 0 []
leftish n = Node n [leftish (n-1), Node n []]

mirror :: Tree a -> Tree a
mirror (Node n ks) = Node n (map mirror $ reverse ks)


-- fringe_naive :: Tree a -> [a]
-- fringe_naive t = aux [] t
--   where aux :: [a]->Tree a->[a] 
--         aux fr (Node n []) = fr ++ [n]
--         aux fr (Node _ ts) = fr'
--             where fr' = auxs fr ts
--         auxs fr [] = fr
--         auxs fr (t:[]) = aux fr t
--         auxs fr (t:ts) = fr''
--             where fr' = aux fr t
--                   fr'' = fr' ++ auxs fr ts



fringe_naive :: Tree a -> [a]
fringe_naive tr = aux [] tr
    where aux fr (Node n []) = fr ++ [n]
          aux fr (Node n (t:ts)) = (aux fr t) ++ concat (map (aux []) ts) 
-- Λόγω των concat που γίνονται σε κάθε επίπεδο, η πολυπλοκότητα είναι Ο(n^2),
-- όπου n τα φύλλα του δέντρου


fringe :: Tree a -> [a]
fringe tr = reverse (aux [] tr)
  where aux f (Node n []) = n : f
        aux f (Node n t) = auxs f t
        auxs f [] = f --just for pattern completeness
        auxs f (t:ts) = f''
          where f' = aux f t
                f'' = auxs f' ts 
-- Η πολυπλοκότητα είναι O(n), αφού εκμεταλλευόμενοι το laziness, 
-- η συνένωση όλων των φύλλων γίνεται μία μόνο φορά και έχουμε ένα reverse στο τέλος


same_fringe :: (Eq a) => Tree a -> Tree a -> Bool
same_fringe t1 t2 = (f1 == f2)
    where f1 = fringe t1
          f2 = fringe t2
-- Η πολυπλoκότητα της same_fringe είναι O(min (f1, f2)), αφού για την ισότητα πρέπει να ελγεχθούν ένα ένα τα στοιχεία των δύο λιστών, μέχρι
-- να βρεθεί κάποια διαφορά. f1, f2 είναι ουσιαστικά τα φύλλα των δύο δέντρων τα περιγράμματα των οποίων συγκρίνονται.
-- Αν τα περιγράμματα διαφέρουν η σύγκριση των δύο λιστών θα σταματήσει με το που βρεθεί το πρώτο διαφορετικό φύλλο.
-- Προφανώς για να μπορούν να συγκριθούν δύο περιγράμματα θα πρέπει να μπορούν να συγκριθούν τα περιεχόμενα των φύλλων,
-- για αυτό και περιορίζουμε τον τύπο της συνάρτησης με το typeclass Eq. 

-- Αν η haskell ήταν eager, δεν θα μπορούσαμε να έχουμε τα fringe σε O(n). Σε μία τέτοια περίπτωση (π.χ ML) θα μπορούσαμε να κάνουμε 
-- ταυτόχρονα ένα DFS traversal και στα δύο δέντρα συκγρίνοντας σειριακά κάθε ζευγάρι φύλλων από το ένα και το άλλο δέντρο που συναντάμε.

-- fibtree_naive :: Int -> Tree Int 
-- fibtree_naive 0 = (Node 0 [])
-- fibtree_naive 1 = (Node 1 [])
-- fibtree_naive n = (Node ((get_n c1) + (get_n c2)) [c1, c2])
--     where get_n :: Tree Int -> Int
--           get_n (Node n _) = n
--           c1 = fibtree_naive (n-1)
--           c2 = fibtree_naive (n-2)

fibtree_naive 0 = (Node 0 [])
fibtree_naive 1 = (Node 1 [])
fibtree_naive n = (Node (fibs!!n) [fibtree_naive (n-1), fibtree_naive (n-2)])
    where fibs :: [Int]
          fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- To Tn καταλαμβάνει μνήμη ανάλογη του πλήθους των κόμβων του δέντρου, δηλαδή
-- O(F(n+2) - 1), όπου  F(n) ο n-οστός αριθμός fibonacci.

fibtree :: Int -> Tree Int
fibtree 0 = (Node 0 [])
fibtree 1 = (Node 1 [])
fibtree n = fst(fibt!!n)
    where fibt :: [(Tree Int, Int)]
          fibt = ((Node 0 []), 0) : ((Node 1 []), 1) : zipWith (aux) fibt (tail fibt)
          aux ((Node (rn) _), ri) ((Node (ln) _), li) = ((Node (ln + rn) [fst(fibt!!li), fst(fibt!!ri)]), li+1)

-- Ακόμα και όταν αποτιμηθεί πλήρως η έκφραση mirror (fibtree n ) το δέντρο καταλαμβάνει και πάλι γραμμική μνήμη.
-- Αυτό συμβαίνει διότι κάθε κόμβος ουσιαστικά χρησιμοποιεί τη λίστα fibt που κατασκευάζει τους κόμβους με τέτοιον τρόπο
-- ώστε οι αναδρομικές κλήσεις να είναι ουσιαστικά αναφορές στις προηγούμενες θέσεις της λίστας. Συνολικά δηλαδή κάθε
-- κόμβος τάξης n θα κατασκευαστεί μία μοναδική φορά και θα αναπαριστά όλους τους όμοιους του κόμβους σε όποιο σημείο
-- του δέντρου και αν εμφανιστεί.

t = Node 'a' [ Node 'b' [ Node 'd' [Node 'i' []]
                        , Node 'e' [Node 'j' [], Node 'k' []]
                        ]
             , Node 'c' [ Node 'f' [Node 'l' [], Node 'm' []]
                        , Node 'g' []
                        , Node 'h' [Node 'n' []]
                        ]
             ]

tm = Node 'a' [ Node 'c' [ Node 'h' [ Node 'n' []]
                         , Node 'g' []
                         , Node 'f' [Node 'm' [], Node 'l' []]
                         ]
              , Node 'b' [ Node 'e' [Node 'k' [], Node 'j' []]
                         , Node 'd' [Node 'i' []]
                         ]
              ]

test_correctness msg testcases = do
  putStr $ msg ++ ": " ++ (if and testcases then "OK" else "FAIL!!!") ++ "\n"

test_complexity msg range f = forM_ range $ \n -> do
  putStr $ msg ++ " with size " ++ show n ++ ", "
  timeIt $ evaluate $ f n

main = do
  -- print(fibtree 5)
  test_correctness "mirror correctness" $
    [mirror t == tm] ++
    [mirror (mirror t) == t | n <- [0..100], let t = leftish n] 
    ++
    [mirror (mirror t) == t | n <- [0..15], let t = fibtree n]
  test_correctness "fringe_naive correctness" $
    [fringe_naive t == "ijklmgn"] ++
    [fringe_naive (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe_naive (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe_naive leftish" [100, 1000, 10000, 20000, 30000] $
    length . fringe_naive . leftish
  test_correctness "fringe correctness" $
    [fringe t == "ijklmgn"] ++
    [fringe (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe leftish" [100, 1000, 10000, 20000, 30000, 60000, 100000, 200000] $
    length . fringe . leftish
  test_correctness "same_fringe correctness" $
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]] ++
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]]
  test_complexity "mirror fibtree_naive" [20, 25, 30, 32] $ \n ->
    let t = fibtree_naive n in mirror (mirror t) == t
  test_complexity "mirror fibtree" [20, 25, 30, 32] $ \n ->
    let t = fibtree n in mirror (mirror t) == t
  test_complexity "same_fringe fibtree_naive" [20, 25, 30, 32] $ \n ->
    same_fringe (fibtree_naive n) (fibtree_naive (n+1))
  test_complexity "same_fringe fibtree" [20, 25, 30, 32, 34, 36] $ \n ->
    same_fringe (fibtree n) (fibtree (n+1))
