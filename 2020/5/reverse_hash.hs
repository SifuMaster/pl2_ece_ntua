import Control.Concurrent
import Control.Parallel.Strategies
import Control.DeepSeq (force)
import System.Random
import System.Timeout
import Data.Hashable (hashWithSalt)
import Data.List (nub)
import Data.Time
import Data.Maybe
import qualified Data.Set as S
import Text.Printf (printf)

#include "./reverse_hash_imports.hs"

gen_salt :: IO Int
gen_salt = do
  g <- getStdGen
  return . head $ (randoms g :: [Int])

gen_hash :: IO (Int -> Int)
gen_hash = do
  salt <- gen_salt
  return $ hashWithSalt salt

gen_index :: IO [Int]
gen_index = do
  g <- getStdGen
  return $ randomRs (1, 2^27-1) g

gen_inputs :: (Int -> Int) -> IO [Int]
gen_inputs hash = do
  indices <- gen_index
  return $ take (2^16) $ nub $ map hash indices

type Pair = (Int, Int)

type Range = (Int, Int)
group :: Int -> Range -> [Range]
group n (start, end) = group' (start, end) []
  where
  step = (end - start) `div` n

  group' (start, end) acc
    | step >= (end - start + 1) = acc ++ [(start, end)]
    | otherwise = group' (start+step, end) $ acc ++ [(start, start+step-1)]

seq_solve :: (Int -> Int) -> [Int] -> [Pair]
seq_solve hash inputs =
  solve' set (1, 2^27-1) []
  where
    solve' set (start,end) acc
      | S.null set || start > end = acc
      | S.member (hash start) set =
          let
            new_set = S.delete (hash start) set
            new_acc = ((hash start), start):acc
          in
            solve' new_set (start+1,end) new_acc
      | otherwise = solve' set (start+1,end) acc
    set = S.fromList inputs

validate :: (Int -> Int) -> [Int] -> [Pair] -> Int
validate hash inputs pairs =
  validate' (S.fromList inputs) 0 pairs
  where
    validate' set acc [] = acc
    validate' set acc ((a,b):rest)
      | S.member (hash b) set  && a == hash b =
        let
          new_set = S.delete (hash b) set
          new_acc = acc + 1
        in
          validate' new_set new_acc rest
      | otherwise = validate' set acc rest

tc :: IO t -> IO NominalDiffTime
tc x = do
  start <- getCurrentTime
  _ <- x
  stop <- getCurrentTime
  return $ diffUTCTime stop start

base_speed :: (Int -> Int) -> [Int] -> IO Double
base_speed hash inputs = do
  let pairs = seq_solve hash inputs
  time <- tc $ force pairs `seq` return ()
  let found = fromIntegral $ validate hash inputs pairs
  return $ found / (realToFrac time)

get_speed :: ((Int -> Int) -> [Int] -> Int -> [Pair])
  -> (Int -> Int)
  -> [Int]
  -> Int
  -> IO Double
get_speed solver hash inputs schedulers = do
  let pairs = solver hash inputs schedulers
  time <- tc $ force pairs `seq` return ()
  let found = fromIntegral $ validate hash inputs pairs
  return $ found / (realToFrac time)

get_score_par :: ((Int -> Int) -> [Int] -> MVar () -> MVar [Pair] -> Int -> IO ())
  -> Int -> IO ()
get_score_par solver s = do
  hash <- gen_hash
  inputs <- gen_inputs hash
  signal <- newEmptyMVar
  box <- newEmptyMVar
  force inputs `seq` return ()

  base <- base_speed hash inputs
  forkIO (solver hash inputs signal box s)
  let time = 2
  threadDelay (time*1000*1000)
  tryPutMVar signal ()
  ret <- timeout (1000*1000) $ takeMVar box
  let pairs = if isJust ret then fromJust ret else []
  let found = fromIntegral $ validate hash inputs pairs
  para <- return $ found / (realToFrac time)
  printf "For %d scheduler(s) score: %.2f\n" s ((para / base) / fromIntegral s)

get_score_conc :: ((Int -> Int) -> [Int] -> Int -> [Pair]) -> Int -> IO ()
get_score_conc solver s = do
  hash <- gen_hash
  inputs <- gen_inputs hash
  force inputs `seq` return ()

  base <- base_speed hash inputs
  let pairs = solver hash inputs s
  time <- tc $ force pairs `seq` return ()
  let found = fromIntegral $ validate hash inputs pairs
  para <- return $ found / (realToFrac time)
  printf "For %d scheduler(s) score: %.2f\n" s ((para / base) / fromIntegral s)

-- #include "./reverse_hash_solver_par.hs"
-- #include "./reverse_hash_solver_conc.hs"

main = do
  mapM_ (\s -> get_score_par solver_par s) [1,2,4,8]
  -- mapM_ (\s -> get_score_conc solver_conc s) [1,2,4,8]
