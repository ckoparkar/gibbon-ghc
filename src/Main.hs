{-# LANGUAGE LinearTypes #-}

module Main where

-- import Criterion.Main
import Control.DeepSeq ( NFData, force )
import Data.List       ( sort )
import System.Mem      (performMajorGC)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Exception (evaluate)


import Gibbon.Prim ( allocRegion )
import Bintree

--------------------------------------------------------------------------------

gB :: Int
{-# INLINE gB #-}
gB = 1000 * 1000 * 1000

main :: IO ()
main = bench_main

debug_main :: IO ()
debug_main = do
    out <- allocRegion (1*gB)
    let !_out1 = buildTree out 25
    -- _ <- printTree out
    let !(s,_) = sumTree out
    putStrLn ("sum: " ++ show s)
    pure ()

bench_main :: IO ()
bench_main = do
  out <- allocRegion (1*gB)

  putStrLn "buildTree/25"
  (_res,selftimed1,batchtime1) <- bench (buildTree out) 25
  putStrLn ("SELFTIMED: " ++ show selftimed1 ++ "\nBATCHTIME: " ++ show batchtime1)
  let (n,_) = sumTree out
  putStrLn $ "SUM: " ++ show n

  -- let !_out1 = buildTree out 25
  putStrLn "sumTree/25"
  (res,selftimed2,batchtime2) <- bench sumTree out
  putStrLn ("SELFTIMED: " ++ show selftimed2 ++ "\nBATCHTIME: " ++ show batchtime2 ++ "\n" ++ show res)

-- bench_main_criterion :: IO ()
-- bench_main_criterion = do
--     defaultMain
--       [
--         env (allocRegion (1*gB)) $ \ ~out -> bgroup "buildTree"
--              [ bench "25" $ nfAppIO (buildTree out) 25 ]

--       , env (allocRegion (1*gB) >>= \out -> buildTree out 25) $ \ ~out1 -> bgroup "sumTree"
--              [ bench "25" $ nfAppIO sumTree out1 ]
--       ]

--------------------------------------------------------------------------------

bench :: (NFData a, NFData b) => (a %n-> b) -> a -> IO (b, Double, Double)
bench f arg = do
    let !arg2 = force arg
        iters = 9
    (results, times) <- unzip <$> mapM (\(_::Int) -> dotrial f arg2) [1..iters]
    let  selftimed = median times
         batchtime = sum times
    return $! (last results, selftimed, batchtime)

dotrial :: (NFData a, NFData b) => (a %n-> b) -> a -> IO (b, Double)
{-# NOINLINE dotrial #-}
dotrial f arg = do
    performMajorGC
    t1 <- getCurrentTime
    !a <- evaluate (f arg)
    t2 <- getCurrentTime
    let delt = fromRational (toRational (diffUTCTime t2 t1))
    putStrLn ("iter time: " ++ show delt)
    return $! (a,delt)


benchIO :: (NFData a, NFData b) => (a %n-> IO b) -> a -> IO (b, Double, Double)
benchIO f arg = do
    let !arg2 = force arg
        iters = 9
    (results, times) <- unzip <$> mapM (\(_::Int) -> dotrialIO f arg2) [1..iters]
    let  selftimed = median times
         batchtime = sum times
    return $! (last results, selftimed, batchtime)

dotrialIO :: (NFData a, NFData b) => (a %n-> IO b) -> a -> IO (b, Double)
{-# NOINLINE dotrialIO #-}
dotrialIO f arg = do
    performMajorGC
    t1 <- getCurrentTime
    !a <- f arg
    t2 <- getCurrentTime
    let delt = fromRational (toRational (diffUTCTime t2 t1))
    putStrLn ("iter time: " ++ show delt)
    return $! (a,delt)

median :: [Double] -> Double
median ls = (sort ls) !! (length ls `div` 2)
