{-# LANGUAGE LinearTypes #-}

module Main where

-- import qualified Criterion.Main as C
import Control.DeepSeq ( NFData, force )
import Data.List       ( sort )
import System.Mem      (performMajorGC)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Exception (evaluate)


import Gibbon.Prim ( allocRegionIO )
import Bintree
import qualified LinearBintree as L

--------------------------------------------------------------------------------

gB :: Int
{-# INLINE gB #-}
gB = 1000 * 1000 * 1000

main :: IO ()
main = bench_main

debug_main :: IO ()
debug_main = do
    out <- allocRegionIO (1*gB)
    let !_out1 = buildTree out 25
    -- _ <- printTree out
    let !(s,_) = sumTree out
    putStrLn ("sum: " ++ show s)
    pure ()

bench_main :: IO ()
bench_main = do
  out <- allocRegionIO (1*gB)

  putStrLn "buildTree/25"
  (_res,selftimed1,batchtime1) <- bench (buildTree out) 25
  putStrLn ("SELFTIMED: " ++ show selftimed1 ++ "\nBATCHTIME: " ++ show batchtime1)
  let (n,_) = sumTree out
  putStrLn $ "SUM: " ++ show n

  putStr "\n"

  -- let !_out1 = buildTree out 25
  putStrLn "sumTree/25"
  (res,selftimed2,batchtime2) <- bench sumTree out
  putStrLn ("SELFTIMED: " ++ show selftimed2 ++ "\nBATCHTIME: " ++ show batchtime2 ++ "\n" ++ show res)

  putStr "\n"

  putStrLn "buildTreeIO/25"
  (_res,selftimed1',batchtime1') <- benchIO (buildTreeIO out) 25
  putStrLn ("SELFTIMED: " ++ show selftimed1' ++ "\nBATCHTIME: " ++ show batchtime1')
  let (n',_) = sumTree out
  putStrLn $ "SUM: " ++ show n'

  putStr "\n"

  putStrLn "sumTreeIO/25"
  (res',selftimed2',batchtime2') <- benchIO sumTreeIO out
  putStrLn ("SELFTIMED: " ++ show selftimed2' ++ "\nBATCHTIME: " ++ show batchtime2' ++ "\n" ++ show res')


  putStr "\n"


  putStrLn "linear buildTree/25"
  (_res,selftimed3,batchtime3) <- bench (L.buildTree out) 25
  putStrLn ("SELFTIMED: " ++ show selftimed3 ++ "\nBATCHTIME: " ++ show batchtime3)
  let (n2,_) = sumTree out
  putStrLn $ "SUM: " ++ show n2

  putStr "\n"

  putStrLn "linear sumTree/25"
  (res2,selftimed4,batchtime4) <- bench L.sumTree out
  putStrLn ("SELFTIMED: " ++ show selftimed4 ++ "\nBATCHTIME: " ++ show batchtime4 ++ "\n" ++ show res2)


-- bench_main_criterion :: IO ()
-- bench_main_criterion = do
--     C.defaultMain
--       [
--         C.env (allocRegionIO (1*gB)) $ \ ~out -> C.bgroup "buildTree"
--              [ C.bench "25" $ C.whnf (buildTree out) 25 ]

--       , C.env (allocRegionIO (1*gB) >>= \out -> pure $ buildTree out 25) $ \ ~out1 -> C.bgroup "sumTree"
--              [ C.bench "25" $ C.nf sumTree out1 ]
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
