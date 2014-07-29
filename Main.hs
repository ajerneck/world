{-# LANGUAGE NoMonomorphismRestriction, DataKinds, TemplateHaskell #-}

module Main where

import System.Random

import Matrix
import Types

import Criterion.Main


simulate iters g = last . take iters . iterate (iteration' g 0.4)

--main :: Int -> IO (Matrix Agent)
main = do
  g <- getStdGen
  let rows = 25
  let pc = 1/3
  let pop = floor $ pc * (fromIntegral rows^2)

  print pop
  let m = populate g [Agent Yellow, Agent Blue] pop rows

  putStrLn "Starting matrix: "
  putStrLn $ display m


  let m' = simulate 100 g m

  putStrLn "Finished matrix: "
  putStrLn $ display m'
  return m'



--main = defaultMain [bench "main' 10" $ \n -> main' (10+n-n)]
