module Main where


import Criterion.Main
import System.Random

import Matrix


main = newStdGen >>= defaultMain . benchmarks

benchFun g f rows = last $ take 10 $ iterate (f g 0.4) m where
  m = populate g [Agent Yellow, Agent Blue] pop rows
  pop = floor $ pc * (fromIntegral rows^2)
  pc = 1/3


benchmarks gen =
  [
  bgroup "Group"
     [
       bench "old" $ whnf (benchFun gen iteration) 10
     , bench "new" $ whnf (benchFun gen iteration') 10
     ]
  ]
