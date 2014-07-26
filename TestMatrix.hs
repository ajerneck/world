{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
module TestMatrix where

import Test.QuickCheck
import Test.QuickCheck.Monadic


import Matrix

closeMatch :: (Num a, Ord a) => a -> a -> Bool
closeMatch a b = (abs a - b) <= 1

prop_sample_lengths :: RealFrac a => a -> Int -> Property
prop_sample_lengths pc rows = monadicIO $ do
  (ys, bs) <- run $ randomPop pc rows (rows^2) 2
  assert $ (abs $ (length ys) - (length bs)) <= 1

prop_matrix_agent_numbers :: RealFrac a => a -> Int -> Property
prop_matrix_agent_numbers pc rows = monadicIO $ do
  (ys, bs) <- run $ randomPop pc rows (rows^2) 2
  let m = mkMatrix rows rows $ (zip ys $ repeat (Agent Yellow))  ++ (zip bs $ repeat (Agent Blue))
  let ys' = agents m Yellow
  let bs' = agents m Blue
  assert $ closeMatch ys' bs'
  assert $ length ys == length ys

deepCheck p = quickCheckWith (stdArgs { maxSuccess = 1000}) p

-- -- To run all tests.
-- -- see https://hackage.haskell.org/package/QuickCheck-2.7.6/docs/Test-QuickCheck.html#g:3
-- return []
-- runTests = $quickCheckAll
