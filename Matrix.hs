module Matrix where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Monoid
import System.Random

type Matrix a = M.Map Entry a

data Entry = Entry { row :: Row
                   , col :: Col
                   } deriving (Ord, Eq, Show)
type Row = Int
type Col = Int


mkMatrix ::  (Monoid a) =>  Row -> Col -> a -> M.Map Entry a
mkMatrix rows cols x = M.fromList $ zip (pairs rows cols) $ repeat $ (mempty x)

pairs :: Row -> Col -> [Entry]
pairs rows cols = [ Entry i j | i <- [0..rows], j <- [0..cols]]

ncol = (+) 1 . col . last . M.keys
nrow = (+) 1 . row . last . M.keys
size x = nrow x * ncol x

display :: (Show a) => Matrix a -> String
display x = concat $ intercalate ["\n"] $ map (intersperse " ") $ partition (nrow x) $ map show $ M.elems x

-- | Split a list into lists of the supplied length.
partition :: Int -> [a] -> [[a]]
partition n xs = go n xs where
  go n [] = []
  go n xs = p : (go n s) where
    (p, s) = splitAt n xs

-- | getting random indices
randomIndices pc m = do
  rg <- getStdGen
  let
    s = size m
    idx = map (indexToEntry s) $ take (round $ pc * fromIntegral s) $ randomRs (0, s) rg
    indexToEntry :: Int -> Int -> Entry
    indexToEntry s = uncurry Entry . flip divMod s
  return idx


-- I AM HERE:
-- updateMatrix func ks m = use mapWithKey to apply func to m if key is in ks
-- M.mapWithKey (\k v -> if k `elem` rps then updateFunc v else v) m


main = do
  let pc = 0.5
  let m = mkMatrix 4 4 (Sum 1) -- :: Matrix (Sum Int)


  rps <- randomIndices pc m
  print rps

  --

  -- Show the matrix
  print $ nrow m
  print $ ncol m
  putStrLn $ display m
