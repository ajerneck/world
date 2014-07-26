{-# LANGUAGE NoMonomorphismRestriction, DataKinds #-}
module Matrix where

import Control.Applicative ((<$>), (<*>), liftA2)
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid
import System.Random

-- | Matrix datatypes

type Matrix a = M.Map Entry a

data Entry = Entry { row :: Row
                   , col :: Col
                   } deriving (Ord, Eq, Show)
type Row = Int
type Col = Int

-- | Agent datatypes
data Agent = Agent {color :: Color} deriving (Eq, Ord, Show)

data Color = Blue | Yellow deriving (Eq, Ord)
instance Show Color where
  show Blue = "B"
  show Yellow = "Y"

textRender (Just (Agent c)) = show c
textRender (Nothing) = " "

-- | Matrix creation
mkMatrix :: Row -> Col -> a -> M.Map Entry a
mkMatrix rows cols x = M.fromList $ zip (pairs rows cols) $ repeat x

pairs :: Row -> Col -> [Entry]
pairs rows cols = [ Entry i j | i <- [0..rows], j <- [0..cols]]

-- | Matrix updating
ncol = (+) 1 . col . last . M.keys
nrow = (+) 1 . row . last . M.keys
size x = nrow x * ncol x

updateEntries :: (Entry -> Bool) -> (a -> a) -> Matrix a -> Matrix a
updateEntries predicate updateFunc = M.mapWithKey (\k v -> if predicate k then updateFunc v else v)

-- | Rendering

--display :: Matrix a -> String
display x = concat $ intercalate ["\n"] $ map (intersperse " ") $ partition (nrow x) $ map textRender $ M.elems x

-- | Split a list into lists of the supplied length.
partition :: Int -> [a] -> [[a]]
partition n xs = go n xs where
  go n [] = []
  go n xs = p : (go n s) where
    (p, s) = splitAt n xs

-- | Random indices
randomIndices pc m = do
  rg <- getStdGen
  let
    s = size m
    r = nrow m
    idx = take (round $ pc * fromIntegral s) $ randomRs (0, s) rg
    ex = map (indexToEntry r) idx
    indexToEntry :: Int -> Int -> Entry
    indexToEntry r = uncurry Entry . flip divMod r
  return ex

-- | Simulation

-- --stay :: (Fractional n, Ord n) => Matrix Value -> n -> Value -> Bool
-- stay m lvl a = neighborSimilarity m a > lvl

-- --I AM HERE: I need to figure out how to handle the empty cells: right now it creates maybe maybe in focalColor, because we lookup a maybe value.


-- neighborSimilarity :: (Fractional a) => Matrix (Maybe Agent) -> Entry -> a
-- neighborSimilarity m e = sameColor / totalNeighbors where
--   sameColor = fromIntegral $ length $ filter (\x -> (liftA2 . liftA2) (==) (color x) focalColor) $ ns
--   totalNeighbors = fromIntegral $ length ns
--   ns = neighbors m e
--   focalColor = (fmap . fmap) color $ M.lookup e m

-- --neighbors :: Matrix (Maybe Agent) -> Entry -> [Agent]
-- neighbors m e = undefined


-- I AM HERE: implement the actual movement next: if percent of neighbors is of the same color, don't move, otherwise move. keep going (how can we define when it should stop? first start with just going for fixed number of iterations, then implement testing for convergence.)

-- | Agent handling

main = do
  let pc = 0.25
  let rows = 20
  let m = mkMatrix rows rows Nothing :: Matrix (Maybe Agent)

  xs <- randomIndices pc m
  let (bs, ys) = splitAt (length xs `div` 2) xs

  -- update it
  let m' = updateEntries (\k -> k `elem` ys) (\v -> Just $ Agent Yellow) $ updateEntries (\k -> k `elem` bs) (\v -> Just $ Agent Blue) m
  --let m' = updateEntries (\k -> k `elem` bs) (\v -> Just $ Agent Blue) m
  putStrLn $ display m'
  --return m'
  return m'
