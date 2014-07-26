{-# LANGUAGE NoMonomorphismRestriction, DataKinds, TemplateHaskell #-}
module Matrix where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Lens
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid
import System.Random

-- | Matrix datatypes
data Entry = Entry { _row :: Row
                   , _col :: Col
                   } deriving (Ord, Eq, Show)
type Row = Int
type Col = Int
makeLenses ''Entry

data Matrix v = Matrix { _entries :: M.Map Entry v
                       , _nrow :: Int
                       , _ncol :: Int
                        } deriving (Show)
makeLenses ''Matrix

-- | Agent datatypes
data Agent = Agent {_color :: Color} deriving (Eq, Ord, Show)
data Color = Blue | Yellow deriving (Eq, Ord)
instance Show Color where
  show Blue = "B"
  show Yellow = "Y"
makeLenses ''Agent

textRender (Just (Agent c)) = show c
textRender (Nothing) = " "

-- | Matrix creation
--mkMatrix :: Row -> Col -> k -> v -> Matrix a
mkMatrix rows cols entries = Matrix (M.fromList entries) rows cols

-- | Matrix updating
-- ncol = (+) 1 . col . last . M.keys
-- nrow = (+) 1 . row . last . M.keys
size x = nrow x * ncol x

updateEntries :: (Entry -> Bool) -> (v -> v) -> Matrix v -> Matrix v
updateEntries predicate updateFunc m = over entries (M.mapWithKey (\k v -> if predicate k then updateFunc v else v)) m

-- | Rendering
display :: Matrix Agent -> String
display x = concat $ intercalate ["\n"] $ map (intersperse " ") $ partition (view nrow x) alls where
  alls = M.elems $ M.union filled empties
  empties = M.fromList [(Entry i j, ".") | i <- [0.. view nrow x], j <- [0.. view ncol x]]
  filled = M.map (show . view color) $ view entries x

-- | Split a list into lists of the supplied length.
partition :: Int -> [a] -> [[a]]
partition n xs = go n xs where
  go n [] = []
  go n xs = p : (go n s) where
    (p, s) = splitAt n xs

-- | Random indices
randomIndices pc r s= do
  rg <- getStdGen
  let
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
  --let m = mkMatrix rows rows Nothing :: Matrix Agent

  xs <- randomIndices pc rows (rows^2)
  let (ys, bs) = splitAt (length xs `div` 2) xs


  print $ length xs
  print $ length bs
  print $ length ys
  -- update it
  let m' = updateEntries (\k -> k `elem` ys) (\v -> Agent Yellow) $ mkMatrix rows rows $ zip bs $ repeat (Agent Blue)
  putStrLn $ display m'

  print $ M.size $ view entries m'
  print $ M.size $ M.filter (\v -> view color v == Yellow) $ view entries m'
  print $ M.size $ M.filter (\v -> view color v == Blue) $ view entries m'

  --return m'
  return m'
