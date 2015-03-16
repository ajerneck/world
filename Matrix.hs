{-# LANGUAGE NoMonomorphismRestriction, DataKinds, TemplateHaskell #-}
module Matrix where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Lens
import Data.List (intercalate, intersperse, delete, (\\), find)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Monoid
import qualified Data.Set as S
import System.Random
import System.Random.Shuffle

-- import Debug.Trace (trace)

-- | Matrix datatypes
data Entry = Entry { _row :: Int
                   , _col :: Int
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
mkMatrix rows cols entries = Matrix (M.fromList entries) rows cols

-- | Matrix updating
-- ncol = (+) 1 . col . last . M.keys
-- nrow = (+) 1 . row . last . M.keys
size x = nrow x * ncol x

-- updateEntries :: (Entry -> Bool) -> (v -> v) -> Matrix v -> Matrix v
-- updateEntries predicate updateFunc m = over entries (M.mapWithKey (\k v -> if predicate k then updateFunc v else v)) m

-- | Rendering
display :: Matrix Agent -> String
display x = concat $ intercalate ["\n"] $ map (intersperse " ") $ partition (view nrow x + 1) alls where
  alls = M.elems $ M.union filled empties
  empties = M.fromList [(Entry i j, ".") | i <- [0.. view nrow x], j <- [0.. view ncol x]]
  filled = M.map (show . view color) $ view entries x

-- | Split a list into lists of the supplied length.
partition :: Int -> [a] -> [[a]]
partition n xs = go n xs where
  go n [] = []
  go n xs = p : (go n s) where
    (p, s) = splitAt n xs

-- | Initialization

randomSample :: (RandomGen gen) => gen -> Int -> [a] -> [a]
randomSample g len seq = take len $ shuffle' seq (length seq) g

indexToEntry :: Int -> Int -> Entry
indexToEntry r = uncurry Entry . flip divMod r


-- | Randomly initialize a square matrix of size rows^2 with a
-- population of size popSize, evenly divided among the individuals in
-- the list individuals.
populate :: (RandomGen gen) => gen -> [a] -> Int -> Int -> Matrix a
populate g individuals popSize rows =  mkMatrix rows rows $ popList where
  popList = concat $ zipWith (\e a -> zip e $ repeat a) els individuals
  els = chunksOf (popSize `div` length individuals +1) $ map (indexToEntry rows) $ randomSample g popSize [0..(rows^2)]


-- | Simulation

-- | Run one iteration of the simulation.
iteration :: (RandomGen gen, Fractional a, Ord a) => gen -> a -> Matrix Agent -> Matrix Agent
iteration g lvl m = foldl (\mm a -> move g mm a) m $ M.keys $ M.filterWithKey (\k v -> stay m lvl k) $ view entries m

-- | TODO: can I memoize this?
move :: (RandomGen gen) => gen -> Matrix Agent -> Entry -> Matrix Agent
move g m e = over entries (updateLocation old new) m where
  old = e
  new = head $ randomSample g 1 $ p''
  p'' = filter (\k -> not $ M.member k $ view entries m) $ pos m


iteration' g lvl m = foldl (\mm x -> move' g mm x) m $ toMoveAndEmptyCells where
  toMoves = M.keys $ M.filterWithKey (\k v -> stay m lvl k) $ view entries m
  emptyCells = randomSample g (length toMoves) $ filter (\k -> not $ M.member k $ view entries m) $ pos m
  toMoveAndEmptyCells = zip toMoves emptyCells

iteration'' g lvl m = foldl (\mm x -> move' g mm x) m $ toMoveAndEmptyCells where
  toMoves = M.keys $ M.filterWithKey (\k v -> stay m lvl k) $ view entries m
  emptyCells = randomSample g (length toMoves) $ filter (\k -> not $ M.member k $ view entries m) $ pos m
  toMoveAndEmptyCells = zip toMoves emptyCells
  -- is there a way here to calculate all the neighbors in one sweep?


-- | TODO: can I memoize this?
move' :: (RandomGen gen) => gen -> Matrix Agent -> (Entry, Entry) -> Matrix Agent
move' g m (old,new) = over entries (updateLocation old new) m where
  -- old = e
  -- new = head $ randomSample g 1 $ p''
  -- p'' = filter (\k -> not $ M.member k $ view entries m) $ pos m



pos :: Matrix Agent -> [Entry]
pos m = map (indexToEntry (view nrow m)) [0..(M.size $ (view entries m))]
es m = M.keys $ view entries m

updateLocation old new m = M.delete old $ M.insert new (fromJust $ M.lookup old m) m

stay :: (Fractional n, Ord n) => Matrix Agent -> n -> Entry -> Bool
stay m lvl a = neighborSimilarity m a >= lvl

neighborSimilarity :: (Fractional a) => Matrix Agent -> Entry -> a
neighborSimilarity m e = sameColor / totalNeighbors where
  totalNeighbors = fromIntegral $ length ns
  ns = neighbors' m e
  focalColor = fmap (view color) $ M.lookup e $ view entries m
  sameColor = case focalColor of
    Just c -> fromIntegral $ length $ filter (\x -> (==) (view color x)  c ) $ ns
    Nothing -> 0


neighbors :: Matrix v -> Entry -> [v]
neighbors m e = catMaybes $ map (flip M.lookup (view entries m)) $ adjacentEntries 1 e

neighbors' m e = catMaybes $ ks $ es' where
  --es' = adjacentEntries' (view nrow m) (view ncol m) 1 e
  es' = adjacentEntries 1 e
  ks = map (flip M.lookup (view entries m))


adjacentEntries' nr nc n x = delete x $ entrySeq (r - n, c - n) (r + n, r + n) where
  c = view row x
  r = view col x
  entrySeq (sr, sc) (er, ec) = [Entry i j | i <- [sr..er], j <- [sc..ec], i >= 0, j >= 0, i <= nr, j <= nc]


adjacentEntries n x = delete x $ entrySeq (r - n, c - n) (r + n, r + n) where
  c = view row x
  r = view col x
  entrySeq (sr, sc) (er, ec) = [Entry i j | i <- [sr..er], j <- [sc..ec]]


agents m c = M.size $ M.filter (\v -> view color v == c) $ view entries m

-- | IO functions for testing and developing.

chkNs m' fe = do
  print "--"
  print fe
  print $ M.lookup (fe) $ view entries m'
  print $ neighbors m' (fe)
  print $ neighborSimilarity m' (fe)
