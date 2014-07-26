{-# LANGUAGE NoMonomorphismRestriction, DataKinds, TemplateHaskell #-}
module Matrix where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Lens
import Data.List (intercalate, intersperse, delete)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Monoid
import System.Random
import System.Random.Shuffle

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
  show Blue = "  B  "
  show Yellow = "  Y  "
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
display x = concat $ intercalate ["\n"] $ map (intersperse " ") $ partition (view nrow x + 1) alls where
  alls = M.elems $ M.union filled empties
  empties = M.fromList [(Entry i j, show (i,j)) | i <- [0.. view nrow x], j <- [0.. view ncol x]]
  filled = M.map (show . view color) $ view entries x

-- | Split a list into lists of the supplied length.
partition :: Int -> [a] -> [[a]]
partition n xs = go n xs where
  go n [] = []
  go n xs = p : (go n s) where
    (p, s) = splitAt n xs


randomIndices pc r s = do
  g <- getStdGen
  return $ map (indexToEntry r) $ take (round $ pc * fromIntegral s) $ shuffle' [0..s] (s+1) g where
    indexToEntry :: Int -> Int -> Entry
    indexToEntry r = uncurry Entry . flip divMod r


-- | Simulation

--stay :: (Fractional n, Ord n) => Matrix Value -> n -> Value -> Bool
stay m lvl a = neighborSimilarity m a > lvl


neighborSimilarity :: (Fractional a) => Matrix Agent -> Entry -> a
neighborSimilarity m e = sameColor / totalNeighbors where
  totalNeighbors = fromIntegral $ length ns
  ns = neighbors m e
  focalColor = fmap (view color) $ M.lookup e $ view entries m
  sameColor = case focalColor of
    Just c -> fromIntegral $ length $ filter (\x -> (==) (view color x)  c ) $ ns
    Nothing -> 0

--compareColor c1 (Maybe c2) = (==) <$> Just (view color x) <*> c2)

--I AM HERE: implement neighbors: it should be, +1, -1 applied in all ways (ie, applicatively, to e, which is the focal entry)
--neighbors :: Matrix (Maybe Agent) -> Entry -> [Agent]
neighbors' m e = undefined

neighbors m e = catMaybes $ map (flip M.lookup (view entries m)) $ adjacentEntries 1 e

context c = [ (flip (-) c), (+c)]

entrySeq (sr, sc) (er, ec) = [Entry i j | i <- [sr..er], j <- [sc..ec]]

adjacentEntries n x = delete x $ entrySeq (r - n, c - n) (r + n, r + n) where
  c = view row x
  r = view col x


-- I AM HERE: implement the actual movement next: if percent of neighbors is of the same color, don't move, otherwise move. keep going (how can we define when it should stop? first start with just going for fixed number of iterations, then implement testing for convergence.)

-- | Agent handling

randomPop pc rows sz nr = do
  xs <- randomIndices pc rows (rows^2)
  return $ splitAt (length xs `div` nr) xs

agents m c = M.size $ M.filter (\v -> view color v == c) $ view entries m

chkNs m' fe = do
  print "--"
  print fe
  print $ M.lookup (fe) $ view entries m'
  print $ neighbors m' (fe)
  print $ neighborSimilarity m' (fe)

testNeighbors = do
  let pc = 0.80
  let rows = 4

  (ys, bs) <- randomPop pc rows (rows^2) 2

  let m' = mkMatrix rows rows $ (zip ys $ repeat (Agent Yellow))  ++ (zip bs $ repeat (Agent Blue))

  putStrLn $ display m'

  chkNs m' $ Entry 1 3
  chkNs m' $ Entry 2 2
  chkNs m' $ Entry 0 0
  chkNs m' $ Entry 4 4


main = do
  let pc = 0.80
  let rows = 4
  --let m = mkMatrix rows rows Nothing :: Matrix Agent

  -- TODO: better way of implementing this, so that there is a list comprehension for each kind of agent.
  (ys, bs) <- randomPop pc rows (rows^2) 2

  print $ length bs
  print $ length ys
  -- update it
  let m' = mkMatrix rows rows $ (zip ys $ repeat (Agent Yellow))  ++ (zip bs $ repeat (Agent Blue))

  putStrLn $ display m'

  print $ neighborSimilarity m' (Entry 3 3)
  print $ neighborSimilarity m' (Entry 1 1)

  chkNs m' $ Entry 1 3
  chkNs m' $ Entry 2 2
  chkNs m' $ Entry 0 0
  chkNs m' $ Entry 4 4



  --return m'
  return m'
