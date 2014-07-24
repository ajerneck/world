module World where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import qualified Data.Map as M
import System.Random

type Grid = M.Map Point Value

data Point = Point { row :: Int
                   , column :: Int
                   } deriving (Eq, Ord, Show)

data Value = Empty | Agent

instance Show Value where
  show Empty = "o"
  show Agent = "x"

emptyGrid size = M.fromList $ map (\p -> (p, Empty)) $  Point <$> [0..size] <*> [0..size]

showGrid :: Grid -> String
showGrid g = intercalate "\n" $ map concat $ partition (nrow g) $ map show $ M.elems g

ncol :: Grid -> Int
ncol = round . sqrt . fromIntegral . M.size

nrow :: Grid -> Int
nrow = ncol

-- | Split a list into lists of the supplied length.
partition :: Int -> [a] -> [[a]]
partition n xs = go n xs where
  go n [] = []
  go n xs = p : (go n s) where
    (p, s) = splitAt n xs


-- | Strategies
fillEven dimFunc p v = case (dimFunc p) `mod` 2 of
  0 -> Agent
  _ -> v

--mapPoints :: (Value -> Value) -> [Point] -> Grid -> Grid
mapPoints f = M.mapWithKey (f)

pf' = undefined

--pf'' :: [Point] -> Point -> Value
pf'' ps p v = case p `elem` ps of
  True -> Agent
  False -> v


--pointToRowCol :: (Num a) => a -> Point
indexToPoint size p = uncurry Point . flip divMod size

main size pc = do

  let eg = emptyGrid size

  rg <- getStdGen
  putStrLn $ showGrid eg

  let rps = take (round $ pc * fromIntegral (size^2)) $ (randomRs (0, size^2) rg)
  print rps

  let ps = map (indexToPoint size) rps
  return ps
--  let ps' = map (itp size) rps
  -- print ps'

  -- print ps


--  let egg' = M.mapWithKey (pf'' ps) eg

  --let g = M.mapWithKey (fillEven row) eg


--  putStrLn $ showGrid egg'

-- I AM HERE: implement the actual strategy: if neighor is of same kind or not, move. also, make quickcheck tests on the grid: that fillRandom (which fills the grid with a specified pc of cells with agents in random positions. which is now in main.)
