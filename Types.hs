module Types where

data Config = Config { rows :: Int
                     , populatedShare :: Double
                     , level :: Double
                     , iterations :: Int
                     }
