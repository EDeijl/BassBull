module Bassbull where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
-- from cassava
import Data.Csv.Streaming

-- a simple type alias for data
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

fourth:: (a, b, c, d) -> d
fourth (_, _, _, d) = d

baseballStats:: BL.ByteString -> Records BaseballStats
baseballStats = decode NoHeader

summer :: (a, b, c, Int) -> Int -> Int
summer = (+) . fourth

getAtBatsSum :: FilePath -> IO Int
getAtBatsSum battingCsv = do
  csvData <- BL.readFile battingCsv
  return $ F.foldr summer 0 (baseballStats csvData)
