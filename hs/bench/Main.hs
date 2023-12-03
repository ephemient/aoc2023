module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, nf)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import qualified Day1 (part1, part2)
import qualified Day2 (part1, part2)
import qualified Day3 (part1, part2)
import System.Environment (lookupEnv)
import System.FilePath (combine)

getDayInput :: Int -> IO Text
getDayInput i = do
    dataDir <- fromMaybe "." . find (not . null) <$> lookupEnv "AOC2023_DATADIR"
    TIO.readFile . combine dataDir $ "day" ++ show i ++ ".txt"

main :: IO ()
main = defaultMain
  [ env (getDayInput 1) $ \input -> bgroup "Day 1"
      [ bench "part 1" $ nf Day1.part1 input
      , bench "part 2" $ nf Day1.part2 input
      ]
  , env (getDayInput 2) $ \input -> bgroup "Day 2"
      [ bench "part 1" $ nf Day2.part1 input
      , bench "part 2" $ nf Day2.part2 input
      ]
  , env (getDayInput 3) $ \input -> bgroup "Day 3"
      [ bench "part 1" $ nf Day3.part1 input
      , bench "part 2" $ nf Day3.part2 input
      ]
  ]
