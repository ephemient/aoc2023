module Main (main) where

import Control.Arrow ((>>>))
import Criterion.Main (bench, bgroup, defaultMain, env, envWithCleanup, nf)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import qualified Day1 (part1, part2)
import qualified Day2 (part1, part2)
import qualified Day3 (part1, part2)
import qualified Day4 (part1, part2)
import qualified Day5 (part1, part2)
import qualified Day6 (part1, part2)
import qualified Day7 (part1, part2)
import qualified Day8 (part1, part2)
import qualified Day9 (part1, part2)
import qualified Day10 (solve)
import qualified Day11 (solve)
import qualified Day12 (part1, part2)
import qualified Day13 (part1, part2)
import qualified Day14 (part1, part2)
import qualified Day15 (part1, part2)
import qualified Day16 (part1, part2)
import qualified Day17 (part1, part2)
import qualified Day18 (part1, part2)
import qualified Day19 (part1, part2)
import qualified Day20 (part1, part2)
import qualified Day21 (part1, part2)
import qualified Day22 (solve)
import qualified Day23 (part1, part2)
import qualified Day24 (part1, part2)
import qualified Day25 (part1)
import System.Environment.Blank (getEnv, setEnv, unsetEnv)
import System.FilePath (combine)

setTrace :: String -> IO (Maybe String)
setTrace value = getEnv "TRACE" <* setEnv "TRACE" value True

unsetTrace :: Maybe String -> IO ()
unsetTrace = maybe (unsetEnv "TRACE") (setEnv "TRACE" `flip` True)

getDayInput :: Int -> IO Text
getDayInput i = do
    dataDir <- fromMaybe "." . find (not . null) <$> getEnv "AOC2023_DATADIR"
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
  , env (getDayInput 4) $ \input -> bgroup "Day 4"
      [ bench "part 1" $ nf Day4.part1 input
      , bench "part 2" $ nf Day4.part2 input
      ]
  , env (getDayInput 5) $ \input -> bgroup "Day 5"
      [ bench "part 1" $ nf Day5.part1 input
      , bench "part 2" $ nf Day5.part2 input
      ]
  , env (getDayInput 6) $ \input -> bgroup "Day 6"
      [ bench "part 1" $ nf Day6.part1 input
      , bench "part 2" $ nf Day6.part2 input
      ]
  , env (getDayInput 7) $ \input -> bgroup "Day 7"
      [ bench "part 1" $ nf Day7.part1 input
      , bench "part 2" $ nf Day7.part2 input
      ]
  , env (getDayInput 8) $ \input -> bgroup "Day 8"
      [ bench "part 1" $ nf Day8.part1 input
      , bench "part 2" $ nf Day8.part2 input
      ]
  , env (getDayInput 9) $ \input -> bgroup "Day 9"
      [ bench "part 1" $ nf Day9.part1 input
      , bench "part 2" $ nf Day9.part2 input
      ]
  , env (getDayInput 10) $ \input -> bgroup "Day 10"
      [ bench "part 1" $ nf (fmap fst . Day10.solve) input
      , bench "part 2" $ nf (fmap snd . Day10.solve) input
      ]
  , env (getDayInput 11) $ \input -> bgroup "Day 11"
      [ bench "part 1" $ nf (Day11.solve 2) input
      , bench "part 2" $ nf (Day11.solve 1000000) input
      ]
  , env (getDayInput 12) $ \input -> bgroup "Day 12"
      [ bench "part 1" $ nf Day12.part1 input
      , bench "part 2" $ nf Day12.part2 input
      ]
  , env (getDayInput 13) $ \input -> bgroup "Day 13"
      [ bench "part 1" $ nf Day13.part1 input
      , bench "part 2" $ nf Day13.part2 input
      ]
  , env (getDayInput 14) $ \input -> bgroup "Day 14"
      [ bench "part 1" $ nf Day14.part1 input
      , bench "part 2" $ nf Day14.part2 input
      ]
  , env (getDayInput 15) $ \input -> bgroup "Day 15"
      [ bench "part 1" $ nf Day15.part1 input
      , bench "part 2" $ nf Day15.part2 input
      ]
  , env (getDayInput 16) $ \input -> bgroup "Day 16"
      [ bench "part 1" $ nf Day16.part1 input
      , bench "part 2" $ nf Day16.part2 input
      ]
  , env (getDayInput 17) $ \input -> bgroup "Day 17"
      [ bench "part 1" $ nf Day17.part1 input
      , bench "part 2" $ nf Day17.part2 input
      ]
  , env (getDayInput 18) $ \input -> bgroup "Day 18"
      [ bench "part 1" $ nf Day18.part1 input
      , bench "part 2" $ nf Day18.part2 input
      ]
  , env (getDayInput 19) $ \input -> bgroup "Day 19"
      [ bench "part 1" $ nf Day19.part1 input
      , bench "part 2" $ nf Day19.part2 input
      ]
  , env (getDayInput 20) $ \input -> bgroup "Day 20"
      [ bench "part 1" $ nf Day20.part1 input
      , bench "part 2" $ nf Day20.part2 input
      ]
  , env (getDayInput 21) $ \input -> bgroup "Day 21"
      [ bench "part 1" $ nf (Day21.part1 16) input
      , bench "part 2" $ nf (Day21.part2 26501365) input
      ]
  , env (getDayInput 22) $ \input -> bgroup "Day 22"
      [ bench "part 1" $ nf (fmap fst . Day22.solve) input
      , bench "part 2" $ nf (fmap snd . Day22.solve) input
      ]
  , env (getDayInput 23) $ \input -> bgroup "Day 23"
      [ bench "part 1" $ nf Day23.part1 input
      , bench "part 2" $ nf Day23.part2 input
      ]
  , env (getDayInput 24) $ \input -> bgroup "Day 24"
      [ bench "part 1" $ nf (Day24.part1 2e14 4e14) input
      , bench "part 2" $ nf (Day24.part2 @Integer) input
      ]
  , envWithCleanup ((,) <$> getDayInput 25 <*> setTrace "0")
        (unsetTrace . snd) $ fst >>> \input -> bgroup "Day 25"
            [bench "part 1" $ nf Day25.part1 input]
  ]
