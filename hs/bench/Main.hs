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
  , envWithCleanup ((,) <$> getDayInput 12 <*> setTrace "0")
        (unsetTrace . snd) $ fst >>> \input -> bgroup "Day 12"
          [ bench "part 1" $ nf Day12.part1 input
          , bench "part 2" $ nf Day12.part2 input
      ]
  , env (getDayInput 13) $ \input -> bgroup "Day 13"
      [ bench "part 1" $ nf Day13.part1 input
      , bench "part 2" $ nf Day13.part2 input
      ]
  ]
