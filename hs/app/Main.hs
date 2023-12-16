{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where

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

import Control.Monad (ap, when)
import Data.Foldable (find)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (putStrLn, readFile)
import Text.Megaparsec (errorBundlePretty)
import System.Environment (getArgs, lookupEnv)
import System.FilePath (combine)

getDayInput :: Int -> IO Text
getDayInput i = do
    dataDir <- fromMaybe "." . find (not . null) <$> lookupEnv "AOC2023_DATADIR"
    TIO.readFile . combine dataDir $ "day" ++ show i ++ ".txt"

run :: Int -> (a -> IO ()) -> [Text -> a] -> IO ()
run = run' `ap` show

run' :: Int -> String -> (a -> IO ()) -> [Text -> a] -> IO ()
run' day name showIO funcs = do
    args <- getArgs
    when (null args || name `elem` args) $ do
    putStrLn $ "Day " ++ name
    contents <- getDayInput day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 print [Day1.part1, Day1.part2]
    run 2 (either (fail . errorBundlePretty) print) [Day2.part1, Day2.part2]
    run 3 print [Day3.part1, Day3.part2]
    run 4 (either fail print) [Day4.part1, Day4.part2]
    run 5 (either (fail . errorBundlePretty) print) [Day5.part1, Day5.part2]
    run 6 (either fail print) [pure . Day6.part1, Day6.part2]
    run 7 print [Day7.part1, Day7.part2]
    run 8 (either (fail . errorBundlePretty) print) [Day8.part1, Day8.part2]
    run 9 (either fail print) [Day9.part1, Day9.part2]
    run 10 (maybe (fail "error") $ uncurry ((>>) `on` print)) [Day10.solve]
    run 11 print [Day11.solve 2, Day11.solve 1000000]
    run 12 print [Day12.part1, Day12.part2]
    run 13 print [Day13.part1, Day13.part2]
    run 14 print [Day14.part1, Day14.part2]
    run 15 print [Day15.part1, Day15.part2]
    run 16 print [Day16.part1, Day16.part2]
