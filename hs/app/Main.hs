{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where

import qualified Day1 (part1, part2)

import Control.Monad (ap, when)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (putStrLn, readFile)
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
