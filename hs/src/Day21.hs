{-|
Module:         Day21
Description:    <https://adventofcode.com/2023/day/21 Day 21: Step Counter>
-}
module Day21 (solve) where

import qualified Data.Set as Set (fromDistinctAscList, fromList, size, toList)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines)
import qualified Data.Vector as V ((!), fromList, length)

solve :: Int -> Text -> Int
solve n input = Set.size $ iterate go start !! n where
    maze = V.fromList $ T.lines input
    go set = Set.fromList
      [ p
      | (y, x) <- Set.toList set
      , p@(y', x') <- [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
      , 0 <= y' && y' < V.length maze
      , let line = maze V.! y'
      , 0 <= x' && x' < T.length line
      , line `T.index` x' /= '#'
      ]
    start = Set.fromDistinctAscList
      [ (y, x)
      | y <- [0..V.length maze - 1]
      , let line = maze V.! y
      , x <- [0..T.length line - 1]
      , line `T.index` x == 'S'
      ]
