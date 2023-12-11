{-|
Module:         Day11
Description:    <https://adventofcode.com/2023/day/11 Day 11: Cosmic Expansion>
-}
module Day11 (solve) where

import qualified Data.IntSet as IntSet (dropWhileAntitone, fromDistinctAscList, size, takeWhileAntitone)
import Data.List (tails, transpose)
import Data.Text (Text)
import qualified Data.Text as T (unpack)

solve :: Int -> Text -> Int
solve n input = sum
  [ y1 - y0 + abs (x1 - x0) + (n - 1) * (IntSet.size ys + IntSet.size xs)
  | (i, (y0, x0):points') <- zip [1..] $ tails points
  , (j, (y1, x1)) <- zip [i + 1..] points'
  , let ys = IntSet.takeWhileAntitone (< y1) $ IntSet.dropWhileAntitone (< y0) rows
        xs = IntSet.takeWhileAntitone (< max x0 x1) $ IntSet.dropWhileAntitone (< min x0 x1) cols
  ] where
    image = lines $ T.unpack input
    rows = IntSet.fromDistinctAscList [y | (y, row) <- zip [0..] image, '#' `notElem` row]
    cols = IntSet.fromDistinctAscList [x | (x, col) <- zip [0..] $ transpose image, '#' `notElem` col]
    points = [(y, x) | (y, row) <- zip [0..] image, (x, '#') <- zip [0..] row]
