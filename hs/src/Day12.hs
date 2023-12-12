{-|
Module:         Day12
Description:    <https://adventofcode.com/2023/day/12 Day 12: Hot Springs>
-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, ViewPatterns #-}
module Day12 (part1, part2) where

import Common (readEntire)
import Control.Parallel.Strategies (parMap, rseq)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T (any, drop, findIndex, index, intercalate, length, lines, split, take, words)
import qualified Data.Text.Read as T (decimal)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V (drop, generate, sum, take)

solutions :: Text -> [Int] -> Int
solutions string (reverse -> run0:runs) =
    V.sum . maybe id (V.take . succ) (T.findIndex (== '#') string) $ foldl' f v0 runs where
    v0 = V.generate (T.length string) $ \i -> if
      | i + run0 > T.length string -> 0
      | i /= 0, string `T.index` (i - 1) == '#' -> 0
      | T.any (== '.') . T.take run0 $ T.drop i string -> 0
      | T.any (== '#') $ T.drop (i + run0) string -> 0
      | otherwise -> 1
    f v run = V.generate (T.length string) $ \i -> if
      | i + run >= T.length string -> 0
      | i /= 0, string `T.index` (i - 1) == '#' -> 0
      | T.any (== '.') . T.take run $ T.drop i string -> 0
      | string `T.index` (i + run) == '#' -> 0
      | otherwise -> V.sum .
            maybe id (V.take . succ) (T.findIndex (== '#') $ T.drop (i + run + 1) string) $
            V.drop (i + run + 1) v

part1 :: Text -> Int
part1 = sum . parMap rseq part1' . T.lines where
    part1' line
      | [lhs, rhs] <- T.words line
      , Right nums <- mapM (readEntire T.decimal) $ T.split (== ',') rhs
      = solutions lhs nums
      | otherwise = 0

part2 :: Text -> Int
part2 = sum . parMap rseq part2' . T.lines where
    part2' line
      | [lhs, rhs] <- T.words line
      , Right nums <- mapM (readEntire T.decimal) $ T.split (== ',') rhs
      = solutions (T.intercalate "?" $ replicate 5 lhs) . concat $ replicate 5 nums
      | otherwise = 0
