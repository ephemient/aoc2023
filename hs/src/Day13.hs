{-|
Module:         Day13
Description:    <https://adventofcode.com/2023/day/13 Day 13: Point of Incidence>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day13 (part1, part2) where

import Data.List (findIndex, inits, tails)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (commonPrefixes, drop, lines, splitOn, transpose)

findReflection :: ([a] -> [a] -> Bool) -> [a] -> Int
findReflection _ [] = 0
findReflection eq lines = maybe 0 succ . findIndex (uncurry $ eq . reverse) .
    drop 1 . init $ zip (inits lines) (tails lines)

part1 :: Text -> Int
part1 = sum . map (part1' . T.lines) . T.splitOn "\n\n" where
    part1' lines = 100 * y + x where
      x = findReflection eq $ T.transpose lines
      y = findReflection eq lines
    xs `eq` ys = and $ zipWith (==) xs ys

part2 :: Text -> Int
part2 = sum . map (part2' . T.lines) . T.splitOn "\n\n" where
    part2' lines = 100 * y + x where
        x = findReflection (almostEqual False) $ T.transpose lines
        y = findReflection (almostEqual False) lines
    almostEqual k [] _ = k
    almostEqual k _ [] = k
    almostEqual k (x:xs) (y:ys)
      | x == y = almostEqual k xs ys
      | (_, x', y') <- fromMaybe ("", x, y) $ T.commonPrefixes x y
      , T.drop 1 x' == T.drop 1 y'
      = not k && almostEqual True xs ys
      | otherwise = False
