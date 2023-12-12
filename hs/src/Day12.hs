{-|
Module:         Day12
Description:    <https://adventofcode.com/2023/day/12 Day 12: Hot Springs>
-}
{-# LANGUAGE OverloadedStrings, TransformListComp, ViewPatterns #-}
module Day12 (part1, part2) where

import Common (readEntire)
import Control.Parallel.Strategies (parMap, rseq)
import Data.List (foldl', maximumBy, inits, tails, scanl')
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T (all, any, breakOn, count, drop, dropAround, dropEnd, dropWhile, dropWhileEnd, head, index, intercalate, length, lines, null, split, splitOn, tail, take, unlines, unpack)
import qualified Data.Text.Read as T (decimal)
import Debug.Trace (trace)

choose :: Int -> Int -> Int
n `choose` r = foldl' f 1 $ zip [1..r] [n, n - 1..] where
    f k (a, b) | (q, 0) <- (k * b) `divMod` a = q
infix 1 `choose`

solutions :: Text -> [Int] -> Int
solutions (T.dropAround (== '.') -> s) xs
  | T.count "#" s > m ||
    m > T.length s - T.count "." s ||
    m + length xs - 1 > T.length s
  = 0
  | T.null s || null xs = 1
  | (leftS, rightS) <- T.breakOn "." s, not $ T.null rightS = sum
      [ left * solutions rightS rightXs
      | (leftXs, rightXs, acc) <- zip3 (inits xs) (tails xs) $ scanl' ((+) . succ) (-1) xs
      , then takeWhile by acc <= T.length leftS
      , let left = solutions leftS leftXs
      , left /= 0
      ]
  | T.all (/= '#') s = T.length s - m + 1 `choose` length xs
  | T.length maxRun > maximum xs = 0
  | not $ T.null maxRun, (leftS, rightS) <- T.breakOn maxRun s = sum
      [ left * solutions (T.drop (x' - dx + 1) rightS) rightXs
      | (leftXs, x' : rightXs, acc) <- zip3 (inits xs) (tails xs) $ scanl' ((+) . succ) 0 xs
      , dx <- [max 0 $ x' - T.length rightS..x' - T.length maxRun]
      , then takeWhile by acc + dx <= T.length leftS
      , dx + 1 > T.length leftS || leftS `T.index` (T.length leftS - dx - 1) /= '#'
      , x' - dx >= T.length rightS || rightS `T.index` (x' - dx) /= '#'
      , let left = solutions (T.dropEnd (dx + 1) leftS) leftXs
      , left /= 0
      ]
  | otherwise =
        (if x < T.length s && s `T.index` x == '#' then 0 else solutions (T.drop (x + 1) s) xs') +
        (if T.head s == '#' then 0 else solutions (T.tail s) xs)
  where
    m = sum xs
    x:xs' = xs
    maxRun = maximumBy (comparing T.length) $ T.split (/= '#') s

solutions' = solutions'' . T.dropWhile (== '.') where
    solutions'' s xs
      | T.count "#" s > m || T.length s' < n = 0
      | T.all (== '?') s'
      = T.length s' - m + 1 `choose` length xs
      where
        s' = T.dropWhileEnd (== '.') s
        m = sum xs
        n = m + length xs - 1
    solutions'' _ [] = 1
    solutions'' s xs@(x:xs') =
        (if T.any (== '.') (T.take x s) || s `T.index` x == '#' then 0 else solutions' (T.drop (x + 1) s) xs') +
        (if T.head s == '#' then 0 else solutions' (T.tail s) xs)

part1 :: Text -> Int
part1 = sum . parMap rseq part1' . T.lines where
    part1' line
      | [lhs, rhs] <- T.splitOn " " line
      , Right nums <- mapM (readEntire T.decimal) $ T.splitOn "," rhs
      = solutions lhs nums
      | otherwise = 0

part2 :: Text -> Int
part2 = sum . parMap rseq part2' . T.lines where
    part2' line
      | [lhs, rhs] <- T.splitOn " " line
      , Right nums <- mapM (readEntire T.decimal) $ T.splitOn "," rhs
      = ((++ ('\t' : T.unpack line)) . show >>= trace) .
        solutions (T.intercalate "?" $ replicate 5 lhs) . concat $ replicate 5 nums
      | otherwise = 0
