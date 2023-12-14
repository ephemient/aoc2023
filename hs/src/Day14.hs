{-|
Module:         Day14
Description:    <https://adventofcode.com/2023/day/14 Day 14: Parabolic Reflector Dish>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day14 (part1, part2) where

import qualified Data.Map as Map (empty, insertLookupWithKey)
import Data.Text (Text)
import qualified Data.Text as T (count, intercalate, lines, partition, reverse, splitOn, transpose)

part1, part2 :: Text -> Int
part1 = load . shift . T.lines where
    shift = T.transpose . map (T.intercalate "#" . map shift' . T.splitOn "#") . T.transpose
    shift' t = u <> v where (u, v) = T.partition (== 'O') t
    load input = sum [n * T.count "O" line | (n, line) <- zip [1..] $ reverse input]
part2 = load . loop Map.empty 0 . T.lines where
    n = 1000000000
    shiftR = map (T.reverse . T.intercalate "#" . map shiftR' . T.splitOn "#") . T.transpose
    shiftR' t = u <> v where (u, v) = T.partition (== 'O') t
    spin = shiftR . shiftR . shiftR . shiftR
    loop m !i a =
        maybe (loop m' (i + 1) $ spin a) (\j -> iterate spin a !! ((n - i) `mod` (i - j))) b
      where (b, m') = Map.insertLookupWithKey undefined a i m
    load input = sum [n * T.count "O" line | (n, line) <- zip [1..] $ reverse input]
