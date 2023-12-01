{-|
Module:         Day1
Description:    <https://adventofcode.com/2023/day/1 Day 1: Trebuchet?!>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day1 (part1, part2) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (inits, isPrefixOf, isSuffixOf, lines, pack, tails)

part1, part2 :: Text -> Int
part1 = solve digits
part2 = solve extendedDigits

solve :: [(Int, Text)] -> Text -> Int
solve values = sum . mapMaybe solve' . T.lines where
    solve' line = do
        x <- listToMaybe [i | s <- T.tails line, (i, t) <- values, t `T.isPrefixOf` s]
        y <- listToMaybe [j | s <- reverse $ T.inits line, (j, t) <- values, t `T.isSuffixOf` s]
        pure $ 10 * x + y

digits, extendedDigits :: [(Int, Text)]
digits = [(d, T.pack $ show d) | d <- [0..9]]
extendedDigits = digits ++
  [ (1, "one")
  , (2, "two")
  , (3, "three")
  , (4, "four")
  , (5, "five")
  , (6, "six")
  , (7, "seven")
  , (8, "eight")
  , (9, "nine")
  ]
