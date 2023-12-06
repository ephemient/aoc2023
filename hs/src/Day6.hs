{-|
Module:         Day6
Description:    <https://adventofcode.com/2023/day/6 Day 6: Wait For It>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day6 (part1, part2) where

import Common (readEntire)
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as T (breakOn, dropWhile, filter)
import qualified Data.Text.Read as T (decimal)

winCount :: Int -> Int -> Int
winCount time distance = ceiling (b + d - 1) - floor (b - d + 1) + 1 where
    b = fromIntegral time / 2
    d = sqrt $ b * b - fromIntegral distance

part1 :: Text -> Int
part1 = product . uncurry (zipWith winCount `on` unfoldr reader) . T.breakOn "\n"
  where reader = either (const Nothing) Just . T.decimal . T.dropWhile (not . isDigit)

part2 :: Text -> Either String Int
part2 input = winCount <$>
    readEntire T.decimal (T.filter isDigit line1) <*>
    readEntire T.decimal (T.filter isDigit line2)
  where (line1, line2) = T.breakOn "\n" input
