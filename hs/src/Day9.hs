{-|
Module:         Day9
Description:    <https://adventofcode.com/2023/day/9 Day 9: Mirage Maintenance>
-}
module Day9 (part1, part2) where

import Common (readEntire, readMany)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal, signed)

predict :: (Num a, Eq a) => [a] -> a
predict xs | all (== 0) xs = 0
predict xs@(_:_) = last xs + predict (zipWith subtract xs $ tail xs)

part1, part2 :: Text -> Either String Int
part1 = fmap sum .  mapM (fmap predict . readEntire (readMany $ T.signed T.decimal)) .  T.lines
part2 = fmap sum .  mapM (fmap (predict . reverse) . readEntire (readMany $ T.signed T.decimal)) .  T.lines
