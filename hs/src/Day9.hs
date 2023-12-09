{-|
Module:         Day9
Description:    <https://adventofcode.com/2023/day/9 Day 9: Mirage Maintenance>
-}
module Day9 (part1, part2) where

import Common (readEntire, readMany)
import Data.List (foldl', scanl')
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal, signed)

binom :: Int -> [Int]
binom n = scanl' f 1 [1..n] where f k i = k * (n + 1 - i) `div` i

predict :: [Int] -> Int
predict xs = foldl' subtract 0 $ zipWith (*) xs $ binom $ length xs

part1, part2 :: Text -> Either String Int
part1 = fmap sum .  mapM (fmap predict . readEntire (readMany $ T.signed T.decimal)) .  T.lines
part2 = fmap sum .  mapM (fmap (predict . reverse) . readEntire (readMany $ T.signed T.decimal)) .  T.lines
