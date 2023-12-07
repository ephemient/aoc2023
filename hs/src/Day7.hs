{-|
Module:         Day7
Description:    <https://adventofcode.com/2023/day/7 Day 7: Camel Cards>
-}
module Day7 (part1, part2) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (elemIndex, sort, sortOn)
import qualified Data.Map as Map (elems, fromListWith)
import Data.Maybe (isNothing)
import Data.Ord (Down(Down))
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack, split)
import qualified Data.Text.Read as T (decimal)

handType :: (Ord a) => [Maybe a] -> Int
handType hand
  | c0 + jokers >= 5 = 6
  | c0 + jokers >= 4 = 5
  | c0 + c1 + jokers >= 5 = 4
  | c0 + jokers >= 3 = 3
  | c0 + c1 + jokers >= 4 = 2
  | c0 + jokers >= 2 = 1
  | otherwise = 0
  where
    (c0:c1:_) = sortOn Down (Map.elems $ Map.fromListWith (+) [(c, 1) | Just c <- hand]) ++ [0, 0]
    jokers = length $ filter isNothing hand

solve :: String -> Text -> Int
solve cards input = sum [rank * bid | (rank, (_, _, bid)) <- zip [1..] $ sort hands] where
    hands =
      [ (handType hand', hand', bid')
      | hand:bid:_ <- T.split isSpace <$> T.lines input
      , let hand' = (`elemIndex` cards) <$> T.unpack hand
      , bid' <- either (const []) ((: []) . fst) $ T.decimal bid
      ]

part1, part2 :: Text -> Int
part1 = solve "23456789TJQKA"
part2 = solve "23456789TQKA"
