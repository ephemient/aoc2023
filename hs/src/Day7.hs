{-|
Module:         Day7
Description:    <https://adventofcode.com/2023/day/7 Day 7: Camel Cards>
-}
module Day7 (part1, part2) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.List ((\\), elem, elemIndex, partition, sortBy)
import qualified Data.Map as Map (elems, fromListWith)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack, split)
import qualified Data.Text.Read as T (decimal)

cards, cards' :: String
cards = "23456789TJQKA"
cards' = cards \\ "J"

strength, strength' :: Char -> Int
strength c = fromMaybe (-1) $ elemIndex c cards
strength' c = fromMaybe (-1) $ elemIndex c cards'

handType, handType' :: String -> Int
handType hand
  | 5 `elem` counts = 6
  | 4 `elem` counts = 5
  | 3 `elem` counts && 2 `elem` counts = 4
  | 3 `elem` counts = 3
  | null $ [2, 2] \\ counts = 2
  | 2 `elem` counts = 1
  | otherwise = 0
  where counts = Map.elems $ Map.fromListWith (+) [(c, 1) | c <- hand]
handType' hand = maximum $ handType . (++) known <$> mapM (const cards') unknown
  where (known, unknown) = partition (/= 'J') hand

compareHands, compareHands' :: String -> String -> Ordering
compareHands a b = foldr (<>) (comparing length a b) $
    comparing handType a b : zipWith (comparing strength) a b
compareHands' a b = foldr (<>) (comparing length a b) $
    comparing handType' a b : zipWith (comparing strength') a b

solve :: (String -> String -> Ordering) -> Text -> Int
solve compare input = sum [rank * bid | (rank, (_, bid)) <- zip [1..] ranks] where
    hands =
      [ (T.unpack hand, bid')
      | hand:bid:_ <- T.split isSpace <$> T.lines input
      , bid' <- either (const []) ((:[]) . fst) $ T.decimal bid
      ]
    ranks = sortBy (compare `on` fst) hands

part1, part2 :: Text -> Int
part1 = solve compareHands
part2 = solve compareHands'
