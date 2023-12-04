{-|
Module:         Day4
Description:    <https://adventofcode.com/2023/day/4 Day 4: Scratchcards>
-}
{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}
module Day4 (part1, part2) where

import Common (readEntire)
import Data.Bits (shiftL, shiftR)
import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, intersection, size)
import Data.Text (Text)
import qualified Data.Text as T (lines, null, span, stripPrefix, stripStart)
import Data.Text.Read (Reader)

readCard :: Reader (Set Text)
readCard input = do
    input1 <- maybe (Left "parse error") Right $ T.stripPrefix "Card" input
    let (n, input2) = T.span isDigit $ T.stripStart input1
    if T.null n then Left "parse error" else do
    input3 <- maybe (Left "parse error") Right $ T.stripPrefix ":" input2
    let (left, input4) = readNumbers Set.empty input3
    input5 <- maybe (Left "parse error") Right . T.stripPrefix "|" $ T.stripStart input4
    let (right, input6) = readNumbers Set.empty input5
    pure (Set.intersection left right, input6)
  where
    readNumbers k input
      | T.null word = (k, input)
      | otherwise = readNumbers (Set.insert word k) input'
      where (word, input') = T.span isDigit $ T.stripStart input

part1, part2 :: Text -> Either String Int
part1 = fmap (sum . map score) . mapM (readEntire readCard) . T.lines where
    score card = 1 `shiftL` Set.size card `shiftR` 1
part2 = fmap (sum . map head . init . scanl score (repeat 1)) .
    mapM (readEntire readCard) . T.lines where
    score (x:xs) card = zipWith (+) xs $ replicate (Set.size card) x ++ repeat 0
