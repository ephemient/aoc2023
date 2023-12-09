{-|
Module:         Day5
Description:    <https://adventofcode.com/2023/day/5 Day 5: If You Give A Seed A Fertilizer>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day5 (part1, part2) where

import Control.Monad.Loops (unfoldM)
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), eof, optional, parse, sepBy1, sepEndBy, skipSomeTill)
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, newline, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

data Range a = Range { rangeStart :: a, rangeEnd :: a }
data Mapping a = Mapping { mappingStart :: a, mappingEnd :: a, mappingOffset :: a }

parser :: (MonadParsec e s m, IsString (Tokens s), Ord (Tokens s), Token s ~ Char, Num a, Ord a) => m ([a], [[Mapping a]])
parser = (,) <$>
    skipSomeTill alphaNumChar (string ": " *> L.decimal `sepBy1` hspace1 <* space1) <*>
    unfoldM (optional mappings)
  where
    mappings = do
        skipSomeTill (alphaNumChar <|> char '-') $ string " map:" *> newline
        sortOn mappingStart <$> mapping `sepEndBy` newline <* (space1 <|> eof)
    mapping = do
        dest <- L.decimal <* hspace1
        source <- L.decimal <* hspace1
        size <- L.decimal
        pure Mapping {mappingStart = source, mappingEnd = source + size, mappingOffset = dest - source}

remap :: (Num a, Ord a, Show a) => [Mapping a] -> Range a -> [Range a]
remap mappings range@Range {rangeStart, rangeEnd} = mapped ++ gaps where
    mappings' =
      [ mapping
      | mapping@Mapping {mappingStart, mappingEnd} <- mappings
      , mappingStart < rangeEnd && rangeStart < mappingEnd
      ]
    mapped =
      [ Range (mappingOffset + max rangeStart mappingStart) (mappingOffset + min rangeEnd mappingEnd)
      | Mapping {mappingStart, mappingEnd, mappingOffset} <- mappings'
      ]
    gaps = filter ok $ zipWith Range
        (rangeStart : map mappingEnd mappings')
        (map mappingStart mappings' ++ [rangeEnd])
    ok Range {rangeStart, rangeEnd} = rangeStart < rangeEnd

part1, part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 input = do
    (seeds, mappings) <- parse parser "" input
    pure . minimum . map rangeStart .
        foldr (concatMap . remap) [Range x $ x + 1 | x <- seeds] $
        reverse mappings
part2 input = do
    (seeds, mappings) <- parse parser "" input
    pure . minimum . map rangeStart .
        foldr (concatMap . remap) [Range x $ x + y | [x, y] <- chunksOf 2 seeds] $
        reverse mappings
