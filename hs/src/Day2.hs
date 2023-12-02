{-|
Module:         Day2
Description:    <https://adventofcode.com/2023/day/2 Day 2: Cube Conundrum>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day2 (part1, part2) where

import Data.Char (isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as Map (elems, fromListWith, toList)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), between, parse, sepBy, sepEndBy, takeWhile1P)
import Text.Megaparsec.Char (newline, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Ord (Tokens s), Token s ~ Char, Num a, Num b, Ord b) => m [(a, Map (Tokens s) b)]
parser = game `sepEndBy` newline where
    game = do
        gameId <- between (string "Game ") (string ": ") L.decimal
        cubes <- Map.fromListWith max <$> cube `sepBy` (string ", " <|> string "; ")
        pure (gameId, cubes)
    cube = do
        count <- L.decimal <* string " "
        color <- takeWhile1P Nothing isAlphaNum
        pure (color, count)

part1, part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 input = do
    games <- parse parser "" input
    pure $ sum [gameId | (gameId, cubes) <- games, all ok $ Map.toList cubes]
  where
    ok ("red", count) = count <= 12
    ok ("green", count) = count <= 13
    ok ("blue", count) = count <= 14
    ok _ = False
part2 = fmap (sum . map (product . Map.elems . snd)) . parse parser ""
