{-|
Module:         Day18
Description:    <https://adventofcode.com/2023/day/18 Day 18: Lavaduct Lagoon>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day18 (part1, part2) where

import Control.Monad (replicateM)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Text (Text)
import Data.Void (Void)
import Numeric (readHex)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Parsec, Stream(Token), choice, eof, oneOf, parse, sepEndBy, skipManyTill)
import Text.Megaparsec.Char (char, digitChar, hexDigitChar, hspace1, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Direction = U | L | D | R deriving (Show)

solve :: (Token s ~ Char, Stream s, Ord e, Integral a) => Parsec e s (Direction, a) -> s -> Either (ParseErrorBundle s e) a
solve parser input = do
    moves <- parse (parser `sepEndBy` newline <* eof) "" input
    let (_, a, l) = foldl' f ((0, 0), 0, 2) moves
    pure $ abs a + l `div` 2
  where
    f ((y, x), a, l) (d, n)
      | U <- d = ((y - n, x), a, l + n)
      | L <- d = ((y, x - n), a - y * n, l + n)
      | D <- d = ((y + n, x), a, l + n)
      | R <- d = ((y, x + n), a + y * n, l + n)

part1, part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 = solve $ do
    d <- choice [char 'U' $> U, char 'L' $> L, char 'D' $> D, char 'R' $> R]
    n <- hspace1 *> decimal
    hspace1 *> string "(#" *> skipManyTill hexDigitChar (char ')') $> (d, n)
part2 = solve $ do
    oneOf @[] "ULDR" *> hspace1 *> skipManyTill digitChar hspace1 *> string "(#"
    (n, ""):_ <- readHex <$> replicateM 5 hexDigitChar
    d <- choice [char '0' $> R, char '1' $> D, char '2' $> L, char '3' $> U]
    char ')' $> (d, n)
