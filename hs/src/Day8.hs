{-|
Module:         Day8
Description:    <https://adventofcode.com/2023/day/8 Day 8: Haunted Wasteland>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day8 (part1, part2) where

import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.List (elemIndex, find, foldl')
import qualified Data.Map as Map ((!), fromList, keys)
import Data.Maybe (fromJust)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T (last)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), manyTill, parse, sepEndBy, takeWhile1P)
import Text.Megaparsec.Char (char, newline, space1, string)

parser :: (MonadParsec e s m, IsString (Tokens s), Ord (Tokens s), Token s ~ Char) => m (Tokens s -> Tokens s, Int, [Tokens s])
parser = do
    instructions <- (char 'L' $> fst <|> char 'R' $> snd) `manyTill` space1
    table <- Map.fromList <$> line `sepEndBy` newline
    pure (foldl' (flip ($) . (table Map.!)) `flip` instructions, length instructions, Map.keys table)
  where
    line = (,) <$> word <* string " = (" <*>
        ((,) <$> word <* string ", " <*> word <* string ")")
    word = takeWhile1P Nothing isAlphaNum

part1, part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 input = do
    (step, n, _) <- parse parser "" input
    pure $ n * fromJust ("ZZZ" `elemIndex` iterate step "AAA")
part2 input = do
    (step, n, keys) <- parse parser "" input
    let check start (Just (i, end)) | step start == step end = i
        findCycle start = check start . find ((== 'Z') . T.last . snd) . zip [0..] $ iterate step start
    pure . (n *) . foldl' lcm 1 $ findCycle <$> filter ((== 'A') . T.last) keys
