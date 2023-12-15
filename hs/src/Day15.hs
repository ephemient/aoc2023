{-|
Module:         Day15
Description:    <https://adventofcode.com/2023/day/15 Day 15: Lens Library>
-}
{-# LANGUAGE OverloadedStrings, TransformListComp #-}
module Day15 (part1, part2) where

import Data.Bits ((.&.))
import Data.Char (isAlphaNum, isSpace, ord)
import Data.Function (on)
import Data.List (foldl', groupBy, sortOn)
import qualified Data.Map as Map (delete, empty, insertWith, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (foldl', null, span, split, stripPrefix, stripSuffix)
import qualified Data.Text.Read as T (decimal)
import GHC.Exts (the)

hash :: Text -> Int
hash = (.&. 255) . T.foldl' hash' 0 where hash' k c = 17 * (k + ord c)

isSep :: Char -> Bool
isSep ',' = True
isSep c = isSpace c

part1, part2 :: Text -> Int
part1 = sum . map hash . T.split isSep
part2 = power . foldl' go Map.empty . zip [0..] . filter (not . T.null) . T.split isSep
  where
    go lens (c, ins)
      | "-" <- rest = Map.delete key lens
      | Just (Right (n, "")) <- T.decimal <$> T.stripPrefix "=" rest
      = Map.insertWith (\(n, _) (_, c) -> (n, c)) key (n, c) lens
      where (key, rest) = T.span isAlphaNum ins
    power lens = sum
      [ (the (hash <$> key) + 1) * sum (zipWith (*) [1..] n)
      | (key, (n, c)) <- Map.toList lens
      , then sortOn by (hash key, c)
      , then group by hash key using groupBy . on (==)
      ]
