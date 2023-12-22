{-|
Module:         Day22
Description:    <https://adventofcode.com/2023/day/22 Day 22: Sand Slabs>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day22 (solve) where

import Common (readEntire)
import Control.Arrow ((***), first, second)
import Control.Parallel.Strategies (parMap, rseq)
import Data.List (foldl', mapAccumL, sortOn, tails)
import qualified Data.Map as Map ((!?), empty, fromDistinctAscList, fromListWith, keys, partition, toList, unionWith, update)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max(Max))
import qualified Data.Set as Set (delete, fromList, minView, null, singleton, size)
import Data.Text (Text)
import qualified Data.Text as T (lines, stripPrefix)
import qualified Data.Text.Read as T (decimal)
import Data.Text.Read (Reader)

parseLine :: (Integral a) => Reader ((a, a, a), (a, a, a))
parseLine text = do
    (x0, text) <- T.decimal text
    (y0, text) <- maybe (Left "expected ,") pure (T.stripPrefix "," text) >>= T.decimal
    (z0, text) <- maybe (Left "expected ,") pure (T.stripPrefix "," text) >>= T.decimal
    (x1, text) <- maybe (Left "expected ~") pure (T.stripPrefix "~" text) >>= T.decimal
    (y1, text) <- maybe (Left "expected ,") pure (T.stripPrefix "," text) >>= T.decimal
    (z1, text) <- maybe (Left "expected ,") pure (T.stripPrefix "," text) >>= T.decimal
    pure (((x0, y0, z0), (x1, y1, z1)), text)

settle :: (Integral a) => [((a, a, a), (a, a, a))] -> [((a, a, a), (a, a, a))]
settle = sortOn bottom . snd . mapAccumL f Map.empty . sortOn bottom where
    f zs ((x0, y0, z0), (x1, y1, z1)) = (zs', ((x0, y0, z + 1), (x1, y1, z'))) where
        Max z = fromMaybe (Max 0) $
            mconcat [Max <$> zs Map.!? (x, y) | x <- [x0..x1], y <- [y0..y1]]
        z' = z + 1 - z0 + z1
        zs' = Map.unionWith max zs $
            Map.fromDistinctAscList [((x, y), z') | x <- [x0..x1], y <- [y0..y1]]
    bottom ((_, _, z), _) = z

solve :: Text -> Either String (Int, Int)
solve input = do
    bricks <- fmap settle . mapM (readEntire (parseLine @Int)) $ T.lines input
    let (rdeps, deps) = (Map.fromListWith (<>) *** Map.fromListWith (<>)) $ unzip
          [ ((below, Set.singleton above), (above, Set.singleton below))
          | (below, ((x0, y0, _), (x1, y1, z))):rest <- tails $ zip @Int [0..] bricks
          , (above, ((x2, y2, _), (x3, y3, _))) <-
                takeWhile (\(_, ((_, _, z'), (_, _, _))) -> z' == z + 1) $
                dropWhile (\(_, ((_, _, z'), (_, _, _))) -> z' <= z) rest
          , x0 <= x3 && x2 <= x1 && y0 <= y3 && y2 <= y1
          ]
        unsafe = Set.fromList
          [ below
          | (above, Just (below, below')) <- second Set.minView <$> Map.toList deps
          , Set.null below'
          ]
        countFalls = fst . accumulateFalls' (0, deps)
        accumulateFalls' (k, deps') below = case rdeps Map.!? below of
            Just above ->
                let (below', deps'') = first Map.keys $ Map.partition Set.null $
                        foldl' (flip $ Map.update (pure . Set.delete below)) deps' above
                 in foldl' accumulateFalls' (k + length below', deps'') below'
            Nothing -> (k, deps')
    pure (length bricks - Set.size unsafe, sum . parMap rseq countFalls $ Map.keys rdeps)
