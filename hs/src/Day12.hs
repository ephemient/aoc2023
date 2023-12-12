{-|
Module:         Day12
Description:    <https://adventofcode.com/2023/day/12 Day 12: Hot Springs>
-}
{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, OverloadedStrings, TransformListComp, ViewPatterns #-}
module Day12 (part1, part2) where

import Common (readEntire)
import Control.Monad (forM_, when)
import Control.Monad.State (MonadState, evalState, execStateT, gets, modify, modify')
import Control.Monad.Trans (lift)
import Control.Parallel.Strategies (parMap, rseq)
import Data.List (foldl', maximumBy, inits, tails, scanl')
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, lookup)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T (all, any, breakOn, count, drop, dropAround, dropEnd, dropWhile, dropWhileEnd, head, index, intercalate, length, lines, null, split, tail, take, unlines, unpack, words)
import qualified Data.Text.Read as T (decimal)

choose :: Int -> Int -> Int
n `choose` r = foldl' f 1 $ zip [1..r] [n, n - 1..] where
    f k (a, b) | (q, 0) <- (k * b) `divMod` a = q
infix 1 `choose`

solutions :: Text -> [Int] -> Int
solutions s xs = evalState (solutions' s xs) Map.empty where
    solutions' (T.dropAround (== '.') -> s) xs = gets (Map.lookup (s, xs)) >>= flip maybe pure do
        let m = sum xs
            x:xs' = xs
            maxRun = maximumBy (comparing T.length) $ T.split (/= '#') s
        result <- if
          | T.count "#" s > m || m > T.length s - T.count "." s || m + length xs - 1 > T.length s
          -> pure 0
          | T.null s || null xs -> pure 1
          | (leftS, rightS) <- T.breakOn "." s, not $ T.null rightS -> flip execStateT 0 $
            forM_
              [ (leftXs, rightXs)
              | (leftXs, rightXs, acc) <- zip3 (inits xs) (tails xs) $ scanl' ((+) . succ) (-1) xs
              , then takeWhile by acc <= T.length leftS
              ] $ \(leftXs, rightXs) -> lift (solutions' leftS leftXs) >>= \case
                0 -> pure ()
                left -> lift (solutions' rightS rightXs) >>= modify' . (+) . (left *)
          | T.all (/= '#') s -> pure $ T.length s - m + 1 `choose` length xs
          | T.length maxRun > maximum xs -> pure 0
          | not $ T.null maxRun, (leftS, rightS) <- T.breakOn maxRun s -> flip execStateT 0 $
            forM_
              [ (T.dropEnd (dx + 1) leftS, leftXs, T.drop (x' - dx + 1) rightS, rightXs)
              | (leftXs, x' : rightXs, acc) <- zip3 (inits xs) (tails xs) $ scanl' ((+) . succ) 0 xs
              , dx <- [max 0 $ x' - T.length rightS..x' - T.length maxRun]
              , then takeWhile by acc + dx <= T.length leftS
              , dx + 1 > T.length leftS || leftS `T.index` (T.length leftS - dx - 1) /= '#'
              , x' - dx >= T.length rightS || rightS `T.index` (x' - dx) /= '#'
              ] $ \(leftS, leftXs, rightS, rightXs) -> lift (solutions' leftS leftXs) >>= \case
                0 -> pure ()
                left -> lift (solutions' rightS rightXs) >>= modify' . (+) . (left *)
          | otherwise -> flip execStateT 0 $ do
            when (x == T.length s || s `T.index` x /= '#') $
                lift (solutions' (T.drop (x + 1) s) xs') >>= modify' . (+)
            when (T.head s /= '#') $ lift (solutions' (T.tail s) xs) >>= modify' . (+)
        modify $ Map.insert (s, xs) result
        pure result

part1 :: Text -> Int
part1 = sum . parMap rseq part1' . T.lines where
    part1' line
      | [lhs, rhs] <- T.words line
      , Right nums <- mapM (readEntire T.decimal) $ T.split (== ',') rhs
      = solutions lhs nums
      | otherwise = 0

part2 :: Text -> Int
part2 = sum . parMap rseq part2' . T.lines where
    part2' line
      | [lhs, rhs] <- T.words line
      , Right nums <- mapM (readEntire T.decimal) $ T.split (== ',') rhs
      = solutions (T.intercalate "?" $ replicate 5 lhs) . concat $ replicate 5 nums
      | otherwise = 0
