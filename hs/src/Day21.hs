{-|
Module:         Day21
Description:    <https://adventofcode.com/2023/day/21 Day 21: Step Counter>
-}
{-# LANGUAGE ViewPatterns #-}
module Day21 (part1, part2) where

import Control.Monad (join)
import Data.Bits ((.&.), (.^.))
import Data.Bool (bool)
import Data.Heap (FstMinPolicy)
import qualified Data.Heap as Heap (empty, insert, view)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map ((!), (!?), empty, insert, intersectionWith, notMember)
import Data.Monoid (Sum(Sum), getSum)
import qualified Data.Set as Set (fromDistinctAscList, member, toList)
import Data.Text (Text)
import qualified Data.Text as T (all, head, index, last, length, lines)
import Data.Tuple (swap)
import qualified Data.Vector as V ((!), all, fromList, head, last, length)

bfs :: (Ord a, Enum b, Ord b) => (a -> [a]) -> [(a, b)] -> Map a b
bfs neighbors = bfs' Map.empty . foldl' (flip $ Heap.insert @FstMinPolicy . swap) Heap.empty where
    bfs' visited (Heap.view -> Just ((b, a), q))
      | Just c <- visited Map.!? a = if c > b then error "invalid state" else bfs' visited q
      | otherwise = bfs' (Map.insert a b visited) . foldl' (flip $ Heap.insert . (succ b,)) q .
            filter (`Map.notMember` visited) $ neighbors a
    bfs' visited _ = visited

part1 :: Int -> Text -> Int
part1 n input = getSum $ foldMap (Sum . bool 0 1 . ok) distances where
    grid = V.fromList $ T.lines input
    keys = Set.fromDistinctAscList
      [ (y, x)
      | y <- [0..V.length grid - 1]
      , let line = grid V.! y
      , x <- [0..T.length line - 1]
      , line `T.index` x /= '#'
      ]
    neighbors (y, x) = filter (`Set.member` keys) [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
    distances = bfs neighbors
        [(p, 0 :: Int) | p@(y, x) <- Set.toList keys, grid V.! y `T.index` x == 'S']
    ok x = x <= n && even (x .^. n)

part2 :: Int -> Text -> Int
part2 n input
  | odd size, V.all ((== size) . T.length) grid
  , T.all (/= '#') $ V.head grid, T.all (/= '#') $ V.last grid
  , V.all (\line -> T.head line /= '#' && T.last line /= '#') grid
  = getSum $ foldMap (Sum . countOne) origin <>
        foldMap (foldMap $ Sum . countQuadrant) quadrants <>
        mconcat (axes >>= countAxis)
  where
    grid = V.fromList $ T.lines input
    size = V.length grid
    keys = Set.fromDistinctAscList
      [ (y, x)
      | y <- [0..V.length grid - 1]
      , let line = grid V.! y
      , x <- [0..T.length line - 1]
      , line `T.index` x /= '#'
      ]
    neighbors (y, x) = filter (`Set.member` keys) [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

    origin = bfs neighbors
        [(p, 0 :: Int) | p@(y, x) <- Set.toList keys, grid V.! y `T.index` x == 'S']
    countOne x = if x <= n && even (x .^. n) then 1 else 0

    quadrants =
      [ bfs neighbors [((y, x), origin Map.! (size - 1 - y, size - 1 - x) + 2)]
      | y <- [0, size - 1], x <- [0, size - 1]
      ]
    countQuadrant x
      | x > n = 0
      | even r = join (*) $ r `div` size `div` 2 + 1
      | otherwise = (succ >>= (*)) $ (r `div` size + 1) `div` 2
      where r = n - x

    axes =
      [ iterate (bfs neighbors . copy) origin
      | f <- [(,), flip (,)], s <- [0, size - 1]
      , let copy distances = [(f s t, distances Map.! f (size - 1 - s) t + 1) | t <- [0..size - 1]]
      ]
    countAxis axis = foldMap (Sum . countRest) rest : (foldMap (Sum . countOne) <$> blocks) where
        done (prev, cur) = all (> n) cur || all (== size) (Map.intersectionWith (-) cur prev)
        (map snd -> blocks, (_, rest):_) = break done . zip axis $ tail axis
        countRest x
          | x > n = 0
          | otherwise = max 0 $ (r `div` size - (r .&. 1)) `div` 2 + 1
          where r = n - x
