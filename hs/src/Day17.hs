{-|
Module:         Day17
Description:    <https://adventofcode.com/2023/day/17 Day 17: Clumsy Crucible>
-}
{-# LANGUAGE ViewPatterns #-}
module Day17 (part1, part2) where

import Control.Arrow (first, second)
import Data.Char (digitToInt)
import Data.Heap (FstMinPolicy, Heap)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.List (foldl')
import qualified Data.Set as Set (empty, insert, member)
import Data.Text (Text)
import qualified Data.Text as T (index, lines, length, null)
import qualified Data.Vector as V (Vector, (!), fromList, last, length)
import qualified Data.Vector.Unboxed as UV (Vector, (!), generate, length)

data Direction = U | L | D | R deriving (Bounded, Enum, Eq, Ord, Show)

move :: Direction -> (Int, Int) -> (Int, Int)
move U = first pred
move L = second pred
move D = first succ
move R = second succ

pred', succ' :: (Bounded a, Enum a, Eq a) => a -> a
pred' a = if a == minBound then maxBound else pred a
succ' a = if a == maxBound then minBound else succ a

parse :: Text -> V.Vector (UV.Vector Int)
parse = V.fromList . map digitsToInts . filter (not . T.null) . T.lines where
    digitsToInts line = UV.generate (T.length line) $ digitToInt . T.index line

part1, part2 :: Text -> Maybe Int
part1 input = bfs (Heap.singleton @FstMinPolicy (0, (0, 0, R, 0))) Set.empty where
    maze = parse input
    bfs (Heap.view -> Just ((k, state@(y, x, d, n)), q)) visited
      | Set.member state visited = bfs q visited
      | y == V.length maze - 1 && x == UV.length (V.last maze) - 1 = Just $ k + y + x
      | otherwise = bfs (foldl' (flip Heap.insert) q next) $ Set.insert state visited
      where
        next =
          [ (k + maze V.! y' UV.! x' + y - y' + x - x', (y', x', d', n'))
          | (d', n') <- (pred' d, 1) : (succ' d, 1) : [(d, n + 1) | n < 3]
          , let (y', x') = move d' (y, x)
          , 0 <= y' && y' < V.length maze
          , 0 <= x' && x' < UV.length (maze V.! y')
          ]
    bfs _ _ = Nothing
part2 input = bfs (Heap.singleton @FstMinPolicy (0, (0, 0, R, 0))) Set.empty where
    maze = parse input
    bfs (Heap.view -> Just ((k, state@(y, x, d, n)), q)) visited
      | Set.member state visited = bfs q visited
      | y == V.length maze - 1 && x == UV.length (V.last maze) - 1 && n >= 4 = Just $ k + y + x
      | otherwise = bfs (foldl' (flip Heap.insert) q next) $ Set.insert state visited
      where
        next =
          [ (k + maze V.! y' UV.! x' + y - y' + x - x', (y', x', d', n'))
          | (d', n') <-
                [(pred' d, 1) | n >= 4] ++
                [(succ' d, 1) | n >= 4] ++
                [(d, n + 1) | n < 10]
          , let (y', x') = move d' (y, x)
          , 0 <= y' && y' < V.length maze
          , 0 <= x' && x' < UV.length (maze V.! y')
          ]
    bfs _ _ = Nothing
