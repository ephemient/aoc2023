{-|
Module:         Day17
Description:    <https://adventofcode.com/2023/day/17 Day 17: Clumsy Crucible>
-}
{-# LANGUAGE ViewPatterns #-}
module Day17 (part1, part2) where

import Control.Arrow (first, second)
import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as Map ((!), (!?), insert, singleton)
import Data.Hashable (Hashable(hashWithSalt), hashUsing)
import Data.Heap (FstMinPolicy, Heap)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T (index, lines, length, null)
import qualified Data.Vector as V (Vector, (!), fromList, last, length)
import qualified Data.Vector.Unboxed as UV (Vector, (!), generate, length)

data Direction = U | L | D | R deriving (Bounded, Enum, Eq, Ord, Show)

instance Hashable Direction where
    hashWithSalt = hashUsing fromEnum

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

solve :: (Int -> Bool) -> (Direction -> Int -> [Direction]) -> Text -> Maybe Int
solve ok nexts input =
    bfs (Heap.singleton @FstMinPolicy (0, start)) $ Map.singleton start 0 where
    digitsToInts line = UV.generate (T.length line) $ digitToInt . T.index line
    maze = V.fromList . map digitsToInts . filter (not . T.null) $ T.lines input
    start = (0, 0, R, 0)
    bfs (Heap.view -> Just ((prio, state@(y, x, d, n)), q)) costs
      | costs Map.! state < cost = bfs q costs
      | y == V.length maze - 1 && x == UV.length (V.last maze) - 1 && ok n = Just cost
      | otherwise = bfs (foldl' heapInsert q adj) $ foldl' hashMapInsert costs adj
      where
        cost = prio + y + x
        adj =
          [ (state', cost')
          | d' <- nexts d n
          , let (y', x') = move d' (y, x)
          , 0 <= y' && y' < V.length maze
          , 0 <= x' && x' < UV.length (maze V.! y')
          , let cost' = cost + maze V.! y' UV.! x'
                state' = (y', x', d', if d == d' then n + 1 else 1)
          , maybe True (cost' <) $ costs Map.!? state'
          ]
        heapInsert q' (state'@(y', x', _, _), cost') = Heap.insert (cost' - y' - x', state') q'
        hashMapInsert costs' (state', cost') = Map.insert state' cost' costs'
    bfs _ _ = Nothing

part1, part2 :: Text -> Maybe Int
part1 = solve (const True) nexts where
    nexts d n
      | n < 3 = [pred' d, succ' d, d]
      | otherwise = [pred' d, succ' d]
part2 = solve (>= 4) nexts where
    nexts d n
      | n < 4 = [d]
      | n < 10 = [pred' d, succ' d, d]
      | otherwise = [pred' d, succ' d]
