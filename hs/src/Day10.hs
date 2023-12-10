{-|
Module:         Day10
Description:    <https://adventofcode.com/2023/day/10 Day 10: Pipe Maze>
-}
{-# LANGUAGE ViewPatterns #-}
module Day10 (solve) where

import Control.Arrow (first, second)
import Control.Monad (ap)
import Data.Heap (FstMinPolicy, Heap)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set (empty, mapMonotonic, member, insert, intersection, size)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines, mapAccumL, unpack)
import Data.Vector (Vector)
import qualified Data.Vector as V ((!), foldl', fromList, imap, length, toList)
import Debug.Trace (trace)

(!) :: Vector Text -> (Int, Int) -> Char
maze ! (y, x)
  | y < 0 || y >= V.length maze = '.'
  | x < 0 || x >= T.length line = '.'
  | otherwise = line `T.index` x
  where line = maze V.! y

parse :: Text -> (Vector Text, Int, Int)
parse input = (maze, V.length maze, V.foldl' (flip $ max . T.length) 0 maze)
  where maze = V.fromList $ T.lines input

solve :: Text -> (Int, Int)
solve input = trace debug (part1, part2) where
    (maze, height, width) = parse input
    [start] = [(y, x) | y <- [0..height - 1], x <- [0..width - 1], maze ! (y, x) == 'S']
    (loop, part1) = bfs Set.empty 0 $ Heap.singleton @FstMinPolicy (0, start)

    outside = (Set.intersection `ap` Set.mapMonotonic (first pred)) $
        (Set.intersection `ap` Set.mapMonotonic (second pred)) fill
    part2 = width * height - Set.size loop - Set.size outside

    bfs visited k (Heap.view -> Just ((d, (y, x)), rest))
      | null next = bfs visited k rest
      | otherwise = bfs (Set.insert (y, x) visited) d $ foldr (Heap.insert . (d + 1,)) rest next
      where
        c = maze ! (y, x)
        next
          | Set.member (y, x) visited = []
          | y < 0 || y >= height || x < 0 || x >= width = []
          | '|' <- c = [(y - 1, x), (y + 1, x)]
          | '-' <- c = [(y, x - 1), (y, x + 1)]
          | 'L' <- c = [(y - 1, x), (y, x + 1)]
          | 'J' <- c = [(y - 1, x), (y, x - 1)]
          | '7' <- c = [(y + 1, x), (y, x - 1)]
          | 'F' <- c = [(y + 1, x), (y, x + 1)]
          | 'S' <- c
          = [(y - 1, x) | maze ! (y - 1, x) `elem` "|7F"] ++
            [(y, x - 1) | maze ! (y, x - 1) `elem` "-LF"] ++
            [(y, x + 1) | maze ! (y, x + 1) `elem` "-J7"] ++
            [(y + 1, x) | maze ! (y + 1,  x) `elem` "|LJ"]
          | otherwise = []
    bfs visited k _ = (visited, k)

    maze' = flip V.imap maze $ \y -> snd . T.mapAccumL (f y) 0 where
        f y x c = (x + 1, if (y, x) `Set.member` loop then c else '.')
    fill = foldl' dfs Set.empty $
        [(0, x) | x <- [0..width - 1]] ++
        [(y, 0) | y <- [0..height - 1]] ++
        [(y, width) | y <- [0..height - 1]] ++
        [(height, x) | x <- [0..width - 1]]
    dfs visited (y, x)
      | Set.member (y, x) visited = visited
      | otherwise = foldl' dfs (Set.insert (y, x) visited) $
            [(y - 1, x) | y > 0, ul `elem` "|J7.", ur `elem` "|LF."] ++
            [(y, x - 1) | x > 0, ul `elem` "-LJ.", dl `elem` "-7F."] ++
            [(y, x + 1) | x < width, ur `elem` "-LJ.", dr `elem` "-7F."] ++
            [(y + 1, x) | y < height, dl `elem` "|J7.", dr `elem` "|LF."]
      where
        ul = maze' ! (y - 1, x - 1)
        ur = maze' ! (y - 1, x)
        dl = maze' ! (y, x - 1)
        dr = maze' ! (y, x)

    debug = unlines . V.toList . flip V.imap maze $ \y line ->
      [ if (y, x) `Set.member` loop then c else if (y, x) `Set.member` outside then 'O' else 'I'
      | (x, c) <- zip [0..] $ T.unpack line
      ]
