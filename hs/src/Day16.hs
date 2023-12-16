{-|
Module:         Day16
Description:    <https://adventofcode.com/2023/day/16 Day 16: The Floor Will Be Lava>
-}
module Day16 (part1, part2) where

import Control.Arrow (first, second)
import Data.List (foldl')
import qualified Data.Set as Set (empty, fromList, insert, member, size, toList)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines, null)
import Data.Vector (Vector)
import qualified Data.Vector as V ((!), any, fromList, head, last, length, null)

data Direction = U | L | D | R deriving (Eq, Ord, Show)

move :: Direction -> (Int, Int) -> (Int, Int)
move U = first pred
move L = second pred
move D = first succ
move R = second succ

turn :: Char -> Direction -> [Direction]
turn '/' U = [R]
turn '/' R = [U]
turn '/' L = [D]
turn '/' D = [L]
turn '\\' U = [L]
turn '\\' L = [U]
turn '\\' D = [R]
turn '\\' R = [D]
turn '|' L = [U, D]
turn '|' R = [U, D]
turn '-' U = [L, R]
turn '-' D = [L, R]
turn _ d = [d]

fill :: Vector Text -> ((Int, Int), Direction) -> Int
fill v = Set.size . Set.fromList . map fst . Set.toList . fill' Set.empty where
    fill' visited pd@(p@(y, x), d)
      | y < 0 || y >= V.length v = visited
      | x < 0 || x >= T.length line = visited
      | pd `Set.member` visited = visited
      | otherwise = foldl' fill' (Set.insert pd visited)
            [(move d' p, d') | d' <- turn (line `T.index` x) d]
      where line = v V.! y

part1, part2 :: Text -> Int
part1 = flip fill ((0, 0), R) . V.fromList . filter (not . T.null) . T.lines
part2 input
  | V.null v = 0
  | otherwise = maximum . map (fill v) $
        [((0, x), D) | x <- [0..T.length (V.head v) - 1]] ++
        [((y, 0), R) | y <- [0..V.length v - 1]] ++
        [((V.length v - 1, x), U) | x <- [0..T.length (V.last v) - 1]] ++
        [((y, T.length (v V.! y) - 1), L) | y <- [0..V.length v - 1]]
  where v = V.fromList . filter (not . T.null) $ T.lines input
