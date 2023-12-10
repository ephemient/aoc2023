{-|
Module:         Day10
Description:    <https://adventofcode.com/2023/day/10 Day 10: Pipe Maze>
-}
{-# LANGUAGE ViewPatterns #-}
module Day10 (solve) where

import Control.Arrow (first, second)
import Control.Monad (ap, guard)
import Data.Ix (range)
import Data.List ((\\), sort)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines, unpack)
import Data.Vector (Vector)
import qualified Data.Vector as V ((!), foldl', fromList, imap, length)

data Direction = U | L | D | R deriving Eq

inverse :: Direction -> Direction
inverse U = D
inverse L = R
inverse D = U
inverse R = L

move :: Direction -> (Int, Int) -> (Int, Int)
move U = first pred
move L = second pred
move D = first succ
move R = second succ

(!) :: Vector Text -> (Int, Int) -> [Direction]
maze ! (y, x)
  | '|' <- char = [U, D]
  | '-' <- char = [L, R]
  | 'L' <- char = [U, R]
  | 'J' <- char = [U, L]
  | '7' <- char = [L, D]
  | 'F' <- char = [D, R]
  | otherwise = []
  where
    line = maze V.! y
    char
      | y < 0 || y >= V.length maze = '.'
      | x < 0 || x >= T.length line = '.'
      | otherwise = line `T.index` x

solve :: Text -> Maybe (Int, Int)
solve input = listToMaybe $ do
    let maze = V.fromList $ T.lines input
        height = V.length maze
        width = V.foldl' (flip $ max . T.length) 0 maze
        follow k (inverse -> d) pos
          | d `elem` ds = ds \\ [d] >>= follow k' `ap` flip move pos
          | otherwise = [(k', d)]
          where
            ds = maze ! pos
            k' = pos : k
    start <- concat . flip V.imap maze $ \y line ->
        [(y, x) | (x, 'S') <- zip [0..] $ T.unpack line]
    d0 <- [U, L, D, R]
    (path@(end:_), d1) <- follow [] d0 $ move d0 start
    guard $ start == end
    let count (k, up, down, path) pos
          | pos':path' <- path, pos == pos'
          = (k, up /= (U `elem` dirs), down /= (D `elem` dirs), path')
          where dirs = if pos == start then [d0, d1] else maze ! pos
        count k@(_, False, False, _) _ = k
        count (!k, True, True, path) _ = (k + 1, True, True, path)
        (area, False, False, []) = foldl count (0, False, False, sort path) $
            range ((0, 0), (height - 1, width - 1))
    pure (length path `div` 2, area)
