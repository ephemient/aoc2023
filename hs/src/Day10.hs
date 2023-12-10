{-|
Module:         Day10
Description:    <https://adventofcode.com/2023/day/10 Day 10: Pipe Maze>
-}
module Day10 (solve) where

import Control.Arrow (first, second)
import Control.Monad (guard)
import Data.List (scanl')
import Data.Map (Map)
import qualified Data.Map as Map ((!?), fromList)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines, split)
import Data.Vector (Vector)
import qualified Data.Vector as V ((!), fromList, length)

data Direction = U | L | D | R deriving (Eq, Ord, Show)

move :: Direction -> (Int, Int) -> (Int, Int)
move U = first pred
move L = second pred
move D = first succ
move R = second succ

lut :: Map (Direction, Char) Direction
lut = Map.fromList
  [ ((U, '|'), U), ((U, '7'), L), ((U, 'F'), R)
  , ((L, '-'), L), ((L, 'F'), D), ((L, 'L'), U)
  , ((D, '|'), D), ((D, 'L'), R), ((D, 'J'), L)
  , ((R, '-'), R), ((R, 'J'), U), ((R, '7'), D)
  ]

solve :: Text -> Maybe (Int, Int)
solve input = listToMaybe $ do
    let maze = V.fromList $ T.lines input
    startY <- [0..V.length maze - 1]
    let line = maze V.! startY
    startX <- drop 1 . init . scanl' (flip $ (+) . succ . T.length) (-1) $
        T.split (== 'S') line
    startDir <- [U, L, D, R]
    let start = (startY, startX)
        f (pos, Just dir)
          | 0 <= y && y < V.length maze, line <- maze V.! y
          , 0 <= x && x < T.length line, char <- line `T.index` x
          = (pos', lut Map.!? (dir, char))
          | otherwise = (pos', Nothing)
          where pos'@(y, x) = move dir pos
        (path, (end, _) : _) = first (map fst) . span (isJust . snd) $
            iterate f (start, Just startDir)
    guard $ start == end
    let (part1, 0) = length path `divMod` 2
        (halfArea, 0) = sum
          [ x0 * y1 - x1 * y0
          | ((y0, x0), (y1, x1)) <- zip path $ drop 1 path ++ path
          ] `divMod` 2
        part2 = abs halfArea - part1 + 1
    pure (part1, part2)
