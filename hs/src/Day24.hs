{-|
Module:         Day24
Description:    <https://adventofcode.com/2023/day/24 Day 24: Never Tell Me The Odds>
-}
{-# LANGUAGE BlockArguments, NondecreasingIndentation, ScopedTypeVariables, TypeFamilies #-}
module Day24 (part1, part2) where

import Common (readEntire)
import Control.Arrow (first)
import Control.Monad (guard, when)
import Data.Char (isSpace)
import Data.List (tails)
import Data.Text (Text)
import qualified Data.Text as T (dropWhile, lines, singleton, stripPrefix)
import Data.Text.Read (Reader, decimal, signed)

parseLine :: (Num a) => Reader ((a, a, a), (a, a, a))
parseLine text = do
    let decimal' = fmap (first fromIntegral) . signed decimal . T.dropWhile isSpace
        skip token = maybe (Left $ "expected " ++ [token]) Right . T.stripPrefix (T.singleton token) . T.dropWhile isSpace
    (x, text) <- decimal' text
    (y, text) <- skip ',' text >>= decimal'
    (z, text) <- skip ',' text >>= decimal'
    (u, text) <- skip '@' text >>= decimal'
    (v, text) <- skip ',' text >>= decimal'
    (w, text) <- skip ',' text >>= decimal'
    pure (((x, y, z), (u, v, w)), text)

part1 :: (Fractional a, Ord a) => a -> a -> Text -> Either String Int
part1 lo hi input = do
    points <- mapM (readEntire parseLine) $ T.lines input
    pure . length $ do
        ((x0, y0, _), (vx0, vy0, _)):rest <- tails points
        ((x1, y1, _), (vx1, vy1, _)) <- rest
        when (vx0 == 0) $ error "vx0"
        when (vx1 == 0) $ error "vx1"
        -- y = (vy0 / vx0) * x + (y0 - x0 * vy0 / vx0)
        -- y = (vy1 / vx1) * x + (y1 - x1 * vy1 / vx1)
        let m0 = vy0 / vx0
            m1 = vy1 / vx1
            b0 = y0 - x0 * vy0 / vx0
            b1 = y1 - x1 * vy1 / vx1
            x = (b0 - b1) / (m1 - m0)
            y2 = m0 * x + b0
            y3 = m1 * x + b1
        when (m0 == m1) . guard $ b0 == b1 && error "m"
        guard $ lo <= x && x <= hi && lo <= y2 && y2 <= hi && lo <= y3 && y3 <= hi &&
            signum (x - x0) == signum vx0 && signum (x - x1) == signum vx1

part2 :: Text -> Either String Int
part2 input = do
    points <- mapM (readEntire parseLine) $ T.lines input
    Left "unimplemented"
