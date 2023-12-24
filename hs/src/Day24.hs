{-|
Module:         Day24
Description:    <https://adventofcode.com/2023/day/24 Day 24: Never Tell Me The Odds>
-}
module Day24 (part1, part2) where

import Common (readEntire)
import Control.Arrow (first)
import Control.Monad ((>=>), filterM, guard, when)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List (tails)
import Data.Text (Text)
import qualified Data.Text as T (dropWhile, lines, singleton, stripPrefix)
import Data.Text.Read (Reader)
import qualified Data.Text.Read as T (decimal, signed)

parseLine :: (Num a) => Reader ((a, a, a), (a, a, a))
parseLine text = do
    let decimal' = fmap (first fromIntegral) . T.signed T.decimal . T.dropWhile isSpace
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
    let toLine ((x, y, _), (vx, vy, _)) = when (vx == 0) (Left "unimplemented") $>
            (m, y - x * m, (/= compare vx 0) . compare x)
          where m = vy / vx
    lines <- mapM (readEntire parseLine >=> toLine) $ T.lines input
    let ok ((m0, b0, ok0), (m1, b1, ok1))
          | m0 == m1 = when (b0 == b1) (Left "unimplemented") $> False
          | otherwise = pure $ lo <= x && x <= hi && lo <= y && y <= hi && ok0 x && ok1 x
          where
            x = (b0 - b1) / (m1 - m0)
            y = m0 * x + b0
    length <$> filterM ok [(line0, line1) | line0:lines' <- tails lines, line1 <- lines']

part2 :: Text -> Either String Int
part2 input = do
    points <- mapM (readEntire parseLine) $ T.lines input
    Left "unimplemented"
