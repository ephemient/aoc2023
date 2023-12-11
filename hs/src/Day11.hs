{-|
Module:         Day11
Description:    <https://adventofcode.com/2023/day/11 Day 11: Cosmic Expansion>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day11 (solve) where

import Control.Monad (ap)
import Data.Function (on)
import Data.List (mapAccumL, tails, scanl')
import Data.Text (Text)
import qualified Data.Text as T

solve :: Int -> Text -> Int
solve n = (((+) `on` solve1) `ap` T.transpose) . T.lines where
    solve1 input = sum $ do
        a:bs <- tails $ T.count "#" <$> input
        if a == 0 then mempty else snd $ mapAccumL (f a) 1 bs
    f _ m 0 = (m + n, 0)
    f a m b = (m + 1, m * a * b)
