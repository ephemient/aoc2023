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
import Data.Maybe (catMaybes)
import Data.Ratio ((%), denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T (dropWhile, lines, singleton, stripPrefix)
import qualified Data.Text.Read as T (decimal, signed)
import Data.Text.Read (Reader)
import GHC.Exts (the)

parseLine :: (Num a) => Reader ((a, a, a), (a, a, a))
parseLine text = do
    let decimal' = fmap (first fromIntegral) . T.signed T.decimal . T.dropWhile isSpace
        skip token = maybe (Left $ "expected " ++ [token]) Right .
            T.stripPrefix (T.singleton token) . T.dropWhile isSpace
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

part2 :: (Integral a) => Text -> Either String a
part2 input = do
    points <- mapM (readEntire parseLine) $ T.lines input
    foldr (<>) (Left "no solution") $ do
        ((x0, y0, z0), (vx0, vy0, vz0)):points <- tails points
        let offset ((x, y, z), (vx, vy, vz)) =
                ((x - x0, y - y0, z - z0), (vx - vx0, vy - vy0, vz - vz0))
        ((x1, y1, z1), (vx1, vy1, vz1)):points <- tails $ offset <$> points
        let px1 = y1 * vz1 - z1 * vy1
            py1 = z1 * vx1 - x1 * vz1
            pz1 = x1 * vy1 - y1 * vx1
        guard $ px1 /= 0 || py1 /= 0 || pz1 /= 0 -- 0 and 1 are skew
        ((x2, y2, z2), (vx2, vy2, vz2)):points <- tails points
        let px2 = y2 * vz2 - z2 * vy2
            py2 = z2 * vx2 - x2 * vz2
            pz2 = x2 * vy2 - y2 * vx2
        guard $ px2 /= 0 || py2 /= 0 || pz2 /= 0 -- 0 and 2 are skew
        let mx = py1 * pz2 - pz1 * py2
            my = pz1 * px2 - px1 * pz2
            mz = px1 * py2 - py1 * px2
        guard $ mx /= 0 || my /= 0 || mz /= 0 -- 1 and 2 are skew
        let u1 = (y1 * vx1 - x1 * vy1) % (my * vx1 - mx * vy1)
            u2 = (y2 * vx2 - x2 * vy2) % (my * vx2 - mx * vy2)
            f _ _ _ 0 = Nothing
            f m u p v = Just $ 1 % v * (m % 1 * u - p % 1)
            t1 = the $ catMaybes [f mx u1 x1 vx1, f my u1 y1 vy1, f mz u1 z1 vz1]
            t2 = the $ catMaybes [f mx u2 x2 vx2, f my u2 y2 vy2, f mz u2 z2 vz2]
        let offset = (mx + my + mz) % 1 * (u1 * t2 - u2 * t1) / (t2 - t1)
        pure $ if denominator offset /= 1
            then Left "non-integral solution"
            else Right $ x0 + y0 + z0 + numerator offset
