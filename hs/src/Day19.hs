{-|
Module:         Day19
Description:    <https://adventofcode.com/2023/day/19 Day 19: Aplenty>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day19 (part1, part2) where

import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.Ix (rangeSize)
import Data.List (find, foldl')
import Data.Map (Map)
import qualified Data.Map as Map ((!?), fromList, insertWith)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Stream, Token, Tokens, (<|>), between, choice, eof, optional, parse, sepBy, sepEndBy, takeWhile1P, try)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

data Category = X | M | A | S deriving (Eq, Ord, Show)
data a :<> b = a :< b | a :> b deriving (Show)

parseRule :: (MonadParsec e s m, Token s ~ Char, Num a) => m (Tokens s, [(Tokens s, Maybe (Category :<> a))])
parseRule = (,) <$> name <*> between (char '{') (char '}') (rule `sepBy` char ',') where
    rule = flip (,) <$> optional (try cmp <* char ':') <*> name
    cmp = flip ($) <$> choice (zipWith (($>) . char) "xmas" [X, M, A, S]) <*>
        (char '<' $> (:<) <|> char '>' $> (:>)) <*> decimal
    name = takeWhile1P (Just "name") isAlphaNum

parsePoint :: (MonadParsec e s m, Token s ~ Char, Num a) => m (Map Category a)
parsePoint = between (char '{') (char '}') $ Map.fromList <$> part `sepBy` char ',' where
    part = (,) <$> choice (zipWith (($>) . char) "xmas" [X, M, A, S]) <*> (char '=' *> decimal)

part1, part2 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 input = do
    let parser = (,) . Map.fromList <$> parseRule `sepEndBy` newline <*> (newline *> parsePoint `sepEndBy` newline)
    (rules, points) <- parse (parser <* eof) "" input
    let ok "A" _ = True
        ok "R" _ = False
        ok name p = maybe False (flip ok p . fst) $ rules Map.!? name >>= find (ok' . snd) where
            ok' (Just (k :< b)) | Just a <- p Map.!? k = a < b
            ok' (Just (k :> b)) | Just a <- p Map.!? k = a > b
            ok' _ = True
    pure <$> sum $ sum <$> filter (ok "in") points
part2 input = do
    rules <- Map.fromList <$> parse (parseRule `sepEndBy` newline) "" input
    let f "A" p = [foldl' (flip $ (*) . rangeSize) 1 p]
        f name p | any (uncurry (>)) p = [] | otherwise = maybe [] (g p) $ rules Map.!? name
        g _ [] = []
        g p _ | any (uncurry (>)) p = []
        g p ((name, Nothing):_) = f name p
        g p ((name, Just cmp):rest) = g p2 rest <> f name p1 where
          (key, r1, r2)
            | key :< a <- cmp = (key, (minBound, a - 1), (a, maxBound))
            | key :> a <- cmp = (key, (a + 1, maxBound), (minBound, a))
          p1 = Map.insertWith intersectRange key r1 p
          p2 = Map.insertWith intersectRange key r2 p
        intersectRange (a, b) (c, d) = (max a c, min b d)
    pure . sum . f "in" $ Map.fromList $ (, (1, 4000 :: Int)) <$> [X, M, A, S]
