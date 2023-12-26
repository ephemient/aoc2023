{-|
Module:         Day23
Description:    <https://adventofcode.com/2023/day/23 Day 23: A Long Walk>
-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
module Day23 (part1, part2) where

import Control.Monad.Loops (whileM_)
import Control.Monad.State (execState, gets, modify)
import Data.Functor (($>))
import Data.List ((\\), foldl')
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map ((!), delete, empty, findWithDefault, foldlWithKey', fromDistinctAscList, keysSet, insert, maxViewWithKey, minViewWithKey, null, toList, update, updateLookupWithKey, withoutKeys)
import Data.Maybe (catMaybes)
import Data.Monoid (All(All), Sum(Sum))
import Data.Semigroup (Max(Max), getMax)
import qualified Data.Set as Set (empty, insert, member, notMember)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines, map)
import qualified Data.Vector as V ((!), fromList, length)

part1, part2 :: Text -> Maybe Int
part1 input = go start Set.empty 0 Nothing where
    grid = V.fromList $ T.lines input
    get (y, x)
      | y < 0 || y >= V.length grid = '#'
      | x <= 0 || x >= T.length line = '#'
      | otherwise = line `T.index` x
      where line = grid V.! y
    gr = Map.fromDistinctAscList
      [ ((y, x), Map.fromDistinctAscList adj)
      | y <- [0..V.length grid - 1]
      , let line = grid V.! y
      , x <- [0..T.length line - 1]
      , let c = line `T.index` x
      , c `elem` ".<>^v"
      , let mkEdge p f b
              | d `elem` ".<>^v" = Just
                ( p,
                    ( All $ (c == '.' || c == f) && (d == '.' || d == f)
                    , All $ (c == '.' || c == b) && (d == '.' || d == b)
                    , Sum 1
                    )
                )
              | otherwise = Nothing
              where d = get p
            adj = catMaybes
              [ mkEdge (y - 1, x) '^' 'v'
              , mkEdge (y, x - 1) '<' '>'
              , mkEdge (y, x + 1) '>' '<'
              , mkEdge (y + 1, x) 'v' '^'
              ]
      ]
    Just ((start, _), _) = Map.minViewWithKey gr
    Just ((end, _), _) = Map.maxViewWithKey gr
    simplify = whileM_ (gets Map.keysSet >>= foldr f (pure False)) $ pure () where
        f key k
          | key == start || key == end = k
          | otherwise = gets (Map.toList . (Map.! key)) >>= \case
            [] -> modify (Map.delete key) >> k $> True
            [(key', _)] -> modify (Map.delete key . Map.update (pure . Map.delete key) key') >> k $> True
            [(key1, value1), (key2, value2)] ->
                let adjust key' edge m = Map.insert key' edge'' m' where
                        (edge', m') = Map.updateLookupWithKey (const $ const Nothing) key m
                        edge'' = maybe edge (edge <>) edge'
                 in modify (Map.delete key .
                        Map.update (pure . adjust key2 value2) key1 .
                        Map.update (pure . adjust key1 value1) key2) >> k $> True
            _ -> k
    gr' = Map.fromDistinctAscList
      [ (key, edges')
      | (key, edges) <- Map.toList $ execState simplify gr
      , let edges' = Map.fromDistinctAscList
              [ (dst, weight)
              | (dst, (All True, _, Sum weight)) <- Map.toList edges
              ]
      , not $ Map.null edges'
      ]
    go pos used distance best
      | pos == end = max best $ Just distance
      | otherwise = Map.foldlWithKey' go' best $ Map.findWithDefault Map.empty pos gr'
      where
        go' best dst weight
          | dst `Set.member` used || Just potential <= best || end `Set.notMember` reachable = best
          | otherwise = go dst (Set.insert pos used) (distance + weight) best
        dfs (reachable, potential) pos
          | pos `Set.member` reachable = (reachable, potential)
          | otherwise
          = foldl' dfs (Set.insert pos reachable, maybe potential (+ potential) weight) $ Map.keysSet next
          where
            next = Map.findWithDefault Map.empty pos gr' `Map.withoutKeys` used
            weight = getMax <$> foldMap (Just . Max) next
        (reachable, potential) = dfs (used, distance) pos
part2 = part1 . T.map \case
    '<' -> '.'
    '>' -> '.'
    '^' -> '.'
    'v' -> '.'
    c -> c
