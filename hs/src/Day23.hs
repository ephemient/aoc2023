{-|
Module:         Day23
Description:    <https://adventofcode.com/2023/day/23 Day 23: A Long Walk>
-}
{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf #-}
module Day23 (part1, part2) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.STM (atomically, modifyTVar', newTQueueIO, newTVarIO, readTQueue, readTVarIO, writeTQueue)
import Control.Exception (bracket)
import Control.Monad (when, replicateM_)
import Control.Monad.Loops (whileJust_, whileM_)
import Control.Monad.State (execState, gets, modify, foldM_)
import Data.Functor (($>))
import Data.List ((\\), foldl')
import qualified Data.Map as Map ((!), delete, empty, findWithDefault, foldlWithKey', fromDistinctAscList, keysSet, insert, maxViewWithKey, minViewWithKey, null, size, toList, update, updateLookupWithKey, withoutKeys)
import Data.Maybe (catMaybes)
import Data.Monoid (All(All), Sum(Sum))
import Data.Semigroup (Max(Max), getMax)
import qualified Data.Set as Set (empty, insert, member, notMember)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines, map)
import qualified Data.Vector as V ((!), fromList, length)

part1, part2 :: Text -> IO (Maybe Int)
part1 input = do
    best <- newTVarIO Nothing
    queue <- newTQueueIO
    pending <- newTQueueIO
    let go (pos, used, distance) = do
            best' <- readTVarIO best
            if
              | pos == end ->
                when (Just distance > best') . atomically . modifyTVar' best . max $ Just distance
              | Just potential > best', end `Set.member` reachable -> do
                let next = Map.findWithDefault Map.empty pos gr' `Map.withoutKeys` used
                atomically . writeTQueue pending $ Map.size next
                Map.foldlWithKey' go' (pure ()) $ Map.findWithDefault Map.empty pos gr' `Map.withoutKeys` used
              | otherwise -> pure ()
            atomically $ writeTQueue pending (-1)
          where
            go' k dst weight = k >> atomically
                (writeTQueue queue $ Just (dst, Set.insert pos used, distance + weight))
            dfs (reachable, potential) pos
              | pos `Set.member` reachable = (reachable, potential)
              | otherwise
              = foldl' dfs (Set.insert pos reachable, maybe potential (+ potential) weight) $ Map.keysSet next
              where
                next = Map.findWithDefault Map.empty pos gr' `Map.withoutKeys` used
                weight = getMax <$> foldMap (Just . Max) next
            (reachable, potential) = dfs (used, distance) pos
        work = whileJust_ (atomically $ readTQueue queue) go
        wait 0 = pure ()
        wait n = atomically (readTQueue pending) >>= wait . (n +)
    bracket getNumCapabilities (flip replicateM_ . atomically $ writeTQueue queue Nothing) $ \n -> do
        replicateM_ n $ forkIO work
        atomically . writeTQueue queue $ Just (start, Set.empty, 0)
        wait 1
    readTVarIO best
  where
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
part2 = part1 . T.map \case
    '<' -> '.'
    '>' -> '.'
    '^' -> '.'
    'v' -> '.'
    c -> c
