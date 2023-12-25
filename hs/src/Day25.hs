{-|
Module:         Day25
Description:    <https://adventofcode.com/2023/day/25 Day 25: Snowverload>
-}
module Day25 (part1) where

import Control.Monad.RWS (execRWS, gets, modify, tell)
import Data.Functor (($>))
import Data.Graph.Inductive (DynGraph, Gr, bft, delEdges, lab, mkGraph, nodes, undir, scc)
import Data.List (sortOn)
import qualified Data.Map as Map ((!?), empty, fromListWith, insert, size, toList)
import Data.Maybe (fromJust, listToMaybe)
import Data.Ord (Down(Down))
import Data.Text (Text)
import qualified Data.Text as T (break, drop, lines, words)
import Data.Tuple (swap)
import Debug.Trace (traceShow)

cut :: (DynGraph gr, Show a) => gr a b -> [gr a b]
cut gr =
  [ traceShow (fromJust $ lab gr a, fromJust $ lab gr b) $ delEdges [(a, b), (b, a)] gr
  | ((a, b), _) <- sortOn (Down . snd) . Map.toList $ Map.fromListWith (+)
      [ ((min a b, max a b), 1 :: Int)
      | start <- nodes gr
      , path <- bft start gr
      , (a, b) <- zip path $ drop 1 path
      ]
  ]

part1 :: Text -> Maybe Int
part1 input = listToMaybe [length a * length b | [a, b] <- fmap scc $ cut gr >>= cut >>= cut]
  where
    node k = gets (Map.!? k) >>= maybe node' pure where
        node' = gets Map.size >>= (modify . Map.insert k >>= ($>))
    rws = sequence_
      [ tell . pure =<< (,, ()) <$> node src <*> node dst
      | (src, dsts) <- T.break (== ':') <$> T.lines input
      , dst <- T.words $ T.drop 1 dsts
      ]
    (nodes, edges) = execRWS rws () Map.empty
    gr = undir $ mkGraph @Gr (swap <$> Map.toList nodes) edges
