{-|
Module:         Day20
Description:    <https://adventofcode.com/2023/day/20 Day 20: Pulse Propagation>
-}
{-# LANGUAGE OverloadedStrings, TransformListComp, ViewPatterns #-}
module Day20 (part1, part2) where

import Control.Monad (forM_, guard, when)
import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Writer (execWriterT, runWriter, tell)
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.List (foldl', scanl', tails)
import Data.Map (Map)
import qualified Data.Map as Map ((!?), empty, findWithDefault, fromList, fromListWith, fromSet, insert, keys, mapMaybeWithKey, notMember, toList, restrictKeys)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Monoid (Any(Any), Last(Last), Sum(Sum))
import qualified Data.Sequence as Seq (Seq((:<|)), fromList)
import Data.Set (Set)
import qualified Data.Set as Set (insert, intersection, member, singleton, toList)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Stream, Token, Tokens, (<|>), parse, sepBy, sepEndBy, takeWhile1P)
import Text.Megaparsec.Char (char, newline, string)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Map (Tokens s) [Tokens s], Map (Tokens s) (Set (Tokens s)), Map (Tokens s) (Either Bool (Map (Tokens s) Bool)))
parser = do
    let word = takeWhile1P Nothing isAlphaNum
        line = do
            state <- char '%' $> Just False <|> char '&' $> Just True <|> pure Nothing
            src <- word
            dsts <- string " -> " *> word `sepBy` string ", "
            pure (src, (dsts, state))
    modules <- Map.fromList <$> line `sepEndBy` newline
    let rmodules = Map.fromListWith (<>)
            [(dst, Set.singleton src) | (src, (dsts, _)) <- Map.toList modules, dst <- dsts]
        f _ (_, Nothing) = Nothing
        f _ (_, Just False) = Just $ Left False
        f key (_, Just True) = Just . Right . Map.fromSet (const False) $
            Map.findWithDefault mempty key rmodules
    pure (fst <$> modules, rmodules, Map.mapMaybeWithKey f modules)

pulse :: (Monad m, Ord a) => ((a, a, Bool) -> m ()) -> Map a [a] -> [(a, a, Bool)] -> Map a (Either Bool (Map a Bool)) -> m (Map a (Either Bool (Map a Bool)))
pulse f modules pulses = pulse' (Seq.fromList pulses) where
    pulse' (p@(src, dst, a) Seq.:<| seq) state = f p >> case state Map.!? dst  of
        Just (Left _) | a -> pulse' seq state
        Just (Left (not -> b)) -> pulse' (seq' b) $ Map.insert dst (Left b) state
        Just (Right (Map.insert src a -> b)) ->
            pulse' (seq' . not $ and b) $ Map.insert dst (Right b) state
        Nothing -> pulse' (seq' a) state
      where seq' b = seq <> Seq.fromList [(dst, dst', b) | dst' <- Map.findWithDefault [] dst modules]
    pulse' _ state = pure state

buttonPulses :: [(Text, Text, Bool)]
buttonPulses = [("button", "broadcaster", False)]

part1 :: Text -> Either (ParseErrorBundle Text Void) Int
part1 input = do
    (modules, _, state0) <- parse parser "" input
    let f (_, _, False) = tell (Sum 1, Sum 0)
        f (_, _, True) = tell (Sum 0, Sum 1)
    (Sum x, Sum y) <- execWriterT . flip evalStateT state0 . forM_ [1..1000] . const $
        get >>= pulse f modules buttonPulses >>= put
    pure $ x * y

part2 :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
part2 input = do
    (modules, rmodules, state0) <- parse parser "" input
    let bfs visited key
          | key `Set.member` visited = visited
          | otherwise = foldl' bfs (Set.insert key visited) $
                Map.findWithDefault mempty key rmodules
        cycle (dst, subset) = do
            let f (_, dst', a) = when (dst' == dst) $ tell (Any a, Last $ Just a)
                modules' = modules `Map.restrictKeys` subset
                state0' = state0 `Map.restrictKeys` subset
                states = iterate (runWriter . pulse f modules' buttonPulses . fst) (state0', mempty)
            (1, size, Just True) <- listToMaybe
              [ (j, i - j, lastOutput)
              | let states' = [state | (state, (Any true, _)) <- states, then takeWhile by not true]
              , (i, (state, (_, Last lastOutput)), m) <- zip3 [0..] states $
                    scanl' (flip $ uncurry Map.insert) mempty $ zip (fst <$> states) [0..]
              , j <- maybeToList $ m Map.!? state
              ]
            pure size
    pure $ do
        guard $ "rx" `Map.notMember` state0
        [inv] <- Set.toList <$> rmodules Map.!? "rx"
        Right (Map.keys -> dsts) <- state0 Map.!? inv
        let subsets = bfs mempty <$> dsts
        guard $ and
          [ s0 `Set.intersection` s1 == Set.singleton "broadcaster"
          | s0:subsets' <- tails subsets
          , s1 <- subsets'
          ]
        foldl' lcm 1 <$> mapM cycle (zip dsts subsets)
