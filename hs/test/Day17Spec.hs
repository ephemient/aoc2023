{-# LANGUAGE OverloadedStrings #-}
module Day17Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day17 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe, it)

example1, example2 :: Text
example1 = T.unlines
  [ -- :r!wl-paste | sed 's/.*/  , "&"/;1s/,/ /'
    "2413432311323"
  , "3215453535623"
  , "3255245654254"
  , "3446585845452"
  , "4546657867536"
  , "1438598798454"
  , "4457876987766"
  , "3637877979653"
  , "4654967986887"
  , "4564679986453"
  , "1224686865563"
  , "2546548887735"
  , "4322674655533"
  ]
example2 = T.unlines
  [ -- :r!wl-paste | sed 's/.*/  , "&"/;1s/,/ /'
    "111111111111"
  , "999999999991"
  , "999999999991"
  , "999999999991"
  , "999999999991"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example1 `shouldBe` Just 102
    describe "part 2" $ do
        it "examples" $ do
            part2 example1 `shouldBe` Just 94
            part2 example2 `shouldBe` Just 71
