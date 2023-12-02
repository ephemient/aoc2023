{-# LANGUAGE OverloadedStrings #-}
module Day2Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day2 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
  , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
  , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
  , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` Right 8
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` Right 2286
