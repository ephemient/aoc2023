{-# LANGUAGE OverloadedStrings #-}
module Day18Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day18 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ -- :r!wl-paste | sed 's/.*/  , "&"/;1s/,/ /'
    "R 6 (#70c710)"
  , "D 5 (#0dc571)"
  , "L 2 (#5713f0)"
  , "D 2 (#d2c081)"
  , "R 2 (#59c680)"
  , "D 2 (#411b91)"
  , "L 5 (#8ceee2)"
  , "U 2 (#caa173)"
  , "L 1 (#1b58a2)"
  , "U 2 (#caa171)"
  , "R 2 (#7807d2)"
  , "U 3 (#a77fa3)"
  , "L 2 (#015232)"
  , "U 2 (#7a21e3)"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` Right 62
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` Right 952408144115
