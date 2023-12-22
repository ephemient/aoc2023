{-# LANGUAGE OverloadedStrings #-}
module Day21Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day21 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "..........."
  , ".....###.#."
  , ".###.##..#."
  , "..#.#...#.."
  , "....#.#...."
  , ".##..S####."
  , ".##..#...#."
  , ".......##.."
  , ".##.#.####."
  , ".##..##.##."
  , "..........."
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 1 example `shouldBe` 2
            part1 2 example `shouldBe` 4
            part1 3 example `shouldBe` 6
            part1 6 example `shouldBe` 16
    describe "part 2" $ do
        it "examples" $ do
            part2 6 example `shouldBe` 16
            part2 10 example `shouldBe` 50
            part2 50 example `shouldBe` 1594
            part2 100 example `shouldBe` 6536
            part2 500 example `shouldBe` 167004
            part2 1000 example `shouldBe` 668697
            part2 5000 example `shouldBe` 16733044
