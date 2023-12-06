{-# LANGUAGE OverloadedStrings #-}
module Day6Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day6 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "Time:      7  15   30"
  , "Distance:  9  40  200"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` 288
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` Right 71503
