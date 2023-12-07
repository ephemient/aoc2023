{-# LANGUAGE OverloadedStrings #-}
module Day7Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day7 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "32T3K 765"
  , "T55J5 684"
  , "KK677 28"
  , "KTJJT 220"
  , "QQQJA 483"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` 6440
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` 5905
