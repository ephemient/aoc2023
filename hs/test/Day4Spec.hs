{-# LANGUAGE OverloadedStrings #-}
module Day4Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day4 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
  , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
  , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
  , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
  , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
  , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` Right 13
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` Right 30
