{-# LANGUAGE OverloadedStrings #-}
module Day1Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day1 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2 :: Text
example1 = T.unlines
  [ "1abc2"
  , "pqr3stu8vwx"
  , "a1b2c3d4e5f"
  , "treb7uchet"
  ]
example2 = T.unlines
  [ "two1nine"
  , "eightwothree"
  , "abcone2threexyz"
  , "xtwone3four"
  , "4nineeightseven2"
  , "zoneight234"
  , "7pqrstsixteen"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example1 `shouldBe` 142
    describe "part 2" $ do
        it "examples" $ do
            part2 example2 `shouldBe` 281
