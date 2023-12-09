{-# LANGUAGE OverloadedStrings #-}
module Day9Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day9 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "0 3 6 9 12 15"
  , "1 3 6 10 15 21"
  , "10 13 16 21 30 45"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` Right 114
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` Right 2
