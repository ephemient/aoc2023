{-# LANGUAGE OverloadedStrings #-}
module Day24Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day24 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe, xit)

example :: Text
example = T.unlines
  [ "19, 13, 30 @ -2,  1, -2"
  , "18, 19, 22 @ -1, -1, -2"
  , "20, 25, 34 @ -2, -2, -4"
  , "12, 31, 28 @ -1, -2, -1"
  , "20, 19, 15 @  1, -5, -3"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 7 27 example `shouldBe` Right 2
    describe "part 2" $ do
        xit "examples" $ do
            part2 example `shouldBe` Right 47
