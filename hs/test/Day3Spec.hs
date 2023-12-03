{-# LANGUAGE OverloadedStrings #-}
module Day3Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day3 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "467..114.."
  , "...*......"
  , "..35..633."
  , "......#..."
  , "617*......"
  , ".....+.58."
  , "..592....."
  , "......755."
  , "...$.*...."
  , ".664.598.."
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` 4361
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` 467835
