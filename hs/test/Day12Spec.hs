{-# LANGUAGE OverloadedStrings #-}
module Day12Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day12 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "???.### 1,1,3"
  , ".??..??...?##. 1,1,3"
  , "?#?#?#?#?#?#?#? 1,3,1,6"
  , "????.#...#... 4,1,1"
  , "????.######..#####. 1,6,5"
  , "?###???????? 3,2,1"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` 21
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` 525152
