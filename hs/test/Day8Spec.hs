{-# LANGUAGE OverloadedStrings #-}
module Day8Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day8 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2, example3 :: Text
example1 = T.unlines
  [ "RL"
  , ""
  , "AAA = (BBB, CCC)"
  , "BBB = (DDD, EEE)"
  , "CCC = (ZZZ, GGG)"
  , "DDD = (DDD, DDD)"
  , "EEE = (EEE, EEE)"
  , "GGG = (GGG, GGG)"
  , "ZZZ = (ZZZ, ZZZ)"
  ]
example2 = T.unlines
  [ "LLR"
  , ""
  , "AAA = (BBB, BBB)"
  , "BBB = (AAA, ZZZ)"
  , "ZZZ = (ZZZ, ZZZ)"
  ]
example3 = T.unlines
  [ "LR"
  , ""
  , "11A = (11B, XXX)"
  , "11B = (XXX, 11Z)"
  , "11Z = (11B, XXX)"
  , "22A = (22B, XXX)"
  , "22B = (22C, 22C)"
  , "22C = (22Z, 22Z)"
  , "22Z = (22B, 22B)"
  , "XXX = (XXX, XXX)"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example1 `shouldBe` Right 2
            part1 example2 `shouldBe` Right 6
    describe "part 2" $ do
        it "examples" $ do
            part2 example3 `shouldBe` Right 6
