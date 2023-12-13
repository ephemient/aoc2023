{-# LANGUAGE OverloadedStrings #-}
module Day13Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day13 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ -- :r!wl-paste | sed 's/.*/  , "&"/;1s/,/ /'
    "#.##..##."
  , "..#.##.#."
  , "##......#"
  , "##......#"
  , "..#.##.#."
  , "..##..##."
  , "#.#.##.#."
  , ""
  , "#...##..#"
  , "#....#..#"
  , "..##..###"
  , "#####.##."
  , "#####.##."
  , "..##..###"
  , "#....#..#"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` 405
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` 400
