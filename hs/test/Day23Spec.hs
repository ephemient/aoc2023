{-# LANGUAGE OverloadedStrings #-}
module Day23Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day23 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldReturn, it)

example :: Text
example = T.unlines
  [ "#.#####################"
  , "#.......#########...###"
  , "#######.#########.#.###"
  , "###.....#.>.>.###.#.###"
  , "###v#####.#v#.###.#.###"
  , "###.>...#.#.#.....#...#"
  , "###v###.#.#.#########.#"
  , "###...#.#.#.......#...#"
  , "#####.#.#.#######.#.###"
  , "#.....#.#.#.......#...#"
  , "#.#####.#.#.#########v#"
  , "#.#...#...#...###...>.#"
  , "#.#.#v#######v###.###v#"
  , "#...#.>.#...>.>.#.###.#"
  , "#####v#.#.###v#.#.###.#"
  , "#.....#...#...#.#.#...#"
  , "#.#########.###.#.#.###"
  , "#...###...#...#...#.###"
  , "###.###.#.###v#####v###"
  , "#...#...#.#.>.>.#.>.###"
  , "#.###.###.#.###.#.#v###"
  , "#.....###...###...#...#"
  , "#####################.#"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldReturn` Just 94
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldReturn` Just 154
