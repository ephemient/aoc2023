{-# LANGUAGE OverloadedStrings #-}
module Day21Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day21 (solve)
import Test.Hspec (Spec, describe, it, shouldBe, pendingWith)

example :: Text
example = T.unlines
  [ "..........."
  , ".....###.#."
  , ".###.##..#."
  , "..#.#...#.."
  , "....#.#...."
  , ".##..S####."
  , ".##..#...#."
  , ".......##.."
  , ".##.#.####."
  , ".##..##.##."
  , "..........."
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            solve 1 example `shouldBe` 2
            solve 2 example `shouldBe` 4
            solve 3 example `shouldBe` 6
            solve 6 example `shouldBe` 16
    describe "part 2" $ do
        it "examples" $ do
            pendingWith "Incomplete"
