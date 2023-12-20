{-# LANGUAGE OverloadedStrings #-}
module Day20Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day20 (part1)
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe)

example1, example2 :: Text
example1 = T.unlines
  [ -- :r!wl-paste | sed 's/.*/  , "&"/;1s/,/ /'
    "broadcaster -> a, b, c"
  , "%a -> b"
  , "%b -> c"
  , "%c -> inv"
  , "&inv -> a"
  ]
example2 = T.unlines
  [ "broadcaster -> a"
  , "%a -> inv, con"
  , "&inv -> b"
  , "%b -> con"
  , "&con -> output"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example1 `shouldBe` Right 32000000
            part1 example2 `shouldBe` Right 11687500
    describe "part 2" $ do
        it "examples" $ do
            pendingWith "No examples provided"
