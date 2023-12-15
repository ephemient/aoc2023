{-# LANGUAGE OverloadedStrings #-}
module Day15Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day15 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` 1320
    describe "part 2" $ do
        it "examples" $ do
            part2 example `shouldBe` 145
