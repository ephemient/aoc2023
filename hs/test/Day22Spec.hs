{-# LANGUAGE OverloadedStrings #-}
module Day22Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day22 (solve)
import Test.Hspec (Spec, describe, it, shouldBe, xit)

example :: Text
example = T.unlines
  [ "1,0,1~1,2,1"
  , "0,0,2~2,0,2"
  , "0,2,3~2,2,3"
  , "0,0,4~0,2,4"
  , "2,0,5~2,2,5"
  , "0,1,6~2,1,6"
  , "1,1,8~1,1,9"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            fst <$> solve example `shouldBe` Right 5
    describe "part 2" $ do
        it "examples" $ do
            snd <$> solve example `shouldBe` Right 7
