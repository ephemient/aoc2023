{-# LANGUAGE OverloadedStrings #-}
module Day25Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day25 (part1)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "jqt: rhn xhk nvd"
  , "rsh: frs pzl lsr"
  , "xhk: hfx"
  , "cmg: qnr nvd lhk bvb"
  , "rhn: xhk bvb hfx"
  , "bvb: xhk hfx"
  , "pzl: lsr hfx nvd"
  , "qnr: nvd"
  , "ntq: jqt hfx bvb xhk"
  , "nvd: lhk"
  , "lsr: lhk"
  , "rzs: qnr cmg lsr rsh"
  , "frs: qnr lhk lsr"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            part1 example `shouldBe` Just 54
