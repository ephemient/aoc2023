{-# LANGUAGE OverloadedStrings #-}
module Day10Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day10 (solve)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2, example3, example4, example5, example6 :: Text
example1 = T.unlines
  [ "-L|F7"
  , "7S-7|"
  , "L|7||"
  , "-L-J|"
  , "L|-JF"
  ]
example2 = T.unlines
  [ "7-F7-"
  , ".FJ|7"
  , "SJLL7"
  , "|F--J"
  , "LJ.LJ"
  ]
example3 = T.unlines
  [ "..........."
  , ".S-------7."
  , ".|F-----7|."
  , ".||.....||."
  , ".||.....||."
  , ".|L-7.F-J|."
  , ".|..|.|..|."
  , ".L--J.L--J."
  , "..........."
  ]
example4 = T.unlines
  [ ".........."
  , ".S------7."
  , ".|F----7|."
  , ".||....||."
  , ".||....||."
  , ".|L-7F-J|."
  , ".|..||..|."
  , ".L--JL--J."
  , ".........."
  ]
example5 = T.unlines
  [ ".F----7F7F7F7F-7...."
  , ".|F--7||||||||FJ...."
  , ".||.FJ||||||||L7...."
  , "FJL7L7LJLJ||LJ.L-7.."
  , "L--J.L7...LJS7F-7L7."
  , "....F-J..F7FJ|L7L7L7"
  , "....L7.F7||L7|.L7L7|"
  , ".....|FJLJ|FJ|F7|.LJ"
  , "....FJL-7.||.||||..."
  , "....L---J.LJ.LJLJ..."
  ]
example6 = T.unlines
  [ "FF7FSF7F7F7F7F7F---7"
  , "L|LJ||||||||||||F--J"
  , "FL-7LJLJ||||||LJL-77"
  , "F--JF--7||LJLJ7F7FJ-"
  , "L---JF-JLJ.||-FJLJJ7"
  , "|F|F-JF---7F7-L7L|7|"
  , "|FFJF7L7F-JF7|JL---7"
  , "7-L-JL7||F7|L7F-7F7|"
  , "L.L7LFJ|||||FJL7||LJ"
  , "L7JLJL-JLJLJL--JLJ.L"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            fst <$> solve example1 `shouldBe` Just 4
            fst <$> solve example2 `shouldBe` Just 8
    describe "part 2" $ do
        it "examples" $ do
            snd <$> solve example3 `shouldBe` Just 4
            snd <$> solve example4 `shouldBe` Just 4
            snd <$> solve example5 `shouldBe` Just 8
            snd <$> solve example6 `shouldBe` Just 10
