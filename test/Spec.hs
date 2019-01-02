{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Int
import qualified Data.Monoid
import qualified Data.Semigroup
import qualified Data.Text.Lazy as LT
import Formatting as F
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Regression tests"
    (do describe "https://github.com/chrisdone/formatting/issues/36"
                 (do it "format (later id <> later id) \"x\""
                        (shouldBe (format (later id Data.Monoid.<> later id) "x")
                                  "xx")
                     it "format (later id <> later id) \"x\""
                        (shouldBe (format (later id Data.Semigroup.<> later id) "x")
                                  "xx"))
        describe
          "https://github.com/chrisdone/formatting/issues/31"
          (do it
                "10^6-1"
                (shouldBe
                   (F.format F.int (10 ^ (16 :: Int) - 1 :: Int))
                   "9999999999999999"))
        describe
          "https://github.com/chrisdone/formatting/issues/28"
          (do it
                "-100"
                (shouldBe (sformat (groupInt 3 ',') (-100 :: Int)) "-100")
              it
                "-100,000,000"
                (shouldBe
                   (sformat (groupInt 3 ',') (-100000000 :: Int))
                   "-100,000,000")
              it
                "100,000,000"
                (shouldBe
                   (sformat (groupInt 3 ',') (-100000000 :: Int))
                   "-100,000,000"))
        describe
          "https://github.com/bos/text-format/issues/18"
          (do it
                "build (minBound :: Int)"
                (shouldBe
                   (format build (minBound :: Int64))
                   "-9223372036854775808"))
        it
          "build (maxBound :: Int)"
          (shouldBe
             (format build (maxBound :: Int))
             "9223372036854775807"))
  describe
    "Floating point"
    (do it "Fixed" (shouldBe (format (fixed 4) (12.123456 :: Double)) "12.1235")
        it
          "Variable"
          (shouldBe (format float (12.123456 :: Double)) "12.123456"))

  describe
    "Buildable a => Buildable [a]"
    (do it "\"\" :: [Char] (backwards compatibility)"
           (shouldBe (format build ("" :: String)) "")
        it "\"hi\" :: [Char] (backwards compatibility)"
           (shouldBe (format build ("hi" :: String)) "hi")
        it "[1,2,3] :: [Int]"
           (shouldBe (format build ([1,2,3] :: [Int])) "[1,2,3]")
        it "[] :: [Int]"
           (shouldBe (format build ([] :: [Int])) "[]"))

  describe "ords" $ do 
      let tests :: [(Int, String)]
          tests = [ ( 1, "1st")
                  , ( 2, "2nd")
                  , ( 3, "3rd")
                  , ( 4, "4th")
                  , ( 5, "5th")
                  , ( 6, "6th")
                  , ( 7, "7th")
                  , ( 8, "8th")
                  , ( 9, "9th")
                  , (10, "10th")
                  , (11, "11th")
                  , (12, "12th")
                  , (13, "13th")
                  , (14, "14th")
                  , (15, "15th")
                  , (16, "16th")
                  , (17, "17th")
                  , (18, "18th")
                  , (19, "19th")
                  , (20, "20th")
                  , (21, "21st")
                  , (22, "22nd")
                  , (23, "23rd")
                  , (24, "24th")
                  , (25, "25th")
                  , (26, "26th")
                  , (27, "27th")
                  , (28, "28th")
                  , (29, "29th")
                  , (30, "30th")
                  , (31, "31st")
                  , (31, "31st")
                  , (32, "32nd")
                  , (33, "33rd")
                  , (34, "34th")
                  ]

      forM_ tests $ \(input, output) -> it output $ format ords input `shouldBe` (LT.pack output)
    
