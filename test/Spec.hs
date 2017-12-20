{-# LANGUAGE OverloadedStrings #-}

import Data.Int
import Formatting as F
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Regression tests"
    (do describe
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
                   "-9223372036854775808")))
  describe
    "Floating point"
    (do it "Fixed" (shouldBe (format (fixed 4) (12.123456 :: Double)) "12.1235")
        it
          "Variable"
          (shouldBe (format float (12.123456 :: Double)) "12.123456"))
