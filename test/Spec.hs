{-# LANGUAGE OverloadedStrings #-}

import Formatting as F
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Regression tests"
    (do describe "https://github.com/chrisdone/formatting/issues/31"
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
                   "-100,000,000")))
