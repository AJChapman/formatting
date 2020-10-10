{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -Wno-type-defaults #-}

import Control.Monad
import Data.Char (isSpace)
import Data.Int
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Monoid
import Data.Scientific
import qualified Data.Semigroup
import qualified Data.Text.Lazy as LT
import Formatting as F
import Formatting.Time
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Regression tests" $ do
    describe "https://github.com/AJChapman/formatting/issues/36" $ do
      it "format (later id <> later id) \"x\"" $ format (later id Data.Monoid.<> later id)    "x" `shouldBe` "xx"
      it "format (later id <> later id) \"x\"" $ format (later id Data.Semigroup.<> later id) "x" `shouldBe` "xx"

    describe "https://github.com/AJChapman/formatting/issues/31" $
      it "10^6-1" $ F.format F.int (10 ^ (16 :: Int) - 1 :: Int) `shouldBe` "9999999999999999"

    describe "https://github.com/AJChapman/formatting/issues/28" $ do
      it "-100"         $ sformat (groupInt 3 ',') (-100 :: Int)       `shouldBe` "-100"
      it "-100,000,000" $ sformat (groupInt 3 ',') (-100000000 :: Int) `shouldBe` "-100,000,000"
      it "100,000,000"  $ sformat (groupInt 3 ',') (-100000000 :: Int) `shouldBe` "-100,000,000"

    describe "https://github.com/bos/text-format/issues/18" $ do
      it "build (minBound :: Int)" $ format build (minBound :: Int64) `shouldBe` "-9223372036854775808"
      it "build (maxBound :: Int)" $ format build (maxBound :: Int)   `shouldBe` "9223372036854775807"

    describe "https://github.com/AJChapman/formatting/issues/62" $ do
      it "left 3 '0' (0 :: Int)"  $ format (left 3 '0') (0 ::Int)  `shouldBe` "000"
      it "left 3 '0' (0 :: Word)" $ format (left 3 '0') (0 ::Word) `shouldBe` "000"

    describe "https://github.com/AJChapman/formatting/issues/60" $
      it "build (minBound :: Word)" $ format build (minBound :: Word) `shouldBe` "0"

    describe "https://github.com/AJChapman/formatting/issues/59" $
      it "shortest not scientific" $ format shortest (0.01 :: Double) `shouldBe` "0.01"

  describe "Floating point" $ do
    it "Fixed"    $ format (fixed 4) (12.123456 :: Double) `shouldBe` "12.1235"
    it "Variable" $ format float     (12.123456 :: Double) `shouldBe` "12.123456"
    it "Shortest" $ format shortest  (12.0000 :: Double) `shouldBe` "12"

  describe "Scientific" $ do
    it "sci" $ format sci (scientific 60221409 16) `shouldBe` "6.0221409e23"
    it "scifmt" $ format (scifmt Exponent (Just 3)) (scientific 60221409 16) `shouldBe` "6.022e23"
    it "scifmt" $ format (scifmt Exponent Nothing) (scientific 60221409 16) `shouldBe` "6.0221409e23"
    it "scifmt" $ format (scifmt Fixed Nothing) (scientific 60221409 16) `shouldBe` "602214090000000000000000.0"
    it "scifmt" $ format (scifmt Generic (Just 5)) (scientific 60221409 16) `shouldBe` "6.02214e23"

  describe "Bytes" $ do
    it "1KB" $ format (bytes shortest) (1024 :: Int) `shouldBe` "1KB"
    it "1.15GB" $ format (bytes (fixed 2)) (1234567890 :: Int) `shouldBe` "1.15GB"

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

      forM_ tests $ \(input, output) -> it output $ format ords input `shouldBe` LT.pack output

  describe "plural" $ do
    let formatPeople = format (int % " " <> plural "person" "people" % ".")
    it "formats a person" $ formatPeople (1 :: Int) `shouldBe` "1 person."
    it "formats a person" $ formatPeople (3 :: Int) `shouldBe` "3 people."

  describe "diffComponents" $ do
    it "59s" $ flip shouldBe "00:00:00:59" $ format diffComponents (59 :: Double)
    it "minute" $ flip shouldBe "00:00:01:00" $ format diffComponents (60 :: Double)
    it "90s" $ flip shouldBe "00:00:01:30" $ format diffComponents (90 :: Double)
    it "hour" $ flip shouldBe "00:01:00:00" $ format diffComponents (3600 :: Double)
    it "day" $ flip shouldBe "01:00:00:00" $ format diffComponents (86400 :: Double)

  describe "list formatters" $ do
    it "concatenated" $ format (concatenated text) ["one", "two", "three"] `shouldBe` "onetwothree"
    it "joinedWith" $ format (joinedWith (mconcat . reverse) int) [123, 456, 789] `shouldBe` "789456123"
    it "intercalated" $ format (intercalated "||" int) [1, 2, 3]  `shouldBe` "1||2||3"
    it "unworded" $ format (unworded int) [1, 2, 3] `shouldBe` "1 2 3"
    it "unlined" $ format (unlined char) ['a'..'c'] `shouldBe` "a\nb\nc\n"
    it "spaced" $ format (spaced int) [1, 2, 3] `shouldBe` "1 2 3"
    it "commaSep" $ format (took 5 (commaSep int)) [1..] `shouldBe` "1,2,3,4,5"
    it "commaSpaceSep" $ format (took 3 (commaSpaceSep ords)) [1..] `shouldBe` "1st, 2nd, 3rd"
    it "list" $ format (list stext) ["one", "two", "three"] `shouldBe` "[one, two, three]"
    it "qlist" $ format (qlist stext) ["one", "two", "three"] `shouldBe` "[\"one\", \"two\", \"three\"]"
    it "took" $ format (took 7 (list bin)) [1..] `shouldBe` "[1, 10, 11, 100, 101, 110, 111]"
    it "dropped" $ format (dropped 3 (list int)) [1..6] `shouldBe` "[4, 5, 6]"

  describe "splitting formatters" $ do
    it "splat" $ format (splat isSpace commaSpaceSep stext) "This\t  is\n\t\t  poorly formatted   " `shouldBe` "This, , , is, , , , , poorly, formatted, , , "
    it "splatWith" $ format (splatWith (LT.chunksOf 3) list int) 1234567890 `shouldBe` "[123, 456, 789, 0]"
    it "splatOn" $ format (splatOn "," unlined text) "one,two,three" `shouldBe` "one\ntwo\nthree\n"
    it "worded" $ format (worded list text) "one  two three  " `shouldBe` "[one, two, three]"
    it "lined" $ format (lined qlist text) "one two three\n\nfour five six\nseven eight nine\n\n" `shouldBe` "[\"one two three\", \"\", \"four five six\", \"seven eight nine\", \"\"]"

  describe "altering combinators" $ do
    it "alteredWith" $ format (alteredWith LT.reverse int) 123456 `shouldBe` "654321"
    it "replaced" $ format (replaced "Bruce" "<redacted>" stext) "Bruce replied that Bruce's name was, in fact, '<redacted>'." `shouldBe` "<redacted> replied that <redacted>'s name was, in fact, '<redacted>'."
    it "uppercased" $ format (uppercased text) "I'm not shouting, you're shouting." `shouldBe` "I'M NOT SHOUTING, YOU'RE SHOUTING."
    it "lowercased" $ format (lowercased text) "Cd SrC/; Rm -Rf *" `shouldBe` "cd src/; rm -rf *"
    it "titlecased" $ format (titlecased string) "the life of brian" `shouldBe` "The Life Of Brian"
    it "ltruncated" $ format (ltruncated 5 text) "hellos" `shouldBe` "he..."
    it "ltruncated, non-truncated" $ format (ltruncated 5 text) "hello" `shouldBe` "hello"
    it "rtruncated" $ format (rtruncated 5 text) "hellos" `shouldBe` "...os"
    it "rtruncated, non-truncated" $ format (rtruncated 5 text) "hello" `shouldBe` "hello"
    it "ctruncated" $ format (ctruncated 15 4 text) "The quick brown fox jumps over the lazy dog." `shouldBe` "The quick brown...dog."
    it "ctruncated, non-truncated" $ format (ctruncated 15 4 text) "The quick brown fox" `shouldBe` "The quick brown fox"
    it "lpadded" $ format (lpadded 7 ' ' int) 1 `shouldBe` "      1"
    it "lpadded doesn't shorten" $ format (lpadded 7 ' ' int) 123456789 `shouldBe` "123456789"
    it "rpadded" $ format (rpadded 7 ' ' int) 1 `shouldBe` "1      "
    it "cpadded" $ format (cpadded 7 ' ' int) 1 `shouldBe` "   1   "
    it "lfixed short" $ format (lfixed 10 ' ' int) 123 `shouldBe` "123       "
    it "lfixed at length" $ format (lfixed 10 ' ' int) 1234567890 `shouldBe` "1234567890"
    it "lfixed long" $ format (lfixed 10 ' ' int) 123456789012345 `shouldBe` "1234567..."
    it "rfixed short" $ format (rfixed 10 ' ' int) 123 `shouldBe` "       123"
    it "rfixed at length" $ format (rfixed 10 ' ' int) 1234567890 `shouldBe` "1234567890"
    it "rfixed long" $ format (rfixed 10 ' ' int) 123456789012345 `shouldBe` "...9012345"
    it "cfixed short" $ format (cfixed 4 3 ' ' int) 123 `shouldBe` "    123   "
    it "cfixed at length" $ format (cfixed 4 3 ' ' int) 1234567890 `shouldBe` "1234567890"
    it "cfixed long" $ format (cfixed 4 3 ' ' int) 123456789012345 `shouldBe` "1234...345"

  describe "wrapping combinators" $ do
    it "prefixed" $ format ("The answer is: " % prefixed "wait for it... " int) 42 `shouldBe` "The answer is: wait for it... 42"
    it "prefixed, combining" $ format (unlined (indented 4 (prefixed "- " int))) [1, 2, 3] `shouldBe` "    - 1\n    - 2\n    - 3\n"
    it "suffixed" $ format (suffixed "!!!" int) 7 `shouldBe` "7!!!"
    it "surrounded" $ format (surrounded "***" string) "glue" `shouldBe` "***glue***"
    it "enclosed" $ format (enclosed "<!--" "-->" text) "an html comment" `shouldBe` "<!--an html comment-->"
    it "squoted" $ let obj :: Maybe (Maybe Int); obj = Just Nothing in format ("The object is: " % squoted shown % ".") obj  `shouldBe` "The object is: 'Just Nothing'."
    it "dquoted" $ format ("He said it was based on " % dquoted stext % ".") "science" `shouldBe` "He said it was based on \"science\"."
    it "parenthesised" $ format (took 5 (list (parenthesised int))) [1..] `shouldBe` "[(1), (2), (3), (4), (5)]"
    it "squared" $ format (squared int) 7 `shouldBe` "[7]"
    it "braced" $ format ("\\begin" % braced text) "section" `shouldBe` "\\begin{section}"
    it "angled" $ format (list (angled text)) ["html", "head", "title", "body", "div", "span"] `shouldBe` "[<html>, <head>, <title>, <body>, <div>, <span>]"
    it "backticked" $ format ("Be sure to run " % backticked builder % " as root.") ":(){:|:&};:" `shouldBe` "Be sure to run `:(){:|:&};:` as root."

  describe "indenters" $ do
    it "indented" $ format (indented 4 int) 7 `shouldBe` "    7"
    it "indentedLines" $ format ("The lucky numbers are:\n" % indentedLines 4 int) [7, 13, 1, 42] `shouldBe` "The lucky numbers are:\n    7\n    13\n    1\n    42\n"
    it "reindented" $ format (reindented 2 text) "one\ntwo\nthree" `shouldBe` "  one\n  two\n  three\n"

  describe "numerical adapters" $ do
    it "roundedTo" $ format (list (roundedTo int)) [10.66, 6.66, 1.0, 3.4] `shouldBe` "[11, 7, 1, 3]"
    it "truncatedTo" $ format (list (truncatedTo int)) [10.66, 6.66, 1.0, 3.4] `shouldBe` "[10, 6, 1, 3]"
    it "ceilingedTo" $ format (list (ceilingedTo int)) [10.66, 6.66, 1.0, 3.4] `shouldBe` "[11, 7, 1, 4]"
    it "flooredTo" $ format (list (flooredTo int)) [10.66, 6.66, 1.0, 3.4] `shouldBe` "[10, 6, 1, 3]"

  describe "structure formatting" $
    it "accessed" $ format (accessed fst int) (1, "hello") `shouldBe` "1"
  -- describe "lens formatters" $ do
  --   it "viewed" $ flip shouldBe "(viewed _1 int) (1, "hello")" $ format

  describe "fixed-width numbers" $ do
    it "binPrefix" $ format (binPrefix 16) 4097 `shouldBe` "0b0001000000000001"
    it "octPrefix" $ format (octPrefix 16) 4097 `shouldBe` "0o0000000000010001"
    it "hexPrefix" $ format (hexPrefix 16) 4097 `shouldBe` "0x0000000000001001"
