{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Char (isSpace)
import Data.Int
import qualified Data.Monoid
import qualified Data.Semigroup
import qualified Data.Text.Lazy as LT
import Formatting as F
import Formatting.Time
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

  describe "diffComponents" $ do
    it "59s" $ flip shouldBe "00:00:00:59" $ format diffComponents 59
    it "minute" $ flip shouldBe "00:00:01:00" $ format diffComponents 60
    it "90s" $ flip shouldBe "00:00:01:30" $ format diffComponents 90
    it "hour" $ flip shouldBe "00:01:00:00" $ format diffComponents 3600
    it "day" $ flip shouldBe "01:00:00:00" $ format diffComponents 86400

  describe "list formatters" $ do
    it "concatenated" $ flip shouldBe "onetwothree" $ format (concatenated text) ["one", "two", "three"]
    it "intercalated" $ flip shouldBe "1||2||3" $ format (intercalated "||" int) [1, 2, 3] 
    it "unworded" $ flip shouldBe "1 2 3" $ format (unworded int) [1, 2, 3] 
    it "unlined" $ flip shouldBe "a\nb\nc\n" $ format (unlined char) ['a'..'c'] 
    it "spaced" $ flip shouldBe "1 2 3" $ format (spaced int) [1, 2, 3] 
    it "commaSep" $ flip shouldBe "1,2,3,4,5" $ format (took 5 (commaSep int)) [1..] 
    it "commaSpaceSep" $ flip shouldBe "1st, 2nd, 3rd" $ format (took 3 (commaSpaceSep ords)) [1..] 
    it "list" $ flip shouldBe "[one, two, three]" $ format (list stext) ["one", "two", "three"] 
    it "took" $ flip shouldBe "[1, 10, 11, 100, 101, 110, 111]" $ format (took 7 (list bin)) [1..] 

  describe "splitting formatters" $ do
    it "splat" $ flip shouldBe "This, , , is, , , , , poorly, formatted, , , " $ format (splat isSpace commaSpaceSep stext) "This\t  is\n\t\t  poorly formatted   "
    it "splatWith" $ flip shouldBe "[123, 456, 789, 0]" $ format (splatWith (LT.chunksOf 3) list int) 1234567890
    it "splatOn" $ flip shouldBe "one\ntwo\nthree\n" $ format (splatOn "," unlined text) "one,two,three"
    it "worded" $ flip shouldBe "[one, two, three]" $ format (worded list text) "one  two three  "
    it "lined" $ flip shouldBe "[one two three, , four five six, seven eight nine, ]" $ format (lined list text) "one two three\n\nfour five six\nseven eight nine\n\n"

  describe "altering combinators" $ do
    it "alteredWith" $ flip shouldBe "654321" $ format (alteredWith LT.reverse int) 123456
    it "replaced" $ flip shouldBe "<redacted> replied that <redacted>'s name was, in fact, '<redacted>'." $ format (replaced "Bruce" "<redacted>" stext) "Bruce replied that Bruce's name was, in fact, '<redacted>'."
    it "uppercased" $ flip shouldBe "I'M NOT SHOUTING, YOU'RE SHOUTING." $ format (uppercased text) "I'm not shouting, you're shouting."
    it "lowercased" $ flip shouldBe "cd src/; rm -rf *" $ format (lowercased text) "Cd SrC/; Rm -Rf *"
    it "titlecased" $ flip shouldBe "The Life Of Brian" $ format (titlecased string) "the life of brian"
    it "truncated" $ flip shouldBe "he..." $ format (truncated 5 text) "hellos"
    it "truncated, non-truncated" $ flip shouldBe "hello" $ format (truncated 5 text) "hello"
    it "midTruncated" $ flip shouldBe "The quick brown...dog." $ format (midTruncated 15 4 text) "The quick brown fox jumps over the lazy dog."
    it "midTruncated, non-truncated" $ flip shouldBe "The quick brown fox" $ format (midTruncated 15 4 text) "The quick brown fox"

  describe "wrapping combinators" $ do
    it "prefixed" $ flip shouldBe "The answer is: wait for it... 42" $ format ("The answer is: " % prefixed "wait for it... " int) 42
    it "prefixed, combining" $ flip shouldBe "    - 1\n    - 2\n    - 3\n" $ format (unlined (indented 4 (prefixed "- " int))) [1, 2, 3]
    it "suffixed" $ flip shouldBe "7!!!" $ format (suffixed "!!!" int) 7
    it "surrounded" $ flip shouldBe "***glue***" $ format (surrounded "***" string) "glue"
    it "enclosed" $ flip shouldBe "<!--an html comment-->" $ format (enclosed "<!--" "-->" text) "an html comment"
    it "quoted" $ flip shouldBe "The object is: 'Just Nothing'." $ let obj :: Maybe (Maybe Int); obj = Just Nothing in format ("The object is: " % quoted shown % ".") obj 
    it "doubleQuoted" $ flip shouldBe "He said it was based on \"science\"." $ format ("He said it was based on " % doubleQuoted stext % ".") "science" 
    it "parenthesised" $ flip shouldBe "[(1), (2), (3), (4), (5)]" $ format (took 5 (list (parenthesised int))) [1..]
    it "squareBracketed" $ flip shouldBe "[7]" $ format (squareBracketed int) 7
    it "curlyBracketed" $ flip shouldBe "\\begin{section}" $ format ("\\begin" % curlyBracketed text) "section"
    it "angleBracketed" $ flip shouldBe "[<html>, <head>, <title>, <body>, <div>, <span>]" $ format (list (angleBracketed text)) ["html", "head", "title", "body", "div", "span"]
    it "backticked" $ flip shouldBe "Be sure to run `:(){:|:&};:` as root." $ format ("Be sure to run " % backticked builder % " as root.") ":(){:|:&};:"

  describe "indenters" $ do
    it "indented" $ flip shouldBe "    7" $ format (indented 4 int) 7
    it "indentedLines" $ flip shouldBe "The lucky numbers are:\n    7\n    13\n    1\n    42\n" $ format ("The lucky numbers are:\n" % indentedLines 4 int) [7, 13, 1, 42]
    it "reindented" $ flip shouldBe "  one\n  two\n  three\n" $ format (reindented 2 text) "one\ntwo\nthree"

  describe "numerical adapters" $ do
    it "roundedTo" $ flip shouldBe "[11, 7, 1, 3]" $ format (list (roundedTo int)) [10.66, 6.66, 1.0, 3.4]
    it "truncatedTo" $ flip shouldBe "[10, 6, 1, 3]" $ format (list (truncatedTo int)) [10.66, 6.66, 1.0, 3.4]
    it "ceilingedTo" $ flip shouldBe "[11, 7, 1, 4]" $ format (list (ceilingedTo int)) [10.66, 6.66, 1.0, 3.4]
    it "flooredTo" $ flip shouldBe "[10, 6, 1, 3]" $ format (list (flooredTo int)) [10.66, 6.66, 1.0, 3.4]

  -- describe "lens formatters" $ do
  --   it "viewed" $ flip shouldBe "(viewed _1 int) (1, "hello")" $ format 
