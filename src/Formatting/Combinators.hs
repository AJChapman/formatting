{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Formatting.Combinators
Copyright   : (c) 2020 Alex Chapman
License     : BSD3
Maintainer  : alex@farfromthere.net
Stability   : experimental
Portability : GHC
Description : Formatting combinators for building new formatters, with some useful pre-defined formatters.

A formatting combinator takes a Format and returns another Format.
Generally we want to change what the original format takes as its *input*,
leaving the output polymorphic.
Many of these combinators can be chained together to form a single 'Format'.

Implementation detail: in order to be able to chain multiple combinators to make a single 'Format' we need them all to use the same intermediate string type, and we have chosen 'Builder'.
This does not tie you to using 'Builder's, because the final output string type 'r' is still polymorphic.
|-}
module Formatting.Combinators
  (
  -- * Formatting common containers
    maybed
  , optioned
  , eithered
  , lefted
  , righted

  -- * Formatting lists of data
  , concatenated
  , joinedWith
  , intercalated
  , unworded
  , unlined
  , spaced
  , commaSep
  , commaSpaceSep
  , list
  , qlist
  , took
  , dropped

  -- * Splitting strings to pass to other formatters
  , splat
  , splatWith
  , splatOn
  , worded
  , lined

  -- * Altering formatted strings
  , alteredWith
  , replaced
  , uppercased
  , lowercased
  , titlecased
  , ltruncated
  , ctruncated
  , rtruncated
  , lpadded
  , rpadded
  , cpadded
  , lfixed
  , rfixed
  , cfixed

  -- * Wrapping formatted strings
  , prefixed
  , suffixed
  , surrounded
  , enclosed
  , squoted
  , dquoted
  , parenthesised
  , squared
  , braced
  , angled
  , backticked

  -- * Changing indentation
  , indented
  , indentedLines
  , reindented

  -- * Numerical adapters
  , roundedTo
  , truncatedTo
  , ceilingedTo
  , flooredTo

  -- * Structure formatting
  , viewed
  , accessed

  -- * Fixed-width number formatting
  , binPrefix
  , octPrefix
  , hexPrefix
  ) where

import Control.Applicative (Const(..), getConst)
import Control.Category ((>>>))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Int (Int64)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB
import Formatting.Internal
import Formatting.Formatters

-- | Render a Maybe value either as a default (if Nothing) or using the given formatter:
--
-- >>> format (maybed "Goodbye" text) Nothing
-- "Goodbye"
--
-- >>> format (maybed "Goodbye" text) (Just "Hello")
-- "Hello"
maybed
  :: Builder -- ^ The value to use when the input is Nothing
  -> Format Builder (a -> Builder) -- ^ The formatter to use on the value in a Just
  -> Format r (Maybe a -> r)
maybed whenNothing f = later $ \case
  Nothing -> whenNothing
  Just x -> bformat f x

-- | Render the value in a Maybe using the given formatter, or produce an empty string:
--
-- >>> format (optioned text) Nothing
-- ""
--
-- >>> format (optioned text) (Just "Hello")
-- "Hello"
optioned :: Format Builder (a -> Builder) -> Format r (Maybe a -> r)
optioned = maybed ""

-- | Render the value in an Either:
--
-- >>> format (eithered text int) (Left "Error!"
-- "Error!"
--
-- >>> format (eithered text int) (Right 69)
-- "69"
eithered
  :: Format Builder (a -> Builder) -- ^ The formatter to use on a value in a Left
  -> Format Builder (b -> Builder) -- ^ The formatter to use on a value in a Right
  -> Format r (Either a b -> r)
eithered l r = later $ \case
  Left x -> bformat l x
  Right x -> bformat r x

-- | Render the value in a Left with the given formatter, rendering a Right as an empty string:
--
-- >>> format (lefted text) (Left "bingo")
-- "bingo"
--
-- >>> format (lefted text) (Right 16)
-- ""
lefted :: Format Builder (a -> Builder) -> Format r (Either a x -> r)
lefted f = eithered f (fconst "")

-- | Render the value in a Right with the given formatter, rendering a Left as an empty string:
--
-- >>> format (righted text) (Left 16)
-- ""
--
-- >>> format (righted text) (Right "bingo")
-- "bingo"
righted :: Format Builder (a -> Builder) -> Format r (Either x a -> r)
righted = eithered (fconst "")

-- | Format each value in a list and concatenate them all:
--
-- >>> format (concatenated text) ["one", "two", "three"]
-- "onetwothree"
--
-- >>> format (took 15 (concatenated bin)) [1..]
-- "1101110010111011110001001101010111100110111101111"
concatenated :: Foldable t => Format Builder (a -> Builder) -> Format r (t a -> r)
concatenated f = later $ foldMap (bformat f)

-- | Use the given text-joining function to join together the individually rendered items of a list.
--
-- >>> format (joinedWith (mconcat . reverse) int) [123, 456, 789]
-- "789456123"
joinedWith :: Foldable t => ([Text] -> Text) -> Format Builder (a -> Builder) -> Format r (t a -> r)
joinedWith joiner f = later $ toList
  >>> fmap (bformat f >>> TLB.toLazyText)
  >>> joiner
  >>> TLB.fromLazyText

-- | Format each value in a list and place the given string between each:
--
-- >>> fprintLn (intercalated "||" int) [1, 2, 3]
-- 1||2||3
intercalated :: Foldable t => Text -> Format Builder (a -> Builder) -> Format r (t a -> r)
intercalated s = joinedWith (TL.intercalate s)

-- | Format each value in a list with spaces in between:
--
-- >>> format (unworded int) [1, 2, 3]
-- "1 2 3"
unworded :: Foldable t => Format Builder (a -> Builder) -> Format r (t a -> r)
unworded = joinedWith TL.unwords

-- | Format each value in a list, placing each on its own line:
--
-- >>> fprint (unlined char) ['a'..'c']
-- a
-- b
-- c
unlined :: Foldable t => Format Builder (a -> Builder) -> Format r (t a -> r)
unlined = joinedWith TL.unlines

-- | Separate the formatted items of the Foldable (e.g. list) with spaces:
--
-- >>> format (spaced int) [1, 2, 3]
-- "1 2 3"
--
-- Note that this behaviour is identical to 'unworded', it's just a different way of thinking about it.
spaced :: Foldable t => Format Builder (a -> Builder) -> Format r (t a -> r)
spaced = intercalated " "

-- | Separate the formatted items of the Foldable (e.g. list) with commas:
--
-- >>> format (commaSep stext) ["one", "two", "three", "four", "five"]
-- "one,two,three,four,five"
--
-- >>> format (took 5 (commaSep int)) [1..]
-- "1,2,3,4,5"
commaSep :: Foldable t => Format Builder (a -> Builder) -> Format r (t a -> r)
commaSep = intercalated ","

-- | Separate the formatted items of the Foldable (e.g. list) with commas and spaces:
--
-- >>> format (took 3 (commaSpaceSep ords)) [1..]
-- "1st, 2nd, 3rd"
commaSpaceSep :: Foldable t => Format Builder (a -> Builder) -> Format r (t a -> r)
commaSpaceSep = intercalated ", "

-- | Add square brackets around the Foldable (e.g. a list), and separate each formatted item with a comma and space.
--
-- >>> format (list stext) ["one", "two", "three"]
-- "[one, two, three]"
--
-- >>> format (list shown) ["one", "two", "three"]
-- "[\"one\", \"two\", \"three\"]"
list :: Foldable t => Format Builder (a -> Builder) -> Format r (t a -> r)
list = commaSpaceSep >>> squared

-- | Like 'list', but also put double quotes around each rendered item:
--
-- >>> fprintLn (qlist stext) ["one", "two", "three"]
-- ["one", "two", "three"]
qlist :: Foldable t => Format Builder (a -> Builder) -> Format r (t a -> r)
qlist = dquoted >>> commaSpaceSep >>> squared

-- | Take only the first n items from the list of items.
--
-- >>> format (took 7 (list bin)) [1..]
-- "[1, 10, 11, 100, 101, 110, 111]"
--
-- >>> format (list bin) (take 7 [1..])
-- "[1, 10, 11, 100, 101, 110, 111]"
took :: Int -> Format r ([a] -> r) -> Format r ([a] -> r)
took n = fmap (. take n)

-- | Drop the first n items from the list of items.
--
-- >>> format (dropped 3 (list int) [1..6]
-- "[4, 5, 6]"
dropped :: Int -> Format r ([a] -> r) -> Format r ([a] -> r)
dropped n = fmap (. drop n)

-- | Utility for taking a text-splitting function and turning it into a formatting combinator.
--
-- >>> format (splatWith (TL.chunksOf 3) list int) 1234567890
-- "[123, 456, 789, 0]"
splatWith
  :: (Text -> [Text]) -- ^ The text splitter
  -> (Format r' (Builder -> r') -> Format Builder ([Builder] -> Builder)) -- ^ A list-formatting combinator, e.g. 'unworded', 'list', 'concatenated', etc.
  -> Format r a -- ^ The base formatter, whose rendered text will be split
  -> Format r a
splatWith splitter lf f = later (TLB.toLazyText
  >>> splitter
  >>> fmap TLB.fromLazyText
  >>> bformat (lf builder))
  %. f

-- | Split the formatted item in places the given predicated matches, and use the given list combinator to render the resultant list of strings
-- (this function was sent to us from a parallel universe in which splat is the past participle of split, e.g. "whoops, I splat my pants").
--
-- >>> format (splat Data.Char.isSpace commaSpaceSep stext) "This\t  is\n\t\t  poorly formatted   "
-- "This, , , is, , , , , poorly, formatted, , , "
splat
  :: (Char -> Bool) -- ^ Whether to split the string at this character
  -> (Format r' (Builder -> r') -> Format Builder ([Builder] -> Builder)) -- ^ A list-formatting combinator, e.g. 'unworded', 'list', 'concatenated', etc.
  -> Format r a -- ^ The base formatter, whose rendered text will be split
  -> Format r a
splat p = splatWith (TL.split p)

-- | Split the formatted item at instances of the given string, and use the given list combinator to render the resultant list of strings.
--
-- >>> fprint (splatOn "," unlined text) "one,two,three"
-- one
-- two
-- three
--
-- >>> fprint (splatOn "," indentedLines text) "one,two,three"
--     one
--     two
--     three
splatOn
  :: Text -- ^ The text to split on
  -> (Format r' (Builder -> r') -> Format Builder ([Builder] -> Builder)) -- ^ A list-formatting combinator, e.g. 'unworded', 'list', 'concatenated', etc.
  -> Format r a -- ^ The base formatter, whose rendered text will be split
  -> Format r a
splatOn t = splatWith (TL.splitOn t)

-- | Split the formatted item into words and use the given list combinator to render the resultant list of strings.
--
-- >>> format (worded list text) "one  two three  "
-- "[one, two, three]"
worded
  :: (Format r' (Builder -> r') -> Format Builder ([Builder] -> Builder)) -- ^ A list-formatting combinator, e.g. 'unworded', 'list', 'concatenated', etc.
  -> Format r a -- ^ The base formatter, whose rendered text will be split
  -> Format r a
worded = splatWith TL.words

-- | Split the formatted item into lines and use the given list combinator to render the resultant list of strings.
--
-- >>> fprintLn (lined qlist text) "one two three\n\nfour five six\nseven eight nine\n\n"
-- ["one two three", "", "four five six", "seven eight nine", ""]
lined
  :: (Format Builder (Builder -> Builder) -> Format Builder ([Builder] -> Builder)) -- ^ A list-formatting combinator, e.g. 'unworded', 'list', 'concatenated', etc.
  -> Format r a -- ^ The base formatter, whose rendered text will be split
  -> Format r a
lined = splatWith TL.lines

-- | Alter the formatted string with the given function.
--
-- >>> format (alteredWith Data.Text.Lazy.reverse int) 123456
-- "654321"
alteredWith :: (Text -> Text) -> Format r a -> Format r a
alteredWith alterer f =
  later (TLB.toLazyText >>> alterer >>> TLB.fromLazyText) %. f

-- | Take a formatter and replace the given needle with the given replacement in its output.
--
-- >>> format (replaced "Bruce" "<redacted>" stext) "Bruce replied that Bruce's name was, in fact, '<redacted>'."
-- "<redacted> replied that <redacted>'s name was, in fact, '<redacted>'."
replaced :: Text -> Text -> Format r a -> Format r a
replaced needle replacement = alteredWith (TL.replace needle replacement)

-- | Convert any letters in the output of the given formatter to upper-case.
--
-- >>> format (uppercased text) "I'm not shouting, you're shouting."
-- "I'M NOT SHOUTING, YOU'RE SHOUTING."
uppercased :: Format r a -> Format r a
uppercased = alteredWith TL.toUpper

-- | Convert any letters in the output of the given formatter to lower-case.
--
-- >>> format (lowercased text) "Cd SrC/; Rm -Rf *"
-- "cd src/; rm -rf *"
lowercased :: Format r a -> Format r a
lowercased = alteredWith TL.toLower

-- | Convert the formatted string to title case, or something like it:
--
-- >>> format (titlecased string) "the life of brian"
-- "The Life Of Brian"
titlecased :: Format r a -> Format r a
titlecased = alteredWith TL.toTitle

-- | Truncate the formatted string at the end so that it is no more than the given number of characters in length, placing an ellipsis at the end such that it does not exceed this length.
--
-- >>> format (truncated 5 text) "hello"
-- "hello"
--
-- >>> format (truncated 5 text) "hellos"
-- "he..."
ltruncated :: Int64 -> Format r a -> Format r a
ltruncated n = ctruncated (n - 3) 0

-- | Truncate the formatted string at the start so that it is no more than the given number of characters in length, placing an ellipsis at the start such that it does not exceed this length.
--
-- >>> format (rtruncated 5 text) "hello"
-- "hello"
--
-- >>> format (rtruncated 5 text) "hellos"
-- "...os"
rtruncated :: Int64 -> Format r a -> Format r a
rtruncated n = ctruncated 0 (n - 3)

-- | Truncate the formatted string in the center, leaving the given number of characters at the start and end, and placing an ellipsis in between.
-- The length will be no longer than `start + end + 3` characters long.
-- 
-- >>> format (ctruncated 15 4 text) "The quick brown fox jumps over the lazy dog."
-- "The quick brown...dog."
--
-- >>> format (ctruncated 15 4 text) "The quick brown fox"
-- "The quick brown fox"
ctruncated :: Int64 -> Int64 -> Format r a -> Format r a
ctruncated start end = alteredWith shorten
  where
    shorten :: Text -> Text
    shorten txt =
      let n = start + end + 3
      in if TL.length txt <= n
        then txt
        else TL.take start txt <> "..." <> TL.takeEnd end txt

-- | Pad the formatted string on the left with the given character to give it the given minimum width:
--
-- >>> format (lpadded 7 ' ' int) 1
-- "      1"
--
-- >>> format (lpadded 7 ' ' int) 123456789
-- "123456789"
lpadded :: Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)
lpadded i c = alteredWith (TL.justifyRight i c)

-- | Pad the formatted string on the right with the given character to give it the given minimum width:
--
-- >>> format (rpadded 7 ' ' int) 1
-- "1      "
rpadded :: Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)
rpadded i c = alteredWith (TL.justifyLeft i c)

-- | Pad the formatted string on the left and right with the given character to center it, giving it the given minimum width:
--
-- >>> format (cpadded 7 ' ' int) 1
-- "   1   "
cpadded :: Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)
cpadded i c = alteredWith (TL.center i c)

-- | Format the item with a fixed width, padding with the given character on the left to extend, adding an ellipsis on the right to shorten:
--
-- >>> format (lfixed 10 ' ' int) 123
-- "123       "
--
-- >>> format (lfixed 10 ' ' int) 1234567890
-- "1234567890"
--
-- >>> format (lfixed 10 ' ' int) 123456789012345
-- "1234567..."
lfixed :: Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)
lfixed n c = ltruncated n . rpadded n c

-- | Format the item with a fixed width, padding with the given character on the right to extend, adding an ellipsis on the right to shorten:
--
-- >>> format (rfixed 10 ' ' int) 123
-- "       123"
--
-- >>> format (rfixed 10 ' ' int) 1234567890
-- "1234567890"
--
-- >>> format (rfixed 10 ' ' int) 123456789012345
-- "...9012345"
rfixed :: Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)
rfixed n c = rtruncated n . lpadded n c

-- | Format the item with a fixed width, padding with the given character on either side to extend, adding an ellipsis in the center to shorten.
--
-- The total length will be `l + r + 3` characters.
--
-- >>> format (cfixed 4 3 ' ' int) 123
-- "    123   "
--
-- >>> format (cfixed 4 3 ' ' int) 1234567890
-- "1234567890"
--
-- >>> format (cfixed 4 3 ' ' int) 123456789012345
-- "1234...345"
cfixed :: Int64 -> Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)
cfixed l r c = ctruncated l r . cpadded (l + r + 3) c

-- | Add the given prefix to the formatted item:
--
-- >>> format ("The answer is: " % prefixed "wait for it... " int) 42
-- "The answer is: wait for it... 42"
--
-- >>> fprint (unlined (indented 4 (prefixed "- " int))) [1, 2, 3]
--     - 1
--     - 2
--     - 3
prefixed :: Builder -> Format r a -> Format r a
prefixed s f = now s % f

-- | Add the given suffix to the formatted item.
suffixed :: Builder -> Format r a -> Format r a
suffixed s f = f % now s

-- | Surround the output string with the given string:
--
-- >>> format (surrounded "***" string) "glue"
-- "***glue***"
surrounded :: Builder -> Format r a -> Format r a
surrounded s f = now s % f % now s

-- | Enclose the output string with the given strings:
--
-- >>> format (enclosed "<!--" "-->" text) "an html comment"
-- "<!--an html comment-->"
enclosed :: Builder -> Builder -> Format r a -> Format r a
enclosed pre suf f = now pre % f % now suf

-- | Add single quotes around the formatted item:
--
-- >>> let obj = Just Nothing in format ("The object is: " % squoted shown % ".") obj
-- "The object is: 'Just Nothing'."
squoted :: Format r a -> Format r a
squoted = surrounded "'"

-- | Add double quotes around the formatted item:
--
-- >>> fprintLn ("He said it was based on " % dquoted stext % ".") "science"
-- He said it was based on "science".
dquoted :: Format r a -> Format r a
dquoted = surrounded "\""

-- | Add parentheses around the formatted item:
--
-- >>> format ("We found " % parenthesised int % " discrepancies.") 17
-- "We found (17) discrepancies."
--
-- >>> fprintLn (took 5 (list (parenthesised int))) [1..]
-- [(1), (2), (3), (4), (5)]
parenthesised :: Format r a -> Format r a
parenthesised = enclosed "(" ")"

-- | Add square brackets around the formatted item:
--
-- >>> format (squared int) 7
-- "[7]"
squared :: Format r a -> Format r a
squared = enclosed "[" "]"

-- | Add curly brackets around the formatted item:
--
-- >>> format ("\\begin" % braced text) "section"
-- "\\begin{section}"
braced :: Format r a -> Format r a
braced = enclosed "{" "}"

-- | Add angle brackets around the formatted item:
--
-- >>> format (angled int) 7
-- "<7>"
--
-- >>> format (list (angled text)) ["html", "head", "title", "body", "div", "span"]
-- "[<html>, <head>, <title>, <body>, <div>, <span>]"
angled :: Format r a -> Format r a
angled = enclosed "<" ">"

-- | Add backticks around the formatted item:
--
-- >>> format ("Be sure to run " % backticked builder % " as root.") ":(){:|:&};:"
-- "Be sure to run `:(){:|:&};:` as root."
backticked :: Format r a -> Format r a
backticked = surrounded "`"

-- | Insert the given number of spaces at the start of the rendered text:
--
-- >>> format (indented 4 int) 7
-- "    7"
--
-- Note that this only indents the first line of a multi-line string.
-- To indent all lines see 'reindented'.
indented :: Int -> Format r a -> Format r a
indented n = prefixed spaces
  where
    spaces = TL.replicate (fromIntegral n) (TL.singleton ' ') & TLB.fromLazyText

-- | Format a list of items, placing one per line, indented by the given number of spaces.
--
-- >>> fprintLn ("The lucky numbers are:\n" % indentedLines 4 int) [7, 13, 1, 42]
-- The lucky numbers are:
--     7
--     13
--     1
--     42
indentedLines :: Foldable t => Int -> Format Builder (a -> Builder) -> Format r (t a -> r)
indentedLines n = unlined . indented n

-- | Indent each line of the formatted string by the given number of spaces:
--
-- >>> fprint (reindented 2 text) "one\ntwo\nthree"
--   one
--   two
--   three
reindented :: Int -> Format r a -> Format r a
reindented n = lined (indentedLines n)

-- | Take a fractional number and round it before formatting it as the given Format:
--
-- >>> format (roundedTo int) 6.66
-- "7"
-- >>> format (list (roundedTo int)) [10.66, 6.66, 1.0, 3.4]
-- "[11, 7, 1, 3]"
--
-- Note: the type variable 'f' will almost always be 'Format r', so the type of this function can be thought of as:
--
-- @
-- roundedTo :: (Integral i, RealFrac d) => Format r (i -> r) -> Format r (d -> r)
-- @
roundedTo :: (Integral i, RealFrac d, Functor f) => f (i -> r) -> f (d -> r)
roundedTo = fmap (. round)

-- | Take a fractional number and truncate it before formatting it as the given Format:
--
-- >>> format (truncatedTo int) 6.66
-- "6"
-- >>> format (list (truncatedTo int)) [10.66, 6.66, 1.0, 3.4]
-- "[10, 6, 1, 3]"
--
-- Note: the type variable 'f' will almost always be 'Format r', so the type of this function can be thought of as:
--
-- @
-- truncatedTo :: (Integral i, RealFrac d) => Format r (i -> r) -> Format r (d -> r)
-- @
truncatedTo :: (Integral i, RealFrac d, Functor f) => f (i -> r) -> f (d -> r)
truncatedTo = fmap (. truncate)

-- | Take a fractional number and ceiling it before formatting it as the given Format:
--
-- >>> format (ceilingedTo int) 6.66
-- "7"
-- >>> format (list (ceilingedTo int)) [10.66, 6.66, 1.0, 3.4]
-- "[11, 7, 1, 4]"
--
-- Note: the type variable 'f' will almost always be 'Format r', so the type of this function can be thought of as:
--
-- @
-- ceilingedTo :: (Integral i, RealFrac d) => Format r (i -> r) -> Format r (d -> r)
-- @
ceilingedTo :: (Integral i, RealFrac d, Functor f) => f (i -> r) -> f (d -> r)
ceilingedTo = fmap (. ceiling)

-- | Take a fractional number and floor it before formatting it as the given Format:
--
-- >>> format (flooredTo int) 6.66
-- "6"
-- >>> format (list (flooredTo int)) [10.66, 6.66, 1.0, 3.4]
-- "[10, 6, 1, 3]"
--
-- Note: the type variable 'f' will almost always be 'Format r', so the type of this function can be thought of as:
--
-- @
-- flooredTo :: (Integral i, RealFrac d) => Format r (i -> r) -> Format r (d -> r)
-- @
flooredTo :: (Integral i, RealFrac d, Functor f) => f (i -> r) -> f (d -> r)
flooredTo = fmap (. floor)

-- | Use the given lens to view an item, formatting it with the given formatter.
--
-- You can think of this as having the type:
--
-- @
-- 'viewed' :: 'Lens'' s a -> Format r (a -> r) -> Format r (s -> r)
-- @
--
-- >>> format (viewed _1 int) (1, "hello")
-- "1"
--
-- This is useful when combined with the Monoid instance for Format, because it allows us to give a data structure as an argument only once, and deconstruct it with the formatters:
--
-- @
-- data Person = Person
--   { _personName :: Text
--   , _personAge :: Int
--   }
-- makeLenses ''Person
--
-- me :: Person
-- me = Person "Alex" 38
--
-- format ("The person's name is " % squoted (viewed personName text) % ", and their age is " <> viewed personAge int) me
-- "The person's name is 'Alex', and their age is 38"
-- @
viewed :: ((a -> Const a b) -> s -> Const a t) -> Format r (a -> r) -> Format r (s -> r)
viewed l = fmap (. (getConst . l Const))

-- | Access an element of the structure and format it with the given formatter.
--
-- >>> format (accessed fst int) (1, "hello")
-- "1"
--
-- Repeating the example from 'viewed':
--
-- format ("The person's name is " % squoted (accessed _personName text) % ", and their age is " <> accessed _personAge int) me
-- "The person's name is 'Alex', and their age is 38"
accessed :: (s -> a) -> Format r (a -> r) -> Format r (s -> r)
accessed accessor = fmap (. accessor)

-- | Render an integer using binary notation with a leading 0b, padding with zeroes to the given width:
--
-- >>> format (binPrefix 16) 4097
-- "0b0001000000000001"
binPrefix :: Integral a => Int64 -> Format r (a -> r)
binPrefix n = "0b" % lpadded n '0' bin

-- | Render an integer using octal notation with a leading 0o, padding with zeroes to the given width:
--
-- >>> format (octPrefix 16) 4097
-- "0o0000000000010001"
octPrefix :: Integral a => Int64 -> Format r (a -> r)
octPrefix n = "0o" % lpadded n '0' oct

-- | Render an integer using octal notation with a leading 0x, padding with zeroes to the given width:
--
-- >>> format (hexPrefix 16) 4097
-- "0x0000000000001001"
hexPrefix :: Integral a => Int64 -> Format r (a -> r)
hexPrefix n = "0x" % lpadded n '0' hex
