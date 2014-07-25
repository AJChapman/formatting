{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

-- |
-- Module      : Formatting.Formatters
-- Copyright   : (c) 2013 Chris Done, 2013 Shachaf Ben-Kiki
-- License     : BSD3
-- Maintainer  : chrisdone@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Formatting functions.

module Formatting.Formatters
  (
  -- * Text/string types
  text,
  stext,
  string,
  shown,
  char,
  builder,
  fconst,
  -- * Numbers
  int,
  float,
  expt,
  fixed,
  prec,
  sci,
  scifmt,
  shortest,
  commas,
  ords,
  asInt,
  -- * Padding
  left,
  right,
  -- * Bases
  hex,
  -- * Buildables
  build,
  Buildable
  ) where

import           Formatting.Holey

import           Data.Monoid
import qualified Data.Text as S
import qualified Data.Text as T
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as B (build)
import qualified Data.Text.Format as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import           Data.Text.Lazy.Builder.Scientific
import           Data.Scientific

-- | Output a lazy text.
text :: Format Text
text = later T.fromLazyText

-- | Render an integer using hexadecimal notation. (No leading 0x is
-- added.)
hex :: Integral a => Format a
hex = later T.hex

-- | Output a strict text.
stext :: Format S.Text
stext = later T.fromText

-- | Output a string.
string :: Format String
string = later (T.fromText . T.pack)

-- | Output a showable value (instance of 'Show') by turning it into
-- 'Text':
--
-- >>> format ("Value number " % shown % " is " % shown % ".") 42 False
-- "Value number 42 is False."
shown :: Show a => Format a
shown = later (T.fromText . T.pack . show)

-- | Output a character.
char :: Format Char
char = later B.build

-- | Build a builder.
builder :: Format Builder
builder = later id

-- | Like `const` but for formatters.
fconst :: Builder -> Format a
fconst m = later (const m)

-- | Build anything that implements the "Buildable" class.
build :: Buildable a => Format a
build = later B.build

-- | Render an integral e.g. 123 -> \"123\", 0 -> \"0\".
int :: Integral a => Format a
int = later T.shortest

-- | Render some floating point with the usual notation, e.g. 123.32 => \"123.32\"
float :: Real a => Format a
float = later (T.shortest)

-- | Render a floating point number using scientific/engineering
-- notation (e.g. 2.3e123), with the given number of decimal places.
expt :: Real a => Int -> Format a
expt i = later (T.expt i)

-- | Render a floating point number using normal notation, with the
-- given number of decimal places.
fixed :: Real a => Int -> Format a
fixed i = later (T.fixed i)

-- | Render a floating point number, with the given number of digits
-- of precision. Uses decimal notation for values between 0.1 and
-- 9,999,999, and scientific notation otherwise.
prec :: Real a => Int -> Format a
prec i = later (T.prec i)

-- | Render a floating point number using the smallest number of
-- digits that correctly represent it.
shortest :: Real a => Format a
shortest = later T.shortest

-- | Render a scientific number.
sci :: Format Scientific
sci = later scientificBuilder

-- | Render a scientific number with options.
scifmt :: FPFormat -> Maybe Int -> Format Scientific
scifmt f i = later (formatScientificBuilder f i)

-- | Shows the Int value of Enum instances using 'fromEnum'.
--
-- >>> format ("Got: " % char % " (" % asInt % ")") 'a' 'a'
-- "Got: a (97)"
asInt :: Enum a => Format a
asInt = later (T.shortest . fromEnum)

-- | Pad the left hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
left :: Buildable a => Int -> Char -> Format a
left i c = later (T.left i c)

-- | Pad the right hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
right :: Buildable a => Int -> Char -> Format a
right i c = later (T.right i c)

-- | Add commas to an integral, e.g 12000 -> \ "12,000".
commas :: (Buildable n,Integral n) => Format n
commas = later (commaize) where
  commaize = T.fromLazyText .
             LT.reverse .
             foldr merge "" .
             LT.zip ("000" <> cycle' ",00") .
             LT.reverse .
             T.toLazyText .
             B.build
  merge (f,c) rest | f == ',' = "," <> LT.singleton c <> rest
                   | otherwise = LT.singleton c <> rest
  cycle' xs = xs <> cycle' xs

-- | Add a suffix to an integral, e.g. 1st, 2nd, 3rd, 21st.
ords :: Integral n => Format n
ords = later go
  where go n
          | tens > 3 && tens < 21 = T.shortest n <> "th"
          | otherwise =
            T.shortest n <>
            case n `mod` 10 of
              1 -> "st"
              2 -> "nd"
              3 -> "rd"
              _ -> "th"
          where tens = n `mod` 100
