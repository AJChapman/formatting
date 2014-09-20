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
  group,
  commas,
  ords,
  asInt,
  -- * Padding
  left,
  right,
  center,
  -- * Bases
  base,
  bin,
  oct,
  hex,
  prefixBin,
  prefixOct,
  prefixHex,
  -- * Buildables
  build,
  Buildable
  ) where

import           Formatting.Holey

import           Data.Char (chr, ord)
import           Numeric (showIntAtBase)
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

-- | Pad the left & right hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
center :: Buildable a => Int -> Char -> Format a
center i c = later centerT where
  centerT = T.fromLazyText . LT.center (fromIntegral i) c . T.toLazyText . B.build

-- | Group integral numbers, eg 123456 -> "12.34.56"
group :: (Buildable n,Integral n) =>  Int -> Char -> Format n
group i c = later (commaize) where
  commaize = T.fromLazyText .
             LT.reverse .
             foldr merge "" .
             LT.zip (zeros <> cycle' zeros') .
             LT.reverse .
             T.toLazyText .
             B.build
  zeros = LT.replicate (fromIntegral i) $ LT.singleton '0'
  zeros' = LT.singleton c <> LT.tail zeros
  merge (f,c') rest | f == c = LT.singleton c <> LT.singleton c' <> rest
                   | otherwise = LT.singleton c' <> rest
  cycle' xs = xs <> cycle' xs

-- | Add commas to an integral, e.g 12000 -> \ "12,000".
-- Should really uses locale's LC_NUMERIC information.
commas :: (Buildable n,Integral n) => Format n
commas = group 3 ','

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

-- Bases
base :: Integral a => Int -> Format a
base numBase = later (B.build . atBase numBase)
                
-- | Render an integer using binary notation. (No leading 0b is
-- added.) Defined as @bin = 'base' 2@.
bin :: Integral a => Format a
bin = base 2
{-# INLINE bin #-}

-- | Render an integer using octal notation. (No leading 0o is
-- added.) Defined as @oct = 'base' 8@.
oct :: Integral a => Format a
oct = base 8
{-# INLINE oct #-}

-- | Render an integer using hexadecimal notation. (No leading 0x is
-- added.) Has a specialized implementation. 
hex :: Integral a => Format a
hex = later T.hex
{-# INLINE hex #-}

-- | Render an integer using binary notation with a leading 0b.
prefixBin :: Integral a => Format a
prefixBin = "0b" % bin
{-# INLINE prefixBin #-}

-- | Render an integer using octal notation with a leading 0o.
prefixOct :: Integral a => Format a
prefixOct = "0o" % oct
{-# INLINE prefixOct #-}

-- | Render an integer using hexadecimal notation with a leading 0x.
prefixHex :: Integral a => Format a
prefixHex = "0x" % hex
{-# INLINE prefixHex #-}

-- The following code is mostly taken from `Numeric.Lens.' (from
-- `lens') and modified.

-- | Internal function that converts a number to a base base-2 through
-- base-36.
atBase :: Integral a => Int -> a -> String
atBase b _ | b < 2 || b > 36 = error ("base: Invalid base " ++ show b)
atBase b n =
  showSigned' (showIntAtBase (toInteger b) intToDigit') (toInteger n) ""
{-# INLINE atBase #-}

-- | A simpler variant of 'Numeric.showSigned' that only prepends a dash and
-- doesn't know about parentheses
showSigned' :: Real a => (a -> ShowS) -> a -> ShowS
showSigned' f n
  | n < 0     = showChar '-' . f (negate n)
  | otherwise = f n

-- | Like 'Data.Char.intToDigit', but handles up to base-36
intToDigit' :: Int -> Char
intToDigit' i
  | i >= 0  && i < 10 = chr (ord '0' + i)
  | i >= 10 && i < 36 = chr (ord 'a' + i - 10)
  | otherwise = error ("intToDigit': Invalid int " ++ show i)
