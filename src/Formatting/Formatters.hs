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
  builder,
  -- * Numbers
  int,
  float,
  expt,
  fixed,
  prec,
  shortest,
  -- * Padding
  left,
  right,
  -- * Bases
  hex,
  -- * Buildables
  Buildable
  ) where

import           Formatting.Holey

import qualified Data.Text as S
import qualified Data.Text as T
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Format       as T
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T

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

-- | Build a builder.
builder :: Format Builder
builder = later id

-- | Render an integral e.g. 123 -> \"123\", 0 -> \"0\".
int :: Integral a => Format a
int = later T.shortest

-- | Render some floating point with the usual notation, e.g. 123.32 => "123.32"
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

-- | Pad the left hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
left :: Buildable a => Int -> Char -> Format a
left i c = later (T.left i c)

-- | Pad the right hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
right :: Buildable a => Int -> Char -> Format a
right i c = later (T.right i c)
