{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

-- |
-- Module      : Text.Format
-- Copyright   : (c) 2013 Chris Done, 2013 Shachaf Ben-Kiki
-- License     : BSD3
-- Maintainer  : chrisdone@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Formatting functions.

module Text.Format.Formatters
  (
  -- * Formatters
  text,
  hex,
  stext,
  string,
  expt,
  fixed,
  prec,
  shortest,
  left,
  right
  ) where

import           Text.Format.Holey

import qualified Data.Text as S
import qualified Data.Text as T
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Format       as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as T

-- | Output a lazy text.
text :: Format Text
text = later T.fromLazyText

-- | Render an integer using hexadecimal notation. (No leading 0x is
-- added.)
hex :: Format Integer
hex = later T.hex

-- | Output a strict text.
stext :: Format S.Text
stext = later T.fromText

-- | Output a string.
string :: Format String
string = later (T.fromText . T.pack)

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
