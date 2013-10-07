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
-- Single letters for short formatting.

module Text.Format.ShortFormatters where

import           Text.Format.Holey

import qualified Data.Text as S
import qualified Data.Text as T
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Format       as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as T

-- | Output a lazy text.
t :: Format Text
t = later T.fromLazyText

-- | Render an integer using hexadecimal notation. (No leading 0x is
-- added.)
x :: Format Integer
x = later T.hex

-- | Output a strict text.
st :: Format S.Text
st = later T.fromText

-- | Output a string.
s :: Format String
s = later (T.fromText . T.pack)

-- | Render a floating point number using scientific/engineering
-- notation (e.g. 2.3e123), with the given number of decimal places.
ef :: Real a => Int -> Format a
ef i = later (T.expt i)

-- | Render a floating point number using normal notation, with the
-- given number of decimal places.
f :: Real a => Int -> Format a
f i = later (T.fixed i)

-- | Render a floating point number, with the given number of digits
-- of precision. Uses decimal notation for values between 0.1 and
-- 9,999,999, and scientific notation otherwise.
pf :: Real a => Int -> Format a
pf i = later (T.prec i)

-- | Render a floating point number using the smallest number of
-- digits that correctly represent it.
sf :: Real a => Format a
sf = later T.shortest

-- | Pad the left hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
l :: Buildable a => Int -> Char -> Format a
l i c = later (T.left i c)

-- | Pad the right hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
r :: Buildable a => Int -> Char -> Format a
r i c = later (T.right i c)
