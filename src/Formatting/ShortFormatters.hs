{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall #-}

-- |
-- Module      : Formatting.ShortFormatters
-- Copyright   : (c) 2013 Chris Done, 2013 Shachaf Ben-Kiki
-- License     : BSD3
-- Maintainer  : chrisdone@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Single letters for short formatting.

module Formatting.ShortFormatters
  ( t
  , d
  , b
  , o
  , x
  , st
  , s
  , sh
  , c
  , f
  , sf
  , l
  , r
  ) where

import           Formatting.Formatters (bin, int, oct)
import           Formatting.Internal

import qualified Data.Text as S
import qualified Data.Text as T
import qualified Data.Text.Format as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as T
import           Formatting.Buildable (Buildable)
import qualified Formatting.Buildable as B (build)

-- | Output a lazy text.
t :: Format r (Text -> r)
t = later T.fromLazyText

-- | Render an integral e.g. 123 -> \"123\", 0 -> \"0\".
d :: Integral a => Format r (a -> r)
d = int

-- | Render an integer using binary notation. (No leading 0b is
-- added.)
b :: Integral a => Format r (a -> r)
b = bin

-- | Render an integer using octal notation. (No leading 0o is added.)
o :: Integral a => Format r (a -> r)
o = oct

-- | Render an integer using hexadecimal notation. (No leading 0x is
-- added.)
x :: Integral a => Format r (a -> r)
x = later T.hex

-- | Output a strict text.
st :: Format r (S.Text -> r)
st = later T.fromText

-- | Output a string.
s :: Format r (String -> r)
s = later (T.fromText . T.pack)

-- | Output a showable value (instance of 'Show') by turning it into
-- 'Text'.
sh :: Show a => Format r (a -> r)
sh = later (T.fromText . T.pack . show)

-- | Output a character.
c :: Format r (Char -> r)
c = later B.build

-- | Render a floating point number using normal notation, with the
-- given number of decimal places.
f :: Real a => Int -> Format r (a -> r)
f i = later (T.fixed i)

-- | Render a floating point number using the smallest number of
-- digits that correctly represent it.
sf :: Real a => Format r (a -> r)
sf = later T.shortest

-- | Pad the left hand side of a string until it reaches @k@ characters
-- wide, if necessary filling with character @ch@.
l :: Buildable a => Int -> Char -> Format r (a -> r)
l i ch = later (T.left i ch)

-- | Pad the right hand side of a string until it reaches @k@ characters
-- wide, if necessary filling with character @ch@.
r :: Buildable a => Int -> Char -> Format r (a -> r)
r i ch = later (T.right i ch)
