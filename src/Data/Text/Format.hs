{-# LANGUAGE RelaxedPolyRec #-}

-- |
-- Module      : Data.Text.Format
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, efficient, flexible support for formatting text strings.

module Data.Text.Format
    (
    -- * Format control
     left
    , right
    -- ** Integers
    , hex
    -- ** Floating point numbers
    , fixed
    , shortest
    ) where

import           Data.Double.Conversion.Text
import qualified Formatting.Buildable as B
import           Data.Text.Format.Types (Hex(..))
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder
import           Prelude hiding (exp, print)

-- | Pad the left hand side of a string until it reaches @k@
-- characters wide, if necessary filling with character @c@.
left :: B.Buildable a => Int -> Char -> a -> Builder
left k c =
    fromLazyText . LT.justifyRight (fromIntegral k) c . toLazyText . B.build

-- | Pad the right hand side of a string until it reaches @k@
-- characters wide, if necessary filling with character @c@.
right :: B.Buildable a => Int -> Char -> a -> Builder
right k c =
    fromLazyText . LT.justifyLeft (fromIntegral k) c . toLazyText . B.build

-- | Render a floating point number using normal notation, with the
-- given number of decimal places.
fixed :: (Real a) =>
         Int
      -- ^ Number of digits of precision after the decimal.
      -> a -> Builder
fixed decs = fromText . toFixed decs . realToFrac
{-# NOINLINE[0] fixed #-}

-- | Render a floating point number using the smallest number of
-- digits that correctly represent it.
shortest :: Real a => a -> Builder
shortest = fromText . toShortest . realToFrac
{-# INLINE shortest #-}

-- | Render an integer using hexadecimal notation.  (No leading "0x"
-- is added.)
hex :: Integral a => a -> Builder
hex = B.build . Hex
{-# INLINE hex #-}
