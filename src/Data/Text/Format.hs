{-# LANGUAGE OverloadedStrings, RelaxedPolyRec #-}

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

import           Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as L
import           Data.Text (Text)
import qualified Data.Text as ST
import qualified Data.Text as T
import qualified Formatting.Buildable as B
import qualified Data.Text.Encoding as T
import           Data.Text.Format.Functions ((<>))
import           Data.Text.Format.Types (Shown(..), Hex(..))
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as LT
import           Prelude hiding (exp, print)
import           System.IO (Handle)
import           Text.Printf

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
fixed decs = B.build . T.pack . (printf ("%." ++ show decs ++ "f") :: Double->String) . realToFrac
{-# NOINLINE[0] fixed #-}

-- | Render a floating point number using the smallest number of
-- digits that correctly represent it.
shortest :: (Real a) => a -> Builder
shortest = B.build . T.decodeUtf8 . L.toStrict . L.toLazyByteString . L.doubleDec . realToFrac
{-# NOINLINE[0] shortest #-}

-- | Render an integer using hexadecimal notation.  (No leading "0x"
-- is added.)
hex :: Integral a => a -> Builder
hex = B.build . Hex
{-# INLINE hex #-}
