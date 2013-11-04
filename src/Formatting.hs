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
-- Combinator-based type-safe formatting (like printf() or FORMAT) for Text.
--
-- Example:
--
-- >>> format ("Person's name is " % text % ", age is " % hex) "Dave" 54
--
-- See "Formatting.Formatters" for a complete list of formatting combinators.

module Formatting
  (
  -- * Top-level functions
  format,
  bprint,
  fprint,
  hprint,
  -- * Formatting library
  module Formatting.Holey,
  module Formatting.Formatters
 ) where

import           Formatting.Holey
import           Formatting.Formatters

import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO      as T
import           System.IO

-- | Run the formatter and return a "Text" value.
format :: Holey Builder Text a -> a
format m = runHM m T.toLazyText

-- | Run the formatter and return a "Builder" value.
bprint :: Holey Builder Builder a -> a
bprint m = runHM m id

-- | Run the formatter and print out the text to stdout.
fprint :: Holey Builder (IO ()) a -> a
fprint m = runHM m (T.putStr . T.toLazyText)

-- | Run the formatter and put the output onto the given "Handle".
hprint :: Handle -> Holey Builder (IO ()) a -> a
hprint h m = runHM m (T.hPutStr h . T.toLazyText)
