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
-- Examples:
--
-- >>> format ("Person's name is " %text% ", age is " %hex) "Dave" 54

module Text.Format
  (
  -- * Top-level functions
  format,
  fprint,
  hprint,
  -- * Formatting library
  module Text.Format.Holey,
  module Text.Format.Formatters
 ) where

import           Text.Format.Holey
import           Text.Format.Formatters

import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO      as T
import           System.IO

-- | Run the formatter and return a "Text" value.
format :: Holey Builder Text a -> a
format m = runHM m T.toLazyText

-- | Run the formatter and print out the text to stdout.
fprint :: Holey Builder (IO ()) a -> a
fprint m = runHM m (T.putStr . T.toLazyText)

-- | Run the formatter and put the output onto the given "Handle".
hprint :: Handle -> Holey Builder (IO ()) a -> a
hprint h m = runHM m (T.hPutStr h . T.toLazyText)
