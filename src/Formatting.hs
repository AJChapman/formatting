{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall #-}

-- $setup
-- >>> :set -XOverloadedStrings

-- |
-- Module      : Text.Format
-- Copyright   : (c) 2013 Chris Done, 2013 Shachaf Ben-Kiki
-- License     : BSD3
-- Maintainer  : alex@farfromthere.net
-- Stability   : experimental
-- Portability : GHC
--
-- Combinator-based type-safe formatting (like printf() or FORMAT) for Text.
--
-- Example:
--
-- >>> format ("Person's name is " % text % ", age is " % hex) "Dave" 54
-- "Person's name is Dave, age is 36"
--
-- See "Formatting.Formatters" for a list of formatters.
-- See "Formatting.Combinators" for a list of formatting combinators, for combining and altering formatters.

module Formatting
  (
  Format,
  (%),
  (%.),
  now,
  later,
  mapf,
  -- * Top-level functions
  runFormat,
  format,
  sformat,
  bprint,
  bformat,
  fprint,
  fprintLn,
  hprint,
  hprintLn,
  formatToString,
  formatted,
  -- * Formatting library
  module Formatting.Formatters,
  module Formatting.Combinators
 ) where

import Formatting.Formatters
import Formatting.Combinators
import Formatting.FromBuilder
import Formatting.Internal
