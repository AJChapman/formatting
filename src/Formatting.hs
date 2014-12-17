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
  sformat,
  bprint,
  fprint,
  hprint,
  -- * Formatting library
  module Formatting.Holey,
  module Formatting.Formatters,
  -- * Other functions
  formatToString
 ) where

import Formatting.Formatters
import Formatting.Holey
import Formatting.Internal
