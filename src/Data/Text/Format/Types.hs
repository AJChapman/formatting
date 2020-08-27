{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Data.Text.Format.Types.Internal
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for text mangling.

module Data.Text.Format.Types
    (

     Shown(..)
    -- * Integer format control
    , Hex(..)
    ) where

-- | Render an integral type in hexadecimal.
newtype Hex a = Hex a
    deriving (Eq, Ord, Read, Show, Num, Real, Enum, Integral)

-- | Render a value using its 'Show' instance.
newtype Shown a = Shown {
      shown :: a
    } deriving (Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac,
                Floating, RealFloat, Enum, Integral, Bounded)
