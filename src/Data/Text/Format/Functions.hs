{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Text.Format.Functions
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Useful functions and combinators.

module Data.Text.Format.Functions
    (
      (<>)
    , i2d
    ) where

import Data.Text.Lazy.Builder (Builder)
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
import GHC.Base hiding ((<>))
#else
import GHC.Base
#endif

-- | Unsafe conversion for decimal digits.
{-# INLINE i2d #-}
i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i#))

-- | The normal 'mappend' function with right associativity instead of
-- left.
(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}

infixr 4 <>
