{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

-- |
-- Module      : Formatting.Holey
-- Copyright   : (c) 2013 Chris Done, 2013 Shachaf Ben-Kiki
-- License     : BSD3
-- Maintainer  : chrisdone@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Copy of the holey monoids library but with constructor exported.

module Formatting.Holey
  (
  -- * Formatting library
  Format,
  Holey,
  HoleyT (..),
  (%),
  (%.),
  now,
  bind,
  later
  ) where

import Data.Monoid
import Data.String
import Data.Text.Lazy.Builder (Builder)

-- | A formatter.
type Format r x = Holey Builder r x

-- | The type of a monoid with holes. The underlying monoid is
-- represented by type parameter @m@. The @r@ is the result type and
-- stays polymorphic until the very last moment when 'run' is
-- called. The last argument @a@ is always a function with zero or
-- more arguments, finally resulting in @r@. Ordering the arguments in
-- this order allows holey monoids to be composed using `.`, stacking
-- the expected arguments. Note that the `Monoid` constraint is only
-- used in the identity 'Holey' and in composing two 'Holey's.
newtype HoleyT r a m = Holey { runHM :: (m -> r) -> a }

type Holey m r a = HoleyT r a m

instance Monoid (Format r (a -> r)) where
  mappend m n = Holey (\k a -> runHM m (\b1 -> runHM n (\b2 -> k (b1 <> b2)) a) a)
  mempty = Holey (\k a -> k mempty)

instance Functor (HoleyT r a) where
  fmap g m = Holey (\k -> runHM m (k . g))

-- | Very useful instance for writing format string.
instance (a ~ r) => IsString (Format r a) where
  fromString = now . fromString

-- | Composition operator. The same as category composition.
(%) :: Format b c -> Format r b -> Format r c
f % g = f `bind` \a -> g `bind` \b -> now (a `mappend` b)
infixr 9 %

-- | Function compose two holeys. Will feed the result of one holey
-- into another.
(%.) :: Format r (Builder -> b) -> Format b c -> Format r c
(%.) (Holey a) (Holey b) = Holey (b . a)
infixr 8 %.

-- | Insert a constant monoidal value.
now :: Builder -> Format r r
now a = Holey ($ a)

-- | Monadic indexed bind for holey monoids.
bind :: Format b c -> (Builder -> Format a b) -> Format a c
m `bind` f = Holey $ \k -> runHM m (\a -> runHM (f a) k)

-- | Insert a monoidal value that is not specified until the
-- computation is 'run'. The argument that is expected later is
-- converted to the monoid type using the given conversion function.
later :: (a -> Builder) -> Format r (a -> r)
later f = Holey (. f)
