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
type Format a = forall r. Holey Builder r (a -> r)

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

instance Functor (HoleyT r a) where
  fmap g m = Holey (\k -> runHM m (k . g))

-- | Very useful instance for writing format string.
instance (IsString m, a ~ r) => IsString (Holey m r a) where
  fromString = now . fromString

-- | Composition operator. The same as category composition.
(%) :: Monoid n => Holey n b c -> Holey n b1 b -> Holey n b1 c
f % g = f `bind` \a -> g `bind` \b -> now (a `mappend` b)

-- | Function compose two holeys. Will feed the result of one holey
-- into another.
(%.) :: Holey m r (a -> b) -> Holey a b c -> Holey m r c
(%.) (Holey a) (Holey b) = Holey (b . a)

-- | Insert a constant monoidal value.
now :: m -> Holey m r r
now a = Holey ($ a)

-- | Monadic indexed bind for holey monoids.
bind :: Holey m b c -> (m -> Holey n a b) -> Holey n a c
m `bind` f = Holey $ \k -> runHM m (\a -> runHM (f a) k)

-- | Insert a monoidal value that is not specified until the
-- computation is 'run'. The argument that is expected later is
-- converted to the monoid type using the given conversion function.
later :: (a -> m) -> Holey m r (a -> r)
later f = Holey (. f)
