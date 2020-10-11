{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples, ScopedTypeVariables #-}

-- Module:      Data.Text.Format.Int
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize an integral value to a 'Builder'.

module Data.Text.Format.Int
    (
      decimal
    , integer
    , hexadecimal
    , minus
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text.Format.Functions (i2d)
import qualified Data.Text.Format.Functions as F ((<>))
import Data.Text.Lazy.Builder
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Base (quotInt, remInt)
import GHC.Types (Int(..))

#ifdef  __GLASGOW_HASKELL__
# if __GLASGOW_HASKELL__ < 611
import GHC.Integer.Internals
# else
import GHC.Integer.GMP.Internals
# endif

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mempty)
import Data.Word (Word)
import GHC.Num (quotRemInteger)
#endif
#endif

#ifdef INTEGER_GMP
# define PAIR(a,b) (# a,b #)
#else
# define PAIR(a,b) (a,b)
#endif

decimal :: forall a. (Integral a, Bounded a) => a -> Builder
{-# SPECIALIZE decimal :: Int -> Builder #-}
{-# SPECIALIZE decimal :: Int8 -> Builder #-}
{-# SPECIALIZE decimal :: Int16 -> Builder #-}
{-# SPECIALIZE decimal :: Int32 -> Builder #-}
{-# SPECIALIZE decimal :: Int64 -> Builder #-}
{-# SPECIALIZE decimal :: Word -> Builder #-}
{-# SPECIALIZE decimal :: Word8 -> Builder #-}
{-# SPECIALIZE decimal :: Word16 -> Builder #-}
{-# SPECIALIZE decimal :: Word32 -> Builder #-}
{-# SPECIALIZE decimal :: Word64 -> Builder #-}
{-# RULES "decimal/Integer" decimal = integer 10 :: Integer -> Builder #-}
decimal i
    | (minBound :: a) < 0 && i == minBound =
        -- special case, since (-i) would not be representable assuming two's
        -- compliment:
        minus F.<> integer 10 (negate $ fromIntegral i)
    | i < 0     = minus F.<> go (-i)
    | otherwise = go i
  where
    go n | n < 10    = digit n
         | otherwise = go (n `quot` 10) F.<> digit (n `rem` 10)
{-# NOINLINE[0] decimal #-}

hexadecimal :: Integral a => a -> Builder
{-# SPECIALIZE hexadecimal :: Int -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int8 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int16 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int32 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int64 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word8 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word16 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word32 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word64 -> Builder #-}
{-# RULES "hexadecimal/Integer" hexadecimal = integer 16 :: Integer -> Builder #-}
hexadecimal i
    | i < 0     = minus F.<> go (-i)
    | otherwise = go i
  where
    go n | n < 16    = hexDigit n
         | otherwise = go (n `quot` 16) F.<> hexDigit (n `rem` 16)
{-# NOINLINE[0] hexadecimal #-}

digit :: Integral a => a -> Builder
digit n = singleton $! i2d (fromIntegral n)
{-# INLINE digit #-}

hexDigit :: Integral a => a -> Builder
hexDigit n
    | n <= 9    = singleton $! i2d (fromIntegral n)
    | otherwise = singleton $! toEnum (fromIntegral n + 87)
{-# INLINE hexDigit #-}

minus :: Builder
minus = singleton '-'

int :: Int -> Builder
int = decimal
{-# INLINE int #-}

data T = T !Integer !Int

integer :: Int -> Integer -> Builder
integer 10 (S# i#) = decimal (I# i#)
integer 16 (S# i#) = hexadecimal (I# i#)
integer base i
    | i < 0     = minus F.<> go (-i)
    | otherwise = go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf p n
      | p > n       = [n]
      | otherwise   = splith p (splitf (p*p) n)

    splith p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) | q > 0     -> q : r : splitb p ns
                                  | otherwise -> r : splitb p ns
    splith _ _      = error "splith: the impossible happened."

    splitb p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) -> q : r : splitb p ns
    splitb _ _      = []

    T maxInt10 maxDigits10 =
        until ((>mi) . (*10) . fstT) (\(T n d) -> T (n*10) (d+1)) (T 10 1)
      where mi = fromIntegral (maxBound :: Int)
    T maxInt16 maxDigits16 =
        until ((>mi) . (*16) . fstT) (\(T n d) -> T (n*16) (d+1)) (T 16 1)
      where mi = fromIntegral (maxBound :: Int)

    fstT (T a _) = a

    maxInt | base == 10 = maxInt10
           | otherwise  = maxInt16
    maxDigits | base == 10 = maxDigits10
              | otherwise  = maxDigits16

    putH (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y)
                        | q > 0     -> int q F.<> pblock r F.<> putB ns
                        | otherwise -> int r F.<> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putH _ = error "putH: the impossible happened"

    putB (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y) -> pblock q F.<> pblock r F.<> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putB _ = mempty

    pblock = loop maxDigits
      where
        loop !d !n
            | d == 1    = digit n
            | otherwise = loop (d-1) q F.<> digit r
            where q = n `quotInt` base
                  r = n `remInt` base
