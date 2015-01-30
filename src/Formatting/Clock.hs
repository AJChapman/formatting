{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Formatters for high-res, real-time and timer clock values from "System.Clock".

module Formatting.Clock where

import Formatting
import Formatting.Internal
import System.Clock

-- | Format the duration from start to end (args passed in that order).
--
-- Examples:
--
-- @
-- 4.00 s
-- 500.69 ms
-- 1.20 ms
-- 19.38 µs
-- @
timeSpecs :: Format r (TimeSpec -> TimeSpec -> r)
timeSpecs = Format (\g x y -> g (fmt x y))
  where fmt (TimeSpec s1 n1) (TimeSpec s2 n2)
          | Just i <- scale ((10 ^ 9) * 60 * 60 * 24) = bprint (fixed 2 % " d") i
          | Just i <- scale ((10 ^ 9) * 60 * 60) = bprint (fixed 2 % " h") i
          | Just i <- scale ((10 ^ 9) * 60) = bprint (fixed 2 % " m") i
          | Just i <- scale (10 ^ 9) = bprint (fixed 2 % " s") i
          | Just i <- scale (10 ^ 6) = bprint (fixed 2 % " ms") i
          | Just i <- scale (10 ^ 3) = bprint (fixed 2 % " µs") i
          | otherwise = bprint (int % " ns") diff
          where scale :: Integer -> Maybe Double
                scale i = if diff >= i
                             then Just (fromIntegral diff / fromIntegral i)
                             else Nothing
                diff :: Integer
                diff = a2 - a1
                a1 = (fromIntegral s1 * 10 ^ 9) + fromIntegral n1
                a2 = (fromIntegral s2 * 10 ^ 9) + fromIntegral n2
