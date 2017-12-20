{-# LANGUAGE OverloadedStrings #-}

-- | Examples that should always compile. If reading on Haddock, you
-- can view the sources to each of these.

module Formatting.Examples where

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Formatting

-- | Simple hello, world!
hello :: Text
hello = format ("Hello, World!")

-- | Printing strings.
strings :: Text
strings =
  format ("Here comes a string: " % string % " and another " % string)
         "Hello, World!"
         "Ahoy!"

-- | Printing texts.
texts :: Text
texts =
  format ("Here comes a string: " % text % " and another " % text)
         "Hello, World!"
         "Ahoy!"

-- | Printing builders.
builders :: Text
builders =
  format ("Here comes a string: " % builder % " and another " % text)
         ("Hello, World!" :: Builder)
         "Ahoy!"

-- | Printing integers.
integers :: Text
integers =
  format ("Here comes an integer: " % int % " and another: " % int)
         (23 :: Int)
         (0 :: Integer)

-- | Printing floating points.
floats :: Text
floats =
  format ("Here comes a float: " % float)
         (123.2342 :: Float)

-- | Printing integrals in hex (base-16).
hexes :: Text
hexes =
  format ("Here comes a hex: " % hex)
         (123 :: Int)

-- | Padding.
padding :: Text
padding =
  format ("A left-padded number: " % left 3 '0')
         (9 :: Int)
