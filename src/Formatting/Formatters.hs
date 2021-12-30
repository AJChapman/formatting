{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Formatting.Formatters
-- Copyright   : (c) 2013 Chris Done, 2013 Shachaf Ben-Kiki
-- License     : BSD3
-- Maintainer  : alex@farfromthere.net
-- Stability   : experimental
-- Portability : GHC
--
-- Formatting functions.

module Formatting.Formatters
  (
  -- * Text/string types
  text,
  stext,
  string,
  shown,
  char,
  builder,
  fconst,
  -- * Numbers
  int,
  float,
  fixed,
  sci,
  scifmt,
  shortest,
  groupInt,
  commas,
  ords,
  plural,
  asInt,
  -- * Padding
  left,
  right,
  center,
  fitLeft,
  fitRight,
  -- * Bases
  base,
  bin,
  oct,
  hex,
  prefixBin,
  prefixOct,
  prefixHex,
  bytes,
  -- * Buildables
  build,
  Buildable,
  ) where

import           Formatting.Internal

import           Data.Char (chr, ord)
import           Data.Scientific
import qualified Data.Text as S
import qualified Data.Text as T
import           Formatting.Buildable (Buildable)
import qualified Formatting.Buildable as B (build)
import qualified Data.Text.Format as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import           Data.Text.Lazy.Builder.Scientific
import           Numeric (showIntAtBase)

-- $setup
-- >>> import Formatting.Internal

-- | Output a lazy text.
text :: Format r (Text -> r)
text = later T.fromLazyText
{-# INLINE text #-}

-- | Output a strict text.
stext :: Format r (S.Text -> r)
stext = later T.fromText
{-# INLINE stext #-}

-- | Output a string.
string :: Format r (String -> r)
string = later (T.fromText . T.pack)
{-# INLINE string #-}

-- | Output a showable value (instance of 'Show') by turning it into
-- 'Text':
--
-- >>> format ("Value number " % shown % " is " % shown % ".") 42 False
-- "Value number 42 is False."
shown :: Show a => Format r (a -> r)
shown = later (T.fromText . T.pack . show)
{-# INLINE shown #-}

-- | Output a character.
char :: Format r (Char -> r)
char = later B.build
{-# INLINE char #-}

-- | Build a builder.
builder :: Format r (Builder -> r)
builder = later id
{-# INLINE builder #-}

-- | Like `const` but for formatters.
fconst :: Builder -> Format r (a -> r)
fconst m = later (const m)
{-# INLINE fconst #-}

-- | Build anything that implements the "Buildable" class.
build :: Buildable a => Format r (a -> r)
build = later B.build
{-# INLINE build #-}

-- | Render an integral e.g. 123 -> \"123\", 0 -> \"0\".
int :: Integral a => Format r (a -> r)
int = base 10
{-# INLINE int #-}

-- | Render some floating point with the usual notation, e.g. 123.32 => \"123.32\"
float :: Real a => Format r (a -> r)
float = later T.shortest
{-# INLINE float #-}

-- | Render a floating point number using normal notation, with the
-- given number of decimal places.
fixed :: Real a => Int -> Format r (a -> r)
fixed i = later (T.fixed i)
{-# INLINE fixed #-}

-- | Render a floating point number using the smallest number of
-- digits that correctly represent it. Note that in the case of whole
-- numbers it will still add one decimal place, e.g. "1.0".
shortest :: Real a => Format r (a -> r)
shortest = later T.shortest
{-# INLINE shortest #-}

-- | Render a scientific number.
sci :: Format r (Scientific -> r)
sci = later scientificBuilder
{-# INLINE sci #-}

-- | Render a scientific number with options.
scifmt :: FPFormat -> Maybe Int -> Format r (Scientific -> r)
scifmt f i = later (formatScientificBuilder f i)
{-# INLINE scifmt #-}

-- | Shows the Int value of Enum instances using 'fromEnum'.
--
-- >>> format ("Got: " % char % " (" % asInt % ")") 'a' 'a'
-- "Got: a (97)"
asInt :: Enum a => Format r (a -> r)
asInt = later (T.shortest . fromEnum)
{-# INLINE asInt #-}

-- | Pad the left hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
left :: Buildable a => Int -> Char -> Format r (a -> r)
left i c = later (T.left i c)
{-# INLINE left #-}

-- | Pad the right hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
right :: Buildable a => Int -> Char -> Format r (a -> r)
right i c = later (T.right i c)
{-# INLINE right #-}

-- | Pad the left & right hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
center :: Buildable a => Int -> Char -> Format r (a -> r)
center i c = later centerT where
  centerT = T.fromLazyText . LT.center (fromIntegral i) c . T.toLazyText . B.build

-- | Group integral numbers, e.g. groupInt 2 '.' on 123456 -> \"12.34.56\".
groupInt :: (Buildable n,Integral n) => Int -> Char -> Format r (n -> r)
groupInt 0 _ = later B.build
groupInt i c =
  later
    (\n ->
       if n < 0
         then "-" <> commaize (negate n)
         else commaize n)
  where
    commaize =
      T.fromLazyText .
      LT.reverse .
      foldr merge "" .
      LT.zip (zeros <> cycle' zeros') . LT.reverse . T.toLazyText . B.build
    zeros = LT.replicate (fromIntegral i) (LT.singleton '0')
    zeros' = LT.singleton c <> LT.tail zeros
    merge (f, c') rest
      | f == c = LT.singleton c <> LT.singleton c' <> rest
      | otherwise = LT.singleton c' <> rest
    cycle' xs = xs <> cycle' xs

-- | Fit in the given length, truncating on the left.
fitLeft :: Buildable a => Int -> Format r (a -> r)
fitLeft size = later (fit (fromIntegral size)) where
  fit i = T.fromLazyText . LT.take i . T.toLazyText . B.build

-- | Fit in the given length, truncating on the right.
fitRight :: Buildable a => Int -> Format r (a -> r)
fitRight size = later (fit (fromIntegral size)) where
  fit i = T.fromLazyText .
          (\t -> LT.drop (LT.length t - i) t)
          . T.toLazyText
          . B.build

-- | Add commas to an integral, e.g 12000 -> \ "12,000".
commas :: (Buildable n,Integral n) => Format r (n -> r)
commas = groupInt 3 ','
{-# INLINE commas #-}

-- | Add a suffix to an integral, e.g. 1st, 2nd, 3rd, 21st.
ords :: Integral n => Format r (n -> r)
ords = later go
  where go n
          | tens > 3 && tens < 21 = T.fixed 0 n <> "th"
          | otherwise =
            T.fixed 0 n <>
            case n `mod` 10 of
              1 -> "st"
              2 -> "nd"
              3 -> "rd"
              _ -> "th"
          where tens = n `mod` 100

-- | English plural suffix for an integral.
--
-- For example:
--
-- >>> :set -XOverloadedStrings
-- >>> formatPeople = format (int % " " <> plural "person" "people" % ".") :: Int -> Data.Text.Lazy.Text
-- >>> formatPeople 1
-- "1 person."
-- >>> formatPeople 3
-- "3 people."
plural :: (Num a, Eq a) => Text -> Text -> Format r (a -> r)
plural s p = later (\i -> if i == 1 then B.build s else B.build p)

-- | Render an integral at base n.
base :: Integral a => Int -> Format r (a -> r)
base numBase = later (B.build . atBase numBase)
{-# INLINE base #-}

-- | Render an integer using binary notation. (No leading 0b is
-- added.) Defined as @bin = 'base' 2@.
bin :: Integral a => Format r (a -> r)
bin = base 2
{-# INLINE bin #-}

-- | Render an integer using octal notation. (No leading 0o is
-- added.) Defined as @oct = 'base' 8@.
oct :: Integral a => Format r (a -> r)
oct = base 8
{-# INLINE oct #-}

-- | Render an integer using hexadecimal notation. (No leading 0x is
-- added.) Has a specialized implementation.
hex :: Integral a => Format r (a -> r)
hex = later T.hex
{-# INLINE hex #-}

-- | Render an integer using binary notation with a leading 0b.
--
-- See also 'Formatting.Combinators.binPrefix' for fixed-width formatting.
prefixBin :: Integral a => Format r (a -> r)
prefixBin = "0b" % bin
{-# INLINE prefixBin #-}

-- | Render an integer using octal notation with a leading 0o.
--
-- See also 'Formatting.Combinators.octPrefix' for fixed-width formatting.
prefixOct :: Integral a => Format r (a -> r)
prefixOct = "0o" % oct
{-# INLINE prefixOct #-}

-- | Render an integer using hexadecimal notation with a leading 0x.
--
-- See also 'Formatting.Combinators.hexPrefix' for fixed-width formatting.
prefixHex :: Integral a => Format r (a -> r)
prefixHex = "0x" % hex
{-# INLINE prefixHex #-}

-- The following code is mostly taken from `Numeric.Lens.' (from
-- `lens') and modified.

-- | Internal function that converts a number to a base base-2 through
-- base-36.
atBase :: Integral a => Int -> a -> String
atBase b _ | b < 2 || b > 36 = error ("base: Invalid base " ++ show b)
atBase b n =
  showSigned' (showIntAtBase (toInteger b) intToDigit') (toInteger n) ""
{-# INLINE atBase #-}

-- | A simpler variant of 'Numeric.showSigned' that only prepends a dash and
-- doesn't know about parentheses
showSigned' :: Real a => (a -> ShowS) -> a -> ShowS
showSigned' f n
  | n < 0     = showChar '-' . f (negate n)
  | otherwise = f n

-- | Like 'Data.Char.intToDigit', but handles up to base-36
intToDigit' :: Int -> Char
intToDigit' i
  | i >= 0  && i < 10 = chr (ord '0' + i)
  | i >= 10 && i < 36 = chr (ord 'a' + i - 10)
  | otherwise = error ("intToDigit': Invalid int " ++ show i)

-- | Renders a given byte count using an appropiate decimal binary suffix:
--
-- >>> format (bytes shortest) 1024
-- "1KB"
--
-- >>> format (bytes (fixed 2 % " ")) (1024*1024*5)
-- "5.00 MB"
--
bytes :: (Ord f,Integral a,Fractional f)
      => Format Builder (f -> Builder) -- ^ formatter for the decimal part
      -> Format r (a -> r)
bytes d = later go
  where go bs =
          bprint d (fromIntegral (signum bs) * dec) <> bytesSuffixes !!
          i
          where (dec,i) = getSuffix (abs bs)
        getSuffix n =
          until p
                (\(x,y) -> (x / 1024,y + 1))
                (fromIntegral n,0)
          where p (n',numDivs) =
                  n' < 1024 || numDivs == (length bytesSuffixes - 1)
        bytesSuffixes =
          ["B","KB","MB","GB","TB","PB","EB","ZB","YB"]
