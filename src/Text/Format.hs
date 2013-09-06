{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

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
-- @
-- formatPerson =
--   print (now \"Person's name is \" . ident . text . now \", age is \" . hex . now \"\n\")
--         \"Dave\"
--         \"Jones\"
--         35
-- @

module Text.Format
  ( Format
  , Build
  , Wrap
  -- * Holey generators
  , now
  , later
  , wrap
  , run
  , ident
  -- * Renderers
  , format
  , builder
  , print
  , hprint
  -- * General
  , build
  , text
  , stext
  , string
  -- * Integers
  , hex
  -- * Floats
  , expt
  , fixed
  , prec
  , shortest
  -- * Padding
  , left
  , right
  -- * Re-exports
  , Text
  , Builder
  , Buildable
  ) where

import           Data.Monoid
import qualified Data.Text as S (Text)
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as B
import qualified Data.Text.Format       as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO      as T
import           Prelude                hiding (print)
import           System.IO              hiding (print)

-- | Continuation based string formatter.
type Format m a b = forall r. ((m -> r) -> a) -> ((m -> r) -> b)

-- | Wrap the result with a function.
type Wrap r m a t = ((m -> a) -> t) -> ((r -> a) -> t)

-- | Builder formatting.
type Build a = forall r b. ((Builder -> r) -> b) -> (Builder -> r) -> a -> b

-- | Append something to the output stream now.
now :: Monoid m => m -> Format m a a
now m k d = k (\m' -> d (m <> m'))

-- | Append something to the output stream, taking the value as an
-- argument to a formatting function later.
later :: Monoid m => (a -> m) -> Format m b (a -> b)
later f k d a = k (\m -> d (f a <> m))

-- | Wrap the whole result in a function.
wrap :: (m -> r) -> Wrap r m a t
wrap f k d = k (\m' -> d (f m'))

-- | Identity on later.
ident :: Monoid m => ((m -> r) -> b) -> (m -> r) -> m -> b
ident = later id

-- | Run the formatter.
run :: Monoid f => Format f f a -> a
run fmt = fmt ($ mempty) id

-- | Run the formatter and at the end extract a lazy "Text" from the "Builder".
format :: Format Builder Text a -> a
format f = f ($ mempty) T.toLazyText

-- | Run the formatter producing a "Builder".
builder :: Format Builder Builder a -> a
builder f = f ($ mempty) (id :: Builder -> Builder)

-- | Run the formatter and print out the text to stdout.
print :: Format Builder (IO ()) a -> a
print f = f ($ mempty) (T.putStr . T.toLazyText)

-- | Run the formatter and put the output onto the given "Handle".
hprint :: Handle -> Format Builder (IO ()) a -> a
hprint h f = f ($ mempty) (T.hPutStr h . T.toLazyText)

-- | Include a buildable in the output stream.
build :: Buildable a => Build a
build = later B.build

-- | Output a lazy text.
text :: Build Text
text = later T.fromLazyText

-- | Output a strict text.
stext :: Build S.Text
stext = later T.fromText

-- | Output a string.
string :: Build String
string = later (T.fromLazyText . T.pack)

-- | Render an integer using hexadecimal notation. (No leading 0x is added.)
-- hex :: Build Integer
hex :: Build Integer
hex = later T.hex

-- | Render a floating point number using scientific/engineering
-- notation (e.g. 2.3e123), with the given number of decimal places.
expt :: Real a => Int -> Build a
expt i = later (T.expt i)

-- | Render a floating point number using normal notation, with the
-- given number of decimal places.
fixed :: Real a => Int -> Build a
fixed i = later (T.fixed i)

-- | Render a floating point number, with the given number of digits
-- of precision. Uses decimal notation for values between 0.1 and
-- 9,999,999, and scientific notation otherwise.
prec :: Real a => Int -> Build a
prec i = later (T.prec i)

-- | Render a floating point number using the smallest number of
-- digits that correctly represent it.
shortest :: Real a => Build a
shortest = later T.shortest

-- | Pad the left hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
left :: Buildable a => Int -> Char -> Build a
left i c = later (T.left i c)

-- | Pad the right hand side of a string until it reaches k characters
-- wide, if necessary filling with character c.
right :: Buildable a => Int -> Char -> Build a
right i c = later (T.right i c)
