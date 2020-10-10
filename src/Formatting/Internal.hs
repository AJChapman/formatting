{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- | Internal format starters.

module Formatting.Internal
  ( Format(..)
  , (%)
  , (%.)
  , now
  , bind
  , mapf
  , later
  , format
  , sformat
  , bprint
  , bformat
  , fprint
  , fprintLn
  , hprint
  , hprintLn
  , formatToString
  ) where

import           Control.Category (Category(..))
import           Data.Monoid
import qualified Data.Semigroup
import           Data.String
import qualified Data.Text as S (Text)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as T
import           Prelude hiding ((.),id)
import           System.IO

-- | A formatter. When you construct formatters the first type
-- parameter, @r@, will remain polymorphic.  The second type
-- parameter, @a@, will change to reflect the types of the data that
-- will be formatted.  For example, in
--
-- @
-- myFormat :: Format r (Text -> Int -> r)
-- myFormat = \"Person's name is \" % text % \", age is \" % hex
-- @
--
-- the first type parameter remains polymorphic, and the second type
-- parameter is @Text -> Int -> r@, which indicates that it formats a
-- 'Text' and an 'Int'.
--
-- When you run the 'Format', for example with 'format', you provide
-- the arguments and they will be formatted into a string.
--
-- @
-- \> format (\"Person's name is \" % text % \", age is \" % hex) \"Dave\" 54
-- \"Person's name is Dave, age is 36\"
-- @
newtype Format r a =
  Format {runFormat :: (Builder -> r) -> a}

-- | This can be used almost like contramap, e.g:
--
-- @
-- formatter :: Format r (b -> r)
-- formatter = _
-- 
-- adapter :: a -> b
-- adapter = _
-- 
-- adapted :: Format r (a -> r)
-- adapted = fmap (. adapter) formatter
-- @
instance Functor (Format r) where
  fmap f (Format k) = Format (f . k)

instance Data.Semigroup.Semigroup (Format r (a -> r)) where
  m <> n =
    Format (\k a ->
              runFormat m (\b1 -> runFormat n (\b2 -> k (b1 <> b2)) a) a)

-- | Useful instance for applying two formatters to the same input
-- argument. For example: @format (year <> "/" % month) now@ will
-- yield @"2015/01"@.
instance
#if !(MIN_VERSION_base(4,11,0))
  Data.Semigroup.Semigroup (Format r (a -> r)) =>
#endif
  Monoid (Format r (a -> r)) where
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif
    mempty = Format (\k _ -> k mempty)

-- | Useful instance for writing format string. With this you can
-- write @"Foo"@ instead of @now "Foo!"@.
instance (a ~ r) => IsString (Format r a) where
  fromString = now . fromString

-- | The same as (%). At present using 'Category' has an import
-- overhead, but one day it might be imported as standard.
instance Category Format where
  id = now mempty
  f . g =
    f `bind`
    \a ->
      g `bind`
      \b -> now (a `mappend` b)

-- | Concatenate two formatters.
--
-- @formatter1 % formatter2@ is a formatter that accepts arguments for
-- @formatter1@ and @formatter2@ and concatenates their results.  For example
--
-- @
-- format1 :: Format r (Text -> r)
-- format1 = \"Person's name is \" % text
-- @
--
-- @
-- format2 :: Format r r
-- format2 = \", \"
-- @
--
-- @
-- format3 :: Format r (Int -> r)
-- format3 = \"age is \" % hex
-- @
--
-- @
-- myFormat :: Format r (Text -> Int -> r)
-- myFormat = format1 % format2 % format3
-- @
--
-- Notice how the argument types of @format1@ and @format3@ are
-- gathered into the type of @myFormat@.
--
-- (This is actually the composition operator for 'Format's
-- 'Category' instance, but that is (at present) inconvenient to use
-- with regular "Prelude". So this function is provided as a
-- convenience.)
(%) :: Format r a -> Format r' r -> Format r' a
(%) = (.)
infixr 9 %

-- | Function compose two formatters. Will feed the result of one
-- formatter into another.
(%.) :: Format r (Builder -> r') -> Format r' a -> Format r a
(%.) (Format a) (Format b) = Format (b . a)
infixr 8 %.

-- | Don't format any data, just output a constant 'Builder'.
now :: Builder -> Format r r
now a = Format ($ a)

-- | Monadic indexed bind for holey monoids.
bind :: Format r a -> (Builder -> Format r' r) -> Format r' a
m `bind` f = Format $ \k -> runFormat m (\a -> runFormat (f a) k)

-- | Functorial map over a formatter's input. Example: @format (mapf (drop 1) string) \"hello\"@
mapf :: (a -> b) -> Format r (b -> t) -> Format r (a -> t)
mapf f m = Format (\k -> runFormat m k . f)

-- | Format a value of type @a@ using a function of type @a ->
-- 'Builder'@. For example, @later (f :: Int -> Builder)@ produces
-- @Format r (Int -> r)@.
later :: (a -> Builder) -> Format r (a -> r)
later f = Format (. f)

-- | Run the formatter and return a lazy 'Text' value.
format :: Format Text a -> a
format m = runFormat m T.toLazyText

-- | Run the formatter and return a strict 'S.Text' value.
sformat :: Format S.Text a -> a
sformat m = runFormat m (T.toStrict . T.toLazyText)

-- | Run the formatter and return a 'Builder' value.
bprint :: Format Builder a -> a
bprint m = runFormat m id

-- | Run the formatter and return a 'Builder' value.
-- 
-- This is a newer synonym for 'bprint', following the naming convention set by 'format' and 'sformat'.
bformat :: Format Builder a -> a
bformat m = runFormat m id

-- | Run the formatter and print out the text to stdout.
fprint :: Format (IO ()) a -> a
fprint m = runFormat m (T.putStr . T.toLazyText)

-- | Run the formatter and print out the text to stdout, followed by a newline.
fprintLn :: Format (IO ()) a -> a
fprintLn m = runFormat m (T.putStrLn . T.toLazyText)

-- | Run the formatter and put the output onto the given 'Handle'.
hprint :: Handle -> Format (IO ()) a -> a
hprint h m = runFormat m (T.hPutStr h . T.toLazyText)

-- | Run the formatter and put the output and a newline onto the given 'Handle'.
hprintLn :: Handle -> Format (IO ()) a -> a
hprintLn h m = runFormat m (T.hPutStrLn h . T.toLazyText)

-- | Run the formatter and return a list of characters.
formatToString :: Format String a -> a
formatToString m = runFormat m (TL.unpack . TLB.toLazyText)
