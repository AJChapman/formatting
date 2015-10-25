{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Internal format starters.

module Formatting.Internal where

import           Control.Category (Category(..))
import           Data.Monoid
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

-- | A formatter. The @r@ type means the returned value at the
-- end. The more formatters you compose, the more this will build up
-- arguments from @r@ to @Int -> r@ to @Char -> (Int -> r)@, etc.
newtype Format r a =
  Format {runFormat :: (Builder -> r) -> a}

-- | Useful instance for applying two formatters to the same input
-- argument. For example: @format (year <> "/" % month) now@ will
-- yield @"2015/01"@.
instance Monoid (Format r (a -> r)) where
  mappend m n =
    Format (\k a ->
              runFormat m (\b1 -> runFormat n (\b2 -> k (b1 <> b2)) a) a)
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

-- | Composition operator. 'Format' is an instance of 'Category', but
-- that is (at present) inconvenient to use with regular "Prelude". So
-- this function is provided as a convenience.
(%) :: Format r a -> Format r' r -> Format r' a
(%) = (.)
infixr 9 %

-- | Function compose two formatters. Will feed the result of one
-- formatter into another.
(%.) :: Format r (Builder -> r') -> Format r' a -> Format r a
(%.) (Format a) (Format b) = Format (b . a)
infixr 8 %.

-- | Insert a constant monoidal value.
now :: Builder -> Format r r
now a = Format ($ a)

-- | Monadic indexed bind for holey monoids.
bind :: Format r a -> (Builder -> Format r' r) -> Format r' a
m `bind` f = Format $ \k -> runFormat m (\a -> runFormat (f a) k)

-- | Insert a function which accepts some argument and produces a
-- 'Builder' which is appended to the output at the end.
--
-- @later (f :: Int -> Builder)@ produces @Format r (Int -> r)@.
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

-- | Run the formatter and print out the text to stdout.
fprint :: Format (IO ()) a -> a
fprint m = runFormat m (T.putStr . T.toLazyText)

-- | Run the formatter and put the output onto the given 'Handle'.
hprint :: Handle -> Format (IO ()) a -> a
hprint h m = runFormat m (T.hPutStr h . T.toLazyText)

-- | Run the formatter and return a list of characters.
formatToString :: Format [Char] a -> a
formatToString m = runFormat m (TL.unpack . TLB.toLazyText)
