{-# LANGUAGE FlexibleInstances #-}

module Formatting.FromBuilder
  ( FromBuilder(..)
  , formatted
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TL
import Formatting.Internal (Format (..))

-- $setup
-- >>> import qualified Data.Text.Lazy as TL (Text)
-- >>> import qualified Data.Text.Lazy.IO as TL
-- >>> import qualified Data.Text as T (Text)
-- >>> import qualified Data.Text.IO as T
-- >>> import Formatting ((%))
-- >>> import Formatting.Formatters (int)
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

-- | Anything that can be created from a 'Builder'.
--   This class makes it easier to add formatting to other API's.
--   See 'formatted' for some examples of this class in action.
class FromBuilder a where
  fromBuilder :: Builder -> a

instance FromBuilder Builder where
  fromBuilder = id
  {-# INLINE fromBuilder #-}

instance FromBuilder TL.Text where
  fromBuilder = TL.toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder T.Text where
  fromBuilder = TL.toStrict . TL.toLazyText
  {-# INLINE fromBuilder #-}

instance FromBuilder [Char] where
  fromBuilder = TL.unpack . TL.toLazyText
  {-# INLINE fromBuilder #-}

-- | Makes it easy to add formatting to any api that is expecting a builder,
--   a strict or lazy text, or a string.
--   It is essentially (flip runFormat), but with a more generous type due to
--   the typeclass.
--
--   For example:
--   >>> formatted TL.putStr ("x is: " % int % "\n") 7
--   x is: 7
--   >>> formatted T.putStr ("x is: " % int % "\n") 7
--   x is: 7
--   >>> formatted (id @TL.Text) ("x is: " % int % "\n") 7
--   "x is: 7\n"
--   >>> formatted (id @T.Text) ("x is: " % int % "\n") 7
--   "x is: 7\n"
formatted :: FromBuilder t => (t -> o) -> Format o a -> a
formatted k f = runFormat f (k . fromBuilder)
