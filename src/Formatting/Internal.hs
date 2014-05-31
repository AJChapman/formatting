-- | Internal format starters.

module Formatting.Internal where

import           Formatting.Holey

import           Data.Text.Lazy         (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO      as T
import           System.IO

-- | Run the formatter and return a "Text" value.
format :: Holey Builder Text a -> a
format m = runHM m T.toLazyText

-- | Run the formatter and return a "Builder" value.
bprint :: Holey Builder Builder a -> a
bprint m = runHM m id

-- | Run the formatter and print out the text to stdout.
fprint :: Holey Builder (IO ()) a -> a
fprint m = runHM m (T.putStr . T.toLazyText)

-- | Run the formatter and put the output onto the given "Handle".
hprint :: Handle -> Holey Builder (IO ()) a -> a
hprint h m = runHM m (T.hPutStr h . T.toLazyText)
